package API;

import java.io.*;
import java.net.*;
import java.nio.ByteBuffer;
import spullara.nio.channels.FutureSocketChannel;

import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

public class API {

    Map<Integer,FutureSocketChannel> lfs;
    FutureSocketChannel coordinator;

    public API() throws IOException, ExecutionException, InterruptedException {
        lfs = new HashMap<>();
        for(int i=12340; i<12349; i++)
            lfs.put(i, getSocket(i));
        coordinator = new FutureSocketChannel();
        CompletableFuture<Void> cf = coordinator.connect(new InetSocketAddress("localhost",12399));
        cf.get();
    }

    private FutureSocketChannel getSocket(int port) throws IOException {
        FutureSocketChannel fs = new FutureSocketChannel();
        fs.connect(new InetSocketAddress("localhost", port));

        return fs;
    }

    private int HashFunction(Long key) {
        return (int) (12340+(key%lfs.size()));
    }

    private int getClock() throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> acceptor = new CompletableFuture<Integer>();
        ByteBuffer buff = ByteBuffer.allocate(100);
        buff.put("getClock".getBytes(StandardCharsets.UTF_8));
        this.coordinator.write(buff.flip()).thenRun(() -> {
           ByteBuffer buffClock = ByteBuffer.allocate(4);
           this.coordinator.read(buffClock).thenRun(() -> {
               acceptor.complete(buffClock.flip().getInt());
           });
        });
        return acceptor.get();
    }

    public CompletableFuture<Void> put(Map<Long, byte[]> values) throws ExecutionException, InterruptedException {
        Integer clock = getClock();
        // Create a list to contain messages
        Map<Integer, Map<Long,byte[]>> peerMap = new HashMap<>();

        // Put the messages in the map distributed by peer
        values.forEach((key, value) -> {
            int peer = HashFunction(key);
            if (!peerMap.containsKey(peer)) {
                peerMap.put(peer, new HashMap<>());
            }
            peerMap.get(peer).put(key, value);
        });

        // Send each put message
        List<CompletableFuture<Void>> cf = peerMap.entrySet().stream().map(e -> putOne(e.getKey(), e.getValue(), clock)).collect(Collectors.toList());

        // Create an acceptor that finishes as soon as every write operation does.
        CompletableFuture<Void> acceptor = new CompletableFuture<>();
        CompletableFuture.allOf(cf.toArray(CompletableFuture[]::new)).thenRun(() -> acceptor.complete(null));
        return acceptor;
    }


    public CompletableFuture<Void> putOne(Integer key, Map<Long,byte[]> value, Integer clock) {
        CompletableFuture<Void> acceptor = new CompletableFuture<>();

        GenericMessage<Map<Long,byte[]>> getMsg = new GenericMessage<>("put", value, clock);

        lfs.get(key).write(getMsg.serialize()).thenRun(() -> acceptor.complete(null));
        return acceptor;
    }

    public CompletableFuture<Map<Long, byte[]>> get(Collection<Long> keys) throws ExecutionException, InterruptedException {
        Integer clock = getClock();

        Map<Long, byte[]> map = new HashMap<>();

        // Create a list to contain messages
        Map<Integer, List<Long>> peerMap = new HashMap<>();

        // Put the messages in the a map distributed by peer
        keys.forEach(k -> {
            int peer = HashFunction(k);
            if (!peerMap.containsKey(peer)) {
                peerMap.put(peer, new ArrayList<>());
            }
            peerMap.get(peer).add(k);
        });

        // Send each get message and asynchronously add elements to the final map.
        List<CompletableFuture<Void>> cf = peerMap.entrySet().stream().map(e -> getOne(e.getKey(), e.getValue(), map, clock)).collect(Collectors.toList());

        // Create an acceptor that finishes as soon as every write operation on the map is done.
        CompletableFuture<Map<Long, byte[]>> acceptor = new CompletableFuture<>();
        CompletableFuture.allOf(cf.toArray(CompletableFuture[]::new)).thenRun( () -> acceptor.complete(map));
        return acceptor;
    }

    public CompletableFuture<Void> getOne(Integer peer, List<Long> value, Map<Long, byte[]> map, Integer clock) {
        CompletableFuture<Void> acceptor = new CompletableFuture<>();

        GenericMessage<List<Long>> getMsg = new GenericMessage<>("get", value, clock);

        // First write the request we wanna do to the correct peer.
        lfs.get(peer).write(getMsg.serialize()).thenRun(() -> {
            // Then read the size of the message that's coming too.
            ByteBuffer sizeBuff = ByteBuffer.allocate(4);
            lfs.get(peer).read(sizeBuff).thenRun(() -> {
                // Finally read the message with the provided size and process it, completing the acceptor afterwards.
                ByteBuffer buff = ByteBuffer.allocate(sizeBuff.flip().getInt());
                lfs.get(peer).read(buff).thenRun(() -> {
                    buff.flip();
                    GenericMessage<Map<Long, byte[]>> resp = new GenericMessage<Map<Long, byte[]>>(buff);

                    Map<Long, byte[]> mapResp = resp.getValue();
                    mapResp.forEach(map::put);
                    acceptor.complete(null);

                });
            });
        });
        return acceptor;
    }
}


