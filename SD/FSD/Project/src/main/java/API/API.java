package API;

import java.io.*;
import java.net.*;
import java.nio.ByteBuffer;

import Protocol.*;
import spullara.nio.channels.FutureSocketChannel;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import static Protocol.GenericMessage.TYPE.*;

public class API {

    private final Map<Integer,FutureSocketChannel> lfs;
    private final FutureSocketChannel coordinator;
    private final ReentrantLock lock;

    private static final int LOWER_BOUND = 12340;
    private static final int UPPER_BOUND = 12349;
    private static final int COORD_PORT = 12399;

    public API() throws IOException, ExecutionException, InterruptedException {
        lock = new ReentrantLock();
        lfs = new HashMap<>();
        for(int i = LOWER_BOUND; i < UPPER_BOUND; i++)
            lfs.put(i, getSocket(i));
        coordinator = new FutureSocketChannel();
        CompletableFuture<Void> cf = coordinator.connect(new InetSocketAddress("localhost",COORD_PORT));
        cf.get();
    }

    private FutureSocketChannel getSocket(int port) throws IOException {
        FutureSocketChannel fs = new FutureSocketChannel();
        fs.connect(new InetSocketAddress("localhost", port));

        return fs;
    }

    private int HashFunction(Long key) {
        return (int) (LOWER_BOUND+(key%lfs.size()));
    }

    private int getClock(Map<Long, byte[]> map) throws ExecutionException, InterruptedException {
        CompletableFuture<Integer> acceptor = new CompletableFuture<>();
        GenericMessage<Map<Long, byte[]>> m = new GenericMessage<>(GET_CLOCK,map);
        Protocol.write(coordinator, m).thenRun(() -> {
            ByteBuffer buffClock = ByteBuffer.allocate(4);
            coordinator.read(buffClock).thenRun(() -> acceptor.complete(buffClock.flip().getInt()));
        });
        return acceptor.get();
    }

    public CompletableFuture<Void> put(Map<Long, byte[]> values) throws ExecutionException, InterruptedException {
        Integer clock = getClock(values);

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

        GenericMessage<Map<Long,byte[]>> msg = new GenericMessage<>(PUT, value, clock);

        Protocol.write(lfs.get(key), msg).thenRun(() -> acceptor.complete(null));
        return acceptor;
    }

    public CompletableFuture<Map<Long, byte[]>> get(Collection<Long> keys) {
        CompletableFuture<Map<Long, byte[]>> acceptor = new CompletableFuture<>();
        List<Long> keyList = new ArrayList<>(keys);

        getFromCoordinator(keyList).thenAccept(ss -> {
            Map<Long, Integer> m = ss.getVersions(keyList);

            Map<Long, byte[]> map = new HashMap<>();

            // Create a list to contain messages
            Map<Integer, Map<Long, Integer>> peerMap = new HashMap<>();

            // Put the messages in the map distributed by peer
            keyList.forEach(k -> {
                int peer = HashFunction(k);
                if (!peerMap.containsKey(peer)) {
                    peerMap.put(peer, new HashMap<>());
                }
                peerMap.get(peer).put(k, m.get(k));
            });

            // Send each get message and asynchronously add elements to the final map.
            List<CompletableFuture<Void>> cf = peerMap.entrySet().stream().map(e -> getOne(e.getKey(), e.getValue(), map)).collect(Collectors.toList());

            // Create an acceptor that finishes as soon as every write operation on the map is done.
            CompletableFuture.allOf(cf.toArray(CompletableFuture[]::new)).thenRun(() -> acceptor.complete(map));
        });

        return acceptor;
    }

    public CompletableFuture<Void> getOne(Integer peer, Map<Long, Integer> value, Map<Long, byte[]> map) {
        CompletableFuture<Void> acceptor = new CompletableFuture<>();

        GenericMessage<Map<Long,Integer>> msg = new GenericMessage<>(GET, value);

        // First write the request we wanna do to the correct peer.
        Protocol.write(lfs.get(peer), msg).thenRun(() -> {
            Protocol.read(lfs.get(peer)).thenAccept(resp -> {
                Map<Long, byte[]> mapResp = (Map<Long, byte[]>) resp.getValue();
                lock.lock();
                mapResp.forEach(map::put);
                lock.unlock();
                acceptor.complete(null);
            });
        });
        return acceptor;
    }

    private CompletableFuture<Snapshot> getFromCoordinator(List<Long> keys){
        CompletableFuture<Snapshot> acceptor = new CompletableFuture<>();
        GenericMessage<List<Long>> m = new GenericMessage<>(GET_SS, keys);
        Protocol.write(coordinator, m).thenRun(() -> {
            Protocol.read(coordinator).thenAccept(gm -> acceptor.complete((Snapshot) gm.getValue()));
        });
        return acceptor;
    }
}

