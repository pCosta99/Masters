package API;

import Protocol.*;
import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import static Protocol.GenericMessage.TYPE.*;

public class Coordinator implements Runnable {

    private Integer clock;
    private final ReentrantLock lock;
    private final Integer port;
    private final Map<Long, List<Integer>> keyVersions; // Map<Key,List<Version>>
    private final Map<Integer, Map<Integer, Boolean>> completedVersions; // Map<Peer, Map<Version, boolean>>
    private final ReentrantLock cVlock;
    private final ReentrantLock kVlock;

    public Coordinator(Integer port, List<Integer> peers) {
        this.clock = 0;
        this.lock = new ReentrantLock();
        this.port = port;
        keyVersions = new ConcurrentHashMap<>();
        completedVersions = new ConcurrentHashMap<>();
        for(Integer peer : peers) completedVersions.put(peer, new ConcurrentHashMap<>());
        cVlock = new ReentrantLock();
        kVlock = new ReentrantLock();
    }

    private int HashFunction(Long key) {
        return (int) (12340 + (key % completedVersions.size()));
    }

    private void putHandler(FutureSocketChannel s, GenericMessage m){
        ByteBuffer clockBuff = ByteBuffer.allocate(4);
        lock.lock();
        clockBuff.putInt(clock);
        int versionID = clock++;
        lock.unlock();
        s.write(clockBuff.flip()).thenRun(clockBuff::clear).thenRun(() -> {
            Set<Long> keys = ((Map<Long, byte[]>) m.getValue()).keySet();
            // Peers that will participate in this put operation
            Set<Integer> peers = keys.stream().map(this::HashFunction).collect(Collectors.toSet());
            // Add this versionID to the key if it exists
            kVlock.lock();
            cVlock.lock();
            keys.forEach(k -> {
                if (!keyVersions.containsKey(k)) keyVersions.put(k, new ArrayList<>());
                keyVersions.get(k).add(versionID);
            });
            // Add this versionID to all peers
            // Set it as true if peer doesn't participate in the operation
            completedVersions.forEach((peer,map) -> completedVersions.get(peer).put(versionID, !peers.contains(peer)));
            cVlock.unlock();
            kVlock.unlock();
        });
    }

    public void requestHandler(FutureSocketChannel s) {
        Protocol.read(s).thenAccept(m -> {
            if(m.getType().equals(GET_CLOCK)) {
                putHandler(s, m);
                requestHandler(s);
            } else if (m.getType().equals(GET_SS)) {
                cVlock.lock();
                kVlock.lock();
                Snapshot ss = new Snapshot(keyVersions, completedVersions);
                GenericMessage<Snapshot> message = new GenericMessage<>(REPLY, ss);
                Protocol.write(s, message).thenRun(() -> requestHandler(s));
                kVlock.unlock();
                cVlock.unlock();
            } else {
                // Deal with server versionID completed message
                Integer versionID = m.getClock();
                Integer peer = (Integer) m.getValue();
                cVlock.lock();
                completedVersions.get(peer).put(versionID, true);
                cVlock.unlock();
                requestHandler(s);
            }
        });
    }

    public void acceptConnection(FutureServerSocketChannel fssc) {
        CompletableFuture<FutureSocketChannel> sc = fssc.accept();

        sc.thenAccept(s -> {
            //System.out.println("New client connected to coordinator at " + this.port);
            requestHandler(s);
            acceptConnection(fssc);
        });
    }

    public void run() {
        FutureServerSocketChannel fssc;
        try {
            fssc = new FutureServerSocketChannel();
            fssc.bind(new InetSocketAddress(this.port));

            acceptConnection(fssc);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}