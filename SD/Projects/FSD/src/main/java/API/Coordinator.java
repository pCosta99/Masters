package API;

import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.locks.ReentrantLock;

public class Coordinator implements Runnable{

    private Integer clock;
    private ReentrantLock lock;
    private Integer port;


    public Coordinator(Integer port) {
        this.clock = 0;
        this.lock = new ReentrantLock();
        this.port = port;
    }

    public void requestHandler(FutureSocketChannel s) {
        ByteBuffer bf = ByteBuffer.allocate(100);
        s.read(bf).thenRun(() -> {
            bf.clear();
            ByteBuffer clockBuff = ByteBuffer.allocate(4);
            this.lock.lock();
            clockBuff.putInt(clock);
            clock++;
            this.lock.unlock();
            s.write(clockBuff.flip()).thenRun(() -> {
                clockBuff.clear();
                requestHandler(s);
            });
        });
    }

    public void acceptConnection(FutureServerSocketChannel fssc) {
        CompletableFuture<FutureSocketChannel> sc = fssc.accept();

        sc.thenAccept(s -> {
            System.out.println("New client connected to coordinator at " + this.port);
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
