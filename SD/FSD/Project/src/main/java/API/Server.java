package API;

import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class Server implements Runnable {
    int port;
    private final State state;

    public Server(int port) {
        this.port = port;
        this.state = new State();
    }

    public void acceptConnection(FutureServerSocketChannel fssc) {
        CompletableFuture<FutureSocketChannel> sc = fssc.accept();

        sc.thenAccept(s -> {
            Connection c = null;
            try {
                c = new Connection(s, this.state, port);
            } catch (IOException | ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            //System.out.println("New client connected to " + this.port);
            c.receiver();

            acceptConnection(fssc);
        });
    }

    @Override
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
