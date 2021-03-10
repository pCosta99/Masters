import java.net.InetSocketAddress;

import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;
import java.util.concurrent.CompletableFuture;

public class Server {

    // Create a new bank (for now bank just have 1 account)
    private static final Bank bank = new Bank();

    public static void acceptConnection(FutureServerSocketChannel fssc) {
        CompletableFuture<FutureSocketChannel> sc = fssc.accept();

        sc.thenAccept(s -> {
            System.out.println(" Client is connected!");

            Thread skT = new Thread(new Skeleton(bank, s));
            skT.start();

            acceptConnection(fssc);
        });
    }

    public static void main(String[] args) {

        FutureServerSocketChannel fssc;

        try {
            fssc = new FutureServerSocketChannel();
            fssc.bind(new InetSocketAddress(Integer.parseInt(args[0])));

            acceptConnection(fssc);

            Thread.sleep(Integer.MAX_VALUE);
        } catch (java.io.IOException | java.lang.InterruptedException e) {
            e.printStackTrace();
        }
    }
}
