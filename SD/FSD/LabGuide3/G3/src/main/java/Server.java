import State.State;
import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousChannel;
import java.nio.channels.AsynchronousChannelGroup;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import static java.util.concurrent.Executors.defaultThreadFactory;

public class Server {
    public static List<CompletableFuture<FutureSocketChannel>> list;

    public static void main(String[] args) throws Exception {
        // What does this do?
        AsynchronousChannelGroup g = AsynchronousChannelGroup.withFixedThreadPool(1, defaultThreadFactory());

        // Create the server socket
        FutureServerSocketChannel ssc = new FutureServerSocketChannel();
        ssc.bind(new InetSocketAddress(12345));

        // Initialize our state empty
        State state = new State();

        // Accept connections
        Connection.acceptNew(ssc, state);

        list = new ArrayList<>();

        while(true){
            Thread.sleep(Long.MAX_VALUE);
        }
    }

    /*
    public static void readLine(FutureServerSocketChannel ssc){
            CompletableFuture<FutureSocketChannel> sc = ssc.accept();
            list.add(sc);
            readLineRec(sc);
    }

    public static void readLineRec(CompletableFuture<FutureSocketChannel> sc){
        sc.thenAccept(s -> {
            ByteBuffer buf = ByteBuffer.allocate(1000);

            CompletableFuture<Integer> read = s.read(buf);

            read.thenAccept(i->{
                list.forEach(s1 -> s1.thenAccept(s2 -> {
                    buf.flip();
                    s2.write(buf);
                }));
                readLineRec(sc);
            });
        }).thenCompose(r -> {
            readLine(ssc);
            return null;
        });
    } */
}
