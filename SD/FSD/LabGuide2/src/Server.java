import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousChannelGroup;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.sql.Connection;
import java.util.concurrent.TimeUnit;

import static java.util.concurrent.Executors.defaultThreadFactory;

public class Server {

    private static class Context {

        public Context(ByteBuffer buf, AsynchronousSocketChannel sc) {
            this.buf = buf;
            this.sc = sc;
        }

        ByteBuffer buf;
        AsynchronousSocketChannel sc;
    }

    private static void recRead(AsynchronousSocketChannel sc, ByteBuffer buf, Context c){
            sc.read(buf, c, new CompletionHandler<Integer, Context>() {
                @Override
                public void completed(Integer integer, Context c) {
                    if(integer == 0) {
                        // Fecha socket e faz return
                        try {
                            sc.close();
                            return;
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    } else {
                        // Le outra vez
                            recRead(sc, buf, c);
                    }
                    c.buf.flip();

                    c.sc.write(c.buf, c, new CompletionHandler<Integer, Context>() {
                        @Override
                        public void completed(Integer integer, Context context) {
                            System.out.println("Done!");
                            c.buf.clear();
                        }

                        @Override
                        public void failed(Throwable throwable, Context context) {

                        }
                    });
                }

                @Override
                public void failed(Throwable throwable, Context c) {
                    System.out.println("Client closed.");
                }
            });
    }

    private static final CompletionHandler<AsynchronousSocketChannel, AsynchronousServerSocketChannel> ach =
            new CompletionHandler<AsynchronousSocketChannel, AsynchronousServerSocketChannel>() {
        @Override
        public void completed(AsynchronousSocketChannel sc,
                AsynchronousServerSocketChannel o) {
            System.out.println("Accepted!");

            ByteBuffer buf = ByteBuffer.allocate(1000);
            Context c = new Context(buf, sc);

            recRead(sc, buf, c);

            acceptRec(o);
        }

        @Override
        public void failed(Throwable throwable, AsynchronousServerSocketChannel o) {

        }
    };

    public static void acceptRec(AsynchronousServerSocketChannel ssc) {
        ssc.accept(ssc, ach);
    }

    public static void main(String[] args) throws Exception {

        AsynchronousChannelGroup g =
                AsynchronousChannelGroup.withFixedThreadPool(1, defaultThreadFactory());

        AsynchronousServerSocketChannel ssc =
                AsynchronousServerSocketChannel.open(g);
        ssc.bind(new InetSocketAddress(12345));

        acceptRec(ssc);

        g.awaitTermination(Long.MAX_VALUE, TimeUnit.DAYS);
        System.out.println("Terminei!");
    }
}
