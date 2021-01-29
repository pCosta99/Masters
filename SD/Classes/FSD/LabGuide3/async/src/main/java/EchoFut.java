import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;

public class EchoFut {
    public static void main(String[] args) throws Exception {
        FutureServerSocketChannel ssc = new FutureServerSocketChannel();
        ssc.bind(new InetSocketAddress(12345));

        CompletableFuture<FutureSocketChannel> sc = ssc.accept();

        sc.thenAccept(s->{
            ByteBuffer buf = ByteBuffer.allocate(1000);

            CompletableFuture<Integer> read = s.read(buf);

            read.thenAccept(i->{
                buf.flip();
                
            });
        });
    }
}
