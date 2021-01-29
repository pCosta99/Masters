import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;

public class FutureSocketChannelWritter {
    public static CompletableFuture<Void> write(FutureSocketChannel socket, String message) {
        CompletableFuture<Void> acceptor = new CompletableFuture<>();
        ByteBuffer buf = ByteBuffer.wrap(message.getBytes());

        writeUntilNoneRemaining(socket, buf, acceptor);

        return acceptor;
    }

    private static void writeUntilNoneRemaining(FutureSocketChannel socket, ByteBuffer buf, CompletableFuture<Void> acceptor) {
        socket.write(buf).thenAccept(wr -> {
            if(buf.hasRemaining()){
                writeUntilNoneRemaining(socket, buf, acceptor);
            } else acceptor.complete(null);
        });
    }
}
