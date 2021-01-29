package Protocol;

import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;

public class Protocol {
    public static CompletableFuture<GenericMessage> read(FutureSocketChannel socket){
        CompletableFuture<GenericMessage> acceptor = new CompletableFuture<>();

        // Get the message size
        ByteBuffer buffSize = ByteBuffer.allocate(4);
        socket.read(buffSize).thenRun(() -> {
            ByteBuffer buff = ByteBuffer.allocate(buffSize.flip().getInt());
            buffSize.clear();
            // Read the message with the content
            socket.read(buff).thenRun(() -> {
                buff.flip();
                GenericMessage m = new GenericMessage(buff);
                buff.clear();
                acceptor.complete(m);
            });
        });

        return acceptor;
    }

    public static CompletableFuture<Integer> write(FutureSocketChannel socket, GenericMessage gm){
        return socket.write(gm.serialize());
    }
}
