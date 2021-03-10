package protocol;

import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;

public class Protocol {
    public static void send(FutureSocketChannel socket, String message){
        byte[] mBytes = message.getBytes(StandardCharsets.UTF_8);
        ByteBuffer buff = ByteBuffer.allocate(mBytes.length);
        buff.put(mBytes).flip();
        ByteBuffer sizeBuf = ByteBuffer.allocate(4);
        sizeBuf.putInt(mBytes.length).flip();
        socket.write(sizeBuf).thenRun(() -> socket.write(buff));
    }

    public static CompletableFuture<String> read(FutureSocketChannel socket){
        CompletableFuture<String> cf = new CompletableFuture<>();
        ByteBuffer sizeBuff = ByteBuffer.allocate(4);
        socket.read(sizeBuff).thenRun(() -> {
            sizeBuff.flip();
            ByteBuffer buff = ByteBuffer.allocate(sizeBuff.getInt());
            socket.read(buff).thenRun(() -> cf.complete(new String(buff.array())));
        });
        return cf;
    }
}
