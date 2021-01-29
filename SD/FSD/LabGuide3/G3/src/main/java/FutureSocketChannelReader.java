import spullara.nio.channels.FutureSocketChannel;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;

public class FutureSocketChannelReader {
    public static CompletableFuture<String> readLine(FutureSocketChannel fsc, ByteBuffer buf){
        CompletableFuture<String> acceptor = new CompletableFuture<>();
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        boolean foundLine = false;

        // Reads a potential line from buf, if there is no line to be read then foundLine will still be false and there will be something read out of the socket.
        if (buf.position() > 0){
            buf.flip();
            foundLine = readLineInto(buf, bytes);
        }

        // If we did not find a line in the buffer then we need to read from the socket.
        if (foundLine) {
            acceptor.complete(new String(bytes.toByteArray(), StandardCharsets.UTF_8));
        }
        else {
            fsc.read(buf).thenAccept(rd -> {
                if (rd == -1) {
                    acceptor.complete(null);
                    return;
                }
                buf.flip();
                readLineInto(buf, bytes);
                acceptor.complete(new String(bytes.toByteArray(), StandardCharsets.UTF_8));
            });
        }

        return acceptor;
    }

    private static boolean readLineInto(ByteBuffer buf, ByteArrayOutputStream bytes) {
        boolean ret = false;

        // Go until \n or the end
        while(buf.hasRemaining() && !ret){
            byte b = buf.get();
            bytes.write(b); // keep writing everything read into the BAoS
            ret = (b == '\n');
        }

        buf.compact();

        return ret;
    }
}
