import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.CompletionHandler;
import java.nio.channels.SocketChannel;

public class G1 {

    SocketChannel sc;
    ByteBuffer buf = ByteBuffer.allocate(1000);

    G1() {
        buf.flip();
    }

    String readLine() throws IOException {
        ByteBuffer line = ByteBuffer.allocate(1000);

        while(true) {
            if (!buf.hasRemaining()) {
                buf.clear();
                if (sc.read(buf) <= 0)
                    return null;
                buf.flip();
            }
            while(buf.hasRemaining()) {
                byte c = buf.get();
                if (c == '\n') {
                    line.flip();
                    byte[] bytes = new byte[line.remaining()];
                    return new String(bytes);
                }
                line.put(c);
            }
        }
    }
}
