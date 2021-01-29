import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.channels.SocketChannel;

public class G1Rec {

    AsynchronousSocketChannel sc;
    ByteBuffer buf = ByteBuffer.allocate(1000);
    ByteBuffer line;
    private CompletionHandler<String, Object> ch;
    private Object a;

    G1Rec() {
        buf.flip();
    }

    <T> void readLine(CompletionHandler<String,T> ch, T a) throws IOException {
        line = ByteBuffer.allocate(1000);
        this.ch = (CompletionHandler<String, Object>) ch;
        this.a = a;
        readLineRec();
    }

    void readLineRec() {
        while(buf.hasRemaining()) {
            byte c = buf.get();
            if (c == '\n') {
                line.flip();
                byte[] bytes = new byte[line.remaining()];
                ch.completed(new String(bytes), a); // Return!
            }
            line.put(c);
        }

        buf.clear();
        sc.read(buf, null, new CompletionHandler<Integer,Object>() {
            @Override
            public void completed(Integer integer, Object o) {
                if (integer<=0)
                    ch.completed(null, a);
                else {
                    buf.flip();
                    readLineRec();
                }
            }

            @Override
            public void failed(Throwable throwable, Object o) {
                ch.failed(throwable, a);
            }
        });


    }


    void main() {
        G1Rec o = new G1Rec();

        o.readLine(new CompletionHandler<String, Object>() {
            @Override
            public void completed(String s, Object o) {

            }

            @Override
            public void failed(Throwable throwable, Object o) {

            }
        });

    }
}
