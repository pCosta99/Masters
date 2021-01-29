import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;

public class G1Fut {

    AsynchronousSocketChannel sc;
    ByteBuffer buf = ByteBuffer.allocate(1000);
    ByteBuffer line;

    class Box<T> {
        private CompletionHandler<T, Object> ch;
        private Object a;
        private T v;

        void complete(T v) {
            this.v = v;
            if (ch != null)
                ch.completed(v, a); // Return!
        }

        public void setCH(CompletionHandler<T,Object> ch) {
            this.ch = ch;
            if (b!=null)
                ch.completed(v, null);
        }
    }

    Box<String> b;


    G1Fut() {
        buf.flip();
    }

    Box<String> readLine() throws IOException {
        line = ByteBuffer.allocate(1000);

        b = new Box();

        readLineRec();

        return b;
    }

    void readLineRec() {
        while(buf.hasRemaining()) {
            byte c = buf.get();
            if (c == '\n') {
                line.flip();
                byte[] bytes = new byte[line.remaining()];
                b.complete(new String(bytes)); // Return!
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
        G1Fut o = new G1Fut();

        Box<String> b = o.readLine();

        b.setCH(...);

    }
}
