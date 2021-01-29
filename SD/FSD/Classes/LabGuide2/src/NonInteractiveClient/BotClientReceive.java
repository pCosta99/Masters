package NonInteractiveClient;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

public class BotClientReceive implements Runnable {
    private SocketChannel socket;
    private MessagesQueue queue;

    public BotClientReceive(SocketChannel socket, MessagesQueue queue) {
        this.socket = socket;
        this.queue = queue;
    }

    @Override
    public void run() {
        ByteBuffer buf = ByteBuffer.allocate(100);

        /* Listen for messages */
        while(true) {
            try {
                this.socket.read(buf);

                buf.flip();
                this.queue.add(new String(buf.array()));

                buf.clear();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}