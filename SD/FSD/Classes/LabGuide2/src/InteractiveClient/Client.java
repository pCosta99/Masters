package InteractiveClient;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SocketChannel;

public class Client {

    public static void main(String[] args) throws IOException {

        /* Socket connects to port 12345 with IP 127.0.0.1 (localhost) */
        SocketChannel socket = SocketChannel.open();
        socket.connect(new InetSocketAddress(InetAddress.getLocalHost(),12345));

        /* Start new client worker*/
        Thread clientWorker = new Thread(new ClientReceive(socket));
        clientWorker.start();

        ByteBuffer buf = ByteBuffer.allocate(100);
        /* Read messages and send them to server */
        ReadableByteChannel channel = Channels.newChannel(System.in);
        while (channel.read(buf) >= 0) {
            buf.flip();
            socket.write(buf);
            buf.clear();
        }
    }
}