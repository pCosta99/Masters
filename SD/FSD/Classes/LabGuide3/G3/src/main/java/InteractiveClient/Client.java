package InteractiveClient;

import java.io.*;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SocketChannel;

public class Client {

    public static void main(String[] args) throws IOException {

        /* Socket connects to port 12345 with IP 127.0.0.1 (localhost) */
        Socket socket = new Socket(InetAddress.getLocalHost(), 12345);
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));

        /* Start new client worker*/
        Thread clientWorker = new Thread(new ClientReceive(socket));
        clientWorker.start();

        ByteBuffer buf = ByteBuffer.allocate(100);
        /* Read messages and send them to server */
        BufferedReader sys_in = new BufferedReader(new InputStreamReader(System.in));

        while (true){
            String s = sys_in.readLine();
            s = s + "\n";
            bw.write(s);
            bw.flush();
        }
    }
}