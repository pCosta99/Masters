package InteractiveClient;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

public class ClientReceive implements Runnable {
    private BufferedReader in;

    public ClientReceive(Socket socket) throws IOException {
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
    }

    @Override
    public void run() {

        /* Listen for messages */
        while(true) {
            try {
                String byteRead = this.in.readLine();
                System.out.println("Received: " + byteRead);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}