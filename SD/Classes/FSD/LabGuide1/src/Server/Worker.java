package Server;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

public class Worker implements Runnable {
    private Socket sc;
    private BufferedReader in;
    private Controller controller;

    public Worker(Socket clientSocket, Controller controller) throws IOException {
        this.sc = clientSocket;
        in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
        this.controller = controller;
    }

    @Override
    public void run() {
        String s;
        try{
            while(true) {
                s = in.readLine();
                // Log the message read
                System.out.println("Got message: " + s + ", broadcasting.");
                // Broadcast the message through the Controller
                controller.broadcast(s);
            }
        } catch (IOException e){
            controller.closeWriter(this.sc);
            try {
                sc.shutdownInput();
                sc.shutdownOutput();
                sc.close();
            } catch (IOException e1){
                e1.printStackTrace();
            }
        }
    }
}
