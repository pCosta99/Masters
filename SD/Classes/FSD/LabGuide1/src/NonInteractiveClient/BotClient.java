package NonInteractiveClient;

import IteractiveClient.Receiver;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.Socket;
import java.util.Random;

public class BotClient {

    public static void main(String[] args) throws IOException, InterruptedException {
        int sleepTime = Integer.parseInt(args[0]);

        System.out.println("Sleep time set to " + sleepTime + "ms.");

        BotController bc = new BotController();
        Socket socket = new Socket(InetAddress.getLocalHost(), 12345);
        BufferedReader sin = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        PrintWriter sout = new PrintWriter(socket.getOutputStream());
        String info = "";

        // The threads below are coordinated by the controller.
        // Runs the receiver
        Thread receiver = new Thread(new BotReceiver(sin, bc, sleepTime));
        receiver.start();

        // Runs the sender
        Thread sender = new Thread(new BotSender(sout, bc, sleepTime));
        sender.start();
    }
}
