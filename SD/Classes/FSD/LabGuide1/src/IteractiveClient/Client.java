package IteractiveClient;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.Socket;

public class Client {
    public static void main(String[] args) throws IOException {
        Socket socket = new Socket(InetAddress.getLocalHost(), 12345);
        BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
        BufferedReader sin = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        PrintWriter sout = new PrintWriter(socket.getOutputStream());
        String input = "";

        // Launch a thread that just receives the broadcasts from the server
        Thread receiver = new Thread(new Receiver(sin));
        receiver.start();

        // Main cycle to send messages to the server
        while(!input.equals("quit")){
            input = stdin.readLine();
            System.out.println("Read: " + input);
            sout.println(input);
            sout.flush();
        }
    }
}
