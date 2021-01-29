package Server;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    public static void main(String[] args) throws IOException {
        System.out.println("Initializing server...");
        ServerSocket ss = new ServerSocket(12345);
        Controller controller = new Controller();

        /* Ideally we should have a stopping mechanism somewhere around here so we could call a shutdown function */
        while(true){
            Socket clientSocket = ss.accept();
            System.out.println("Accepted a new customer.");
            controller.addWriter(clientSocket);
            Thread customer = new Thread(new Worker(clientSocket, controller));
            customer.start();
        }
    }
}
