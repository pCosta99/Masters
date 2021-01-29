import org.zeromq.ZMQ;
import utils.ClientConfig;
import utils.JsonParser;

import java.io.IOException;
import java.net.Socket;

public class Main {
    public static void main(String[] args) throws IOException {

        // Parse the JSON configuration file by a given path (default: ../config.json)
        ClientConfig config = JsonParser.parseConfig(args[0]);

        if (config == null){
            System.err.println("Error while parsing the JSON file.");
            System.exit(1);
        }

        // Create a socket to communicate with the frontend server
        Socket socket = new Socket(config.frontendServer.frontend_address, config.frontendServer.frontend_port);

        // Initialize a thread to handle client requests
        Thread reqHandler = new Thread(new RequestsHandler(socket, ZMQ.context(1), config));
        reqHandler.start();
    }
}
