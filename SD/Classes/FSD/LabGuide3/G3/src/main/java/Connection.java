import State.Client;
import State.State;
import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;

public class Connection {

    public static void acceptNew(FutureServerSocketChannel ssc, State state) {
        CompletableFuture<FutureSocketChannel> fClient = ssc.accept();

        fClient.thenAccept(client -> {
            // Create the structure to hold his info
            Client c = new Client(client, state.mq.currentID());
            // Add the client to the state
            state.clients.addClient(c);
            // Initialize a buffer for the client
            ByteBuffer buf = ByteBuffer.allocate(1000);
            // Put the client in a reading state
            ClientConnection.read(c, state, buf);
            // Accept more connections
            Connection.acceptNew(ssc, state);
        });
    }
}