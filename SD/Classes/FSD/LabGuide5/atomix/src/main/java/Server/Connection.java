package Server;

import Base.SharedResources;
import State.Client;
import State.State;
import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;

public class Connection {

    public static void acceptNew(FutureServerSocketChannel ssc, SharedResources sr) {
        CompletableFuture<FutureSocketChannel> fClient = ssc.accept();

        fClient.thenAccept(client -> {
            // Create the structure to hold his info
            Client c = new Client(client, sr.getState().mq.currentID());
            // Add the client to the sr.getState()
            sr.getState().clients.addClient(c);
            // Initialize a buffer for the client
            ByteBuffer buf = ByteBuffer.allocate(1000);
            // Put the client in a reading sr.getState()
            ClientConnection.read(c, sr, buf);
            // Accept more connections
            Connection.acceptNew(ssc, sr);
        });
    }
}