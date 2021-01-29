package Server;

import Base.SharedResources;
import State.Client;
import State.State;

import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;

public class ClientConnection {

    public static void read(Client client, SharedResources sr, ByteBuffer buf) {
        FutureSocketChannelReader.readLine(client.getSocket(), buf).thenAccept(message -> {
            if(message == null){
                // Connection was closed
                sr.getState().clients.removeClient(client);
                return;
            }
            // Process message read otherwise
            System.out.println("Received: " + message);
            sr.getState().mq.putMessage(message);
            sr.broadcastMessageOnCluster(message);
            ClientConnection.handleWrites(sr.getState());
            ClientConnection.read(client, sr, buf);
        });
    }

    // Broadcast
    public static void handleWrites(State state) {
        for(Client c : state.clients.getClients()){
            write(c, state);
        }
    }

    private static void write(Client c, State state) {
        String message = state.mq.getMessage(c.getMessageID());
        CompletableFuture<Void> writter = FutureSocketChannelWritter.write(c.getSocket(), message);

        writter.thenAccept(vd -> {
            c.incrementMessageID();
            write(c, state);
        });
    }
}
