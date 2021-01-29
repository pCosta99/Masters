import State.Client;
import State.State;
import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.util.concurrent.CompletableFuture;

public class ClientConnection {
    public static void read(Client client, State state, ByteBuffer buf) {
        FutureSocketChannelReader.readLine(client.getSocket(), buf).thenAccept(message -> {
            if(message == null){
                // Connection was closed
                state.clients.removeClient(client);
                return;
            }
            // Process message read otherwise
            System.out.println("Received: " + message);
            state.mq.putMessage(message);
            ClientConnection.handleWrites(state);
            ClientConnection.read(client, state, buf);
        });
    }

    // Broadcast
    private static void handleWrites(State state) {
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
