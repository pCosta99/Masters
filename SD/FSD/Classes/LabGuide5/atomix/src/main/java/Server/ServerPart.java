package Server;

import Base.SharedResources;
import State.State;
import spullara.nio.channels.FutureServerSocketChannel;

import java.io.IOException;
import java.net.InetSocketAddress;


public class ServerPart implements Runnable {
    private int ownPort;
    private SharedResources sr;

    public ServerPart(int ownPort, SharedResources sr){
        this.ownPort = ownPort;
        this.sr = sr;
    }

    @Override
    public void run() {
        try {
            // Create the server socket
            FutureServerSocketChannel ssc = new FutureServerSocketChannel();
            ssc.bind(new InetSocketAddress(ownPort));

            // Initialize our state empty
            State state = new State();
            sr.setState(state);

            // Accept connections
            Connection.acceptNew(ssc, sr);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
