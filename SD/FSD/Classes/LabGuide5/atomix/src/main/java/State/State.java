package State;

import Base.SharedResources;

public class State {
    // The state contains all clients received to the moment
    public final Clients clients;
    public final MessageQueue mq;

    public State() {
        mq = new MessageQueue();
        clients = new Clients();
    }
}
