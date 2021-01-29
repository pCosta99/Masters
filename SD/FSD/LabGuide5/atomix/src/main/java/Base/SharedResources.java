package Base;

import State.State;
import io.atomix.cluster.messaging.impl.NettyMessagingService;
import io.atomix.utils.net.Address;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class SharedResources {
    private final NettyMessagingService ms;
    private final List<Integer> otherPorts;
    private State state;

    public SharedResources(NettyMessagingService ms, String[] args){
        this.ms = ms;
        this.otherPorts = Arrays.stream(args).skip(1).map(Integer::parseInt).collect(Collectors.toList());
    }

    // Sends a message to all hosts on the cluster.
    public void broadcastMessageOnCluster(String message){
        for(Integer port : otherPorts){
            ms.sendAsync(Address.from("localhost", port), "messageFromClient", message.getBytes());
        }
    }

    // Adds a message to the queue.
    public void addMessageToState(String message){
        this.state.mq.putMessage(message);
    }

    public void setState(State state){
        this.state = state;
    }

    public State getState(){
        return this.state;
    }
}
