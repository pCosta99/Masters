import io.atomix.cluster.messaging.MessagingConfig;
import io.atomix.cluster.messaging.impl.NettyMessagingService;
import io.atomix.utils.net.Address;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.stream.Collectors;

public class Sync {
    public static List<Integer> otherPorts;
    public static Integer port;

    public static void main(String[] args) {
        // args[0] -> port to run this instance
        // args[1..n] -> ports of other instances
        port = Integer.parseInt(args[0]);
        otherPorts = Arrays.stream(args).skip(1).map(Integer::parseInt).collect(Collectors.toList());

        ScheduledExecutorService es = Executors.newScheduledThreadPool(1);
        NettyMessagingService ms = new NettyMessagingService("name", Address.from(Integer.parseInt(args[0])), new MessagingConfig());

        // Launches the messaging service.
        ms.start().join();

        // Register a handler to process election messages.
    }

    void handleElection(NettyMessagingService ms){

    }
}
