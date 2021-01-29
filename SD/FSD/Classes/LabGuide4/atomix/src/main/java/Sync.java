import io.atomix.cluster.messaging.MessagingConfig;
import io.atomix.cluster.messaging.impl.NettyMessagingService;
import io.atomix.utils.net.Address;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class Sync {
    static List<Integer> otherPorts;
    static Integer port;
    static Object leaderLock = new Object();
    static Integer leader;
    static NettyMessagingService ms;
    static ScheduledExecutorService es;

    public static void main(String[] args) throws IOException {
        // args[0] -> port to run this instance
        // args[1..n] -> ports of other instances
        port = Integer.parseInt(args[0]);
        otherPorts = Arrays.stream(args).skip(1).map(Integer::parseInt).collect(Collectors.toList());

        // Setup leader to be ourselves.
        leader = port;

        es = Executors.newScheduledThreadPool(1);
        ms = new NettyMessagingService("name", Address.from(Integer.parseInt(args[0])), new MessagingConfig());

        // Launches the messaging service.
        ms.start().join();

        // Register a handler to process election messages. a -> client address; m-> message
        handleElection();

        System.out.println("ENTER TO CONTINUE.");
        System.in.read();

        // Send a first message when you join the NettyMessagingService
        sendPort();
        setSchedule();
    }

    public static void handleElection(){
        ms.registerHandler("election", (a,m) -> {
            synchronized (leaderLock){
                if(a.port() > leader){
                    leader = a.port();
                    System.out.println("New leader is " + leader + ".");
                }
            }
        }, es);
    }

    public static void setSchedule() {
        es.schedule(()-> {
            synchronized (leaderLock){
                System.out.println("The leader is: " + leader + ".");
            }
            sendPort();
            setSchedule();

        }, 1, TimeUnit.SECONDS);
    }

    public static void sendPort() {
        for(Integer sendToPort : otherPorts) {
            ms.sendAsync(Address.from("localhost", sendToPort), "election", "msg".getBytes()).
                    thenRun(() -> {
                        System.out.println("Sent my port " +  port + " to " + sendToPort + ".");
                    });
        }
    }
}
