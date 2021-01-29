import io.atomix.cluster.messaging.MessagingConfig;
import io.atomix.cluster.messaging.impl.NettyMessagingService;
import io.atomix.utils.net.Address;
import spullara.nio.channels.FutureServerSocketChannel;
import spullara.nio.channels.FutureSocketChannel;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class Tester {
    public static void main(String[] args) {
        ScheduledExecutorService es = Executors.newScheduledThreadPool(1);
        // args[0] -> port to run this instance
        NettyMessagingService ms = new NettyMessagingService("name", Address.from(Integer.parseInt(args[0])), new MessagingConfig());

        ms.registerHandler("hello", (a,m)->{
            System.out.println("Hello "+new String(m)+" from "+a);
        }, es);

        ms.start();

        es.schedule(()-> {
            System.out.println("Timeout!");
        }, 1, TimeUnit.SECONDS);

        ms.sendAsync(Address.from("localhost", 12345), "hello", "world!".getBytes())
                .thenRun(()->{
                    System.out.println("Mensagem enviada!");
                })
                .exceptionally(t->{
                   t.printStackTrace();
                   return null;
                });
    }
}
