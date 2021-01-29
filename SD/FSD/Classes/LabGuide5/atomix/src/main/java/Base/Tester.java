package Base;

import Server.ClientConnection;
import Server.ServerPart;
import io.atomix.cluster.messaging.MessagingConfig;
import io.atomix.cluster.messaging.impl.NettyMessagingService;
import io.atomix.utils.net.Address;

import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

public class Tester {
    public static void main(String[] args) throws Exception {
        ScheduledExecutorService es = Executors.newScheduledThreadPool(1);
        NettyMessagingService ms = new NettyMessagingService("p2pChat", Address.from("localhost", Integer.parseInt(args[1])), new MessagingConfig());

        // Create the shared resources.
        SharedResources sr = new SharedResources(ms, args);

        ms.registerHandler("message", (a,m) ->{
            String s = new String(m);
            s = s.substring(0, s.indexOf('\n'));
            System.out.println("Message: " + s + " from " + a.port());
            // Broadcast the message to the clients of the server.
            // Weird \n "bug", need to fix it eventually.
            String fix_s = s + "\n";
            sr.addMessageToState(fix_s);
            ClientConnection.handleWrites(sr.getState());
        }, es);

        ms.registerHandler("messageFromClient", (a,m) ->{
            String s = new String(m);
            s = s.substring(0, s.indexOf('\n'));
            System.out.println("Message: " + s + " from " + a.port() + " client base.");
        }, es);

        ms.start();

        // Launch new thread to act as a server
        Thread t = new Thread(new ServerPart(Integer.parseInt(args[0]), sr));
        t.start();

        ByteBuffer buf = ByteBuffer.allocate(100);

        /* Read messages and send them to server */
        ReadableByteChannel channel = Channels.newChannel(System.in);
        while (channel.read(buf) >= 0) {
            for(int i=1;i<args.length;i++) {
                buf.flip();
                ms.sendAsync(Address.from("localhost", Integer.parseInt(args[i])), "message", buf.array());
                buf.clear();
            }
        }
    }
}