package NonInteractiveClient;

import InteractiveClient.ClientReceive;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.LinkedList;
import java.util.Queue;

public class BotClient {

    /**
     * The nonInteractiveClient(BotClient) implements a bot that sleeps a given time between sending or printing received messages.
     * args[0] = sleep time in ms between sending messages
     * args[1] = sleep time in ms between printing received messages
     */
    public static void main(String[] args) throws IOException, InterruptedException {

        System.out.println("Sleep time: " + Float.parseFloat(args[0]));
        float sleepTime = Float.parseFloat(args[0]) * 1000;

        /* Socket connects to port 12345 with IP 127.0.0.1 (localhost) */
        SocketChannel socket = SocketChannel.open();
        socket.connect(new InetSocketAddress(InetAddress.getLocalHost(),12345));

        /*  Start new client Receiver and Writer
            The receiver will add incoming messages to the queue and notify the writer
            The writer will sleep 10 seconds and then wait for notification to write some message
        */
        MessagesQueue queue = new MessagesQueue();
        Thread clientReceiver = new Thread(new BotClientReceive(socket, queue));
        clientReceiver.start();
        Thread clientWriter = new Thread(new BotClientWrite(socket, Float.parseFloat(args[1]), queue));
        clientWriter.start();

        ByteBuffer buf = ByteBuffer.allocate(100);
        /* Send them to server */
        buf.put(("Message sent after sleeping: " + sleepTime).getBytes());
        buf.flip();
        while (true) {
            Thread.sleep((long) sleepTime);
            socket.write(buf.duplicate());
        }
    }

}