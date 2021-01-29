package NonInteractiveClient;

import java.nio.channels.SocketChannel;

public class BotClientWrite implements Runnable{
    private SocketChannel socket;
    private float sleepTime;
    private MessagesQueue queue;

    public BotClientWrite(SocketChannel socket, float sleepTime, MessagesQueue queue) {
        this.socket = socket;
        this.sleepTime = sleepTime * 1000;
        this.queue = queue;
    }

    @Override
    public void run() {
        while(true) {
            try {
                Thread.sleep((long) sleepTime);
                System.out.print("After sleeping: " + sleepTime/1000 + "s | Got to print: " + this.queue.poll() + "\n");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
