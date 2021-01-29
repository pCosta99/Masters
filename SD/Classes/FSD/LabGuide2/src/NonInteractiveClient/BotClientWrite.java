package NonInteractiveClient;

import java.nio.channels.SocketChannel;

public class BotClientWrite implements Runnable{
    private SocketChannel socket;
    private int sleepTime;
    private MessagesQueue queue;

    public BotClientWrite(SocketChannel socket, int sleepTime, MessagesQueue queue) {
        this.socket = socket;
        this.sleepTime = sleepTime;
        this.queue = queue;
    }

    @Override
    public void run() {
        while(true) {
            try {
                Thread.sleep(sleepTime);
                System.out.print("After sleeping: "+ sleepTime + " | Got to print: " + this.queue.poll());
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
