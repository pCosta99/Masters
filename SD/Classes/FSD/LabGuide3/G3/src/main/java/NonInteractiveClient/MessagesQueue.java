package NonInteractiveClient;

import java.util.LinkedList;
import java.util.Queue;

public class MessagesQueue {
    private Queue<String> queue;

    public MessagesQueue() {
        this.queue = new LinkedList<>();
    }

    public synchronized String poll() throws InterruptedException {
        wait();
        return this.queue.poll();
    }

    public synchronized void add(String message) {
        this.queue.add(message);
        notify();
    }
}
