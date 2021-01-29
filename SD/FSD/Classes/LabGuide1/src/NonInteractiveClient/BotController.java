package NonInteractiveClient;

import java.util.concurrent.locks.ReentrantLock;

public class BotController {
    // The controller will manage what the bot can do.
    // If the controllers is asleep, then nothing can happen. The other threads keep this controller updated.
    public boolean asleep;
    public ReentrantLock sleepLock;

    public BotController(){
        asleep = false;
        sleepLock = new ReentrantLock();
    }

    public void set_asleep(int time) throws InterruptedException {
        asleep = true;
        sleepLock.lock();
        Thread.sleep(time);
        sleepLock.unlock();
        asleep = false;
    }
}
