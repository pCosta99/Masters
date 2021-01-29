package NonInteractiveClient;

import java.io.BufferedReader;
import java.io.IOException;

public class BotReceiver implements Runnable {
    private BufferedReader sin;
    private BotController botController;
    private int sleepTime;

    public BotReceiver(BufferedReader sin, BotController botController, int sleepTime) {
        this.sin = sin;
        this.botController = botController;
        this.sleepTime = sleepTime;
    }

    @Override
    public void run() {
        String s;
        while(true){
            if(!botController.asleep){
                try {
                    s = sin.readLine();
                    System.out.println("Received message: " + s);
                    botController.set_asleep(sleepTime);
                } catch (IOException | InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
