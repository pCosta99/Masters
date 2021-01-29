package IteractiveClient;

import java.io.BufferedReader;
import java.io.IOException;

public class Receiver implements Runnable {
    private BufferedReader sin;

    public Receiver(BufferedReader sin) {
        this.sin = sin;
    }

    @Override
    public void run() {
        String s;
        while(true){
            try {
                s = sin.readLine();
                System.out.println("Received message: " + s);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
