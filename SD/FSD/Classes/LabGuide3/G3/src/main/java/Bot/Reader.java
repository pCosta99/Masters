package Bot;

import java.io.BufferedReader;
import java.io.BufferedWriter;

public class Reader extends Thread {

    private final BufferedReader in;
    private final long delay;
    private int n;

    Reader(BufferedReader in, long delay, int n){
        this.in = in;
        this.delay = delay;
        this.n = n;
    }

    public void run() {
        try {
            while(n-- > 0){
                String received = in.readLine();
                System.out.println("Got " + received + "!");
                Thread.sleep(delay);
            }
        } catch (Exception e){
            e.printStackTrace();
        }
    }

}
