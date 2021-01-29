package Bot;

import java.io.BufferedWriter;

public class Writter extends Thread {

    private final BufferedWriter out;
    private final String message;
    private final long delay;
    private int n;

    Writter(BufferedWriter out, String message, long delay, int n){
        this.out = out;
        this.message = message;
        this.delay = delay;
        this.n = n;
    }

    public void run() {
        try {
            while(n-- > 0){
                out.write(message);
                out.flush();
                System.out.print("Sent " + message);
                Thread.sleep(delay);
            }
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}
