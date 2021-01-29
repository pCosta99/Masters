package NonInteractiveClient;

import java.io.PrintWriter;
import java.util.Random;

public class BotSender implements Runnable {
    private PrintWriter sout;
    private BotController bc;
    private int sleepTime;

    public BotSender(PrintWriter sout, BotController bc, int sleepTime) {
        this.sout = sout;
        this.bc = bc;
        this.sleepTime = sleepTime;
    }

    public static String genRandomString() {
        int leftLimit = 48; // numeral '0'
        int rightLimit = 122; // letter 'z'
        int targetStringLength = 10;
        Random random = new Random();

        String generatedString = random.ints(leftLimit, rightLimit + 1)
                .filter(i -> (i <= 57 || i >= 65) && (i <= 90 || i >= 97))
                .limit(targetStringLength)
                .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
                .toString();

        return generatedString;
    }

    @Override
    public void run() {
        while(true) {
            if (!bc.asleep) {
                try {
                    String send = genRandomString();
                    System.out.println("Sending: " + send);
                    sout.println(send);
                    sout.flush();
                    bc.set_asleep(sleepTime);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
