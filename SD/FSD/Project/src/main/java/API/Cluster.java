package API;

import java.util.ArrayList;
import java.util.List;

public class Cluster {
    private static int LOWER_BOUND = 12340;
    private static int UPPER_BOUND = 12349;
    private static int COORD_PORT = 12399;

    public static void main(String[] args) throws InterruptedException {
        // If there are args use them, otherwise use default values
        if(args.length == 3){
            LOWER_BOUND = Integer.parseInt(args[0]);
            UPPER_BOUND = Integer.parseInt(args[1]);
            COORD_PORT = Integer.parseInt(args[2]);
        }

        List<Integer> peers =  new ArrayList<>();
        for(int i=LOWER_BOUND; i < UPPER_BOUND; i++) peers.add(i);

        new Thread(new Coordinator(COORD_PORT, peers)).start();

        for(int peer : peers) new Thread(new Server(peer)).start();

        Thread.sleep(Integer.MAX_VALUE);
    }
}
