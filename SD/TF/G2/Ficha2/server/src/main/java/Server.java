import spread.SpreadConnection;
import spread.SpreadException;
import spread.SpreadGroup;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class Server {

    // Create a new bank (for now bank just have 1 account)
    private static final Bank bank = new Bank();

    public static void main(String[] args) throws UnknownHostException, SpreadException, InterruptedException {
        // Create connection
        SpreadConnection conn = new SpreadConnection();
        conn.connect(InetAddress.getByName("localhost"), 4803, "name", false, false);
        conn.add(req -> {
            try {
                Skeleton.parseCommand(req, conn, bank);
            } catch (SpreadException e) {
                e.printStackTrace();
            }
        });

        // Join logical server group
        SpreadGroup group = new SpreadGroup();
        group.join(conn, "servers");

        Thread.sleep(Integer.MAX_VALUE);

        // Disconnect from spread
        conn.disconnect();
    }
}
