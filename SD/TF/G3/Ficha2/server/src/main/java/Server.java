import protocol.Protocol;
import spread.*;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class Server {

    // Create a new bank (for now bank just have 1 account)
    private static final Bank bank = new Bank();

    public static void main(String[] args) throws UnknownHostException, SpreadException, InterruptedException {
        if(args.length > 1 && args[1].equals("init")) Skeleton.initialize();
        // Create connection
        SpreadConnection conn = new SpreadConnection();
        String server = "server" + args[0];
        conn.connect(InetAddress.getByName("localhost"), Integer.parseInt(args[0]), server, false, true);
        conn.add(new AdvancedMessageListener() {
            @Override
            public void regularMessageReceived(SpreadMessage sm) {
                try {
                    Skeleton.handler(sm, conn, bank, args[0]);
                } catch (SpreadException e) {
                    e.printStackTrace();
                }
            }

            @Override
            public void membershipMessageReceived(SpreadMessage sm) {
                MembershipInfo info = sm.getMembershipInfo();
                if (info.isRegularMembership()){
                    try {
                        Protocol.send("server " + args[0] + " " + bank.balance(), sm.getSender(), conn);
                    } catch (SpreadException e) {
                        e.printStackTrace();
                    }
                }
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
