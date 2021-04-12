import protocol.Protocol;
import spread.*;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Server {

    // Create a new bank (for now bank just have 1 account)
    private static final Bank bank = new Bank();
    private static boolean isLeader = false;

    public static void main(String[] args) throws UnknownHostException, SpreadException, InterruptedException {
        if(args.length > 1 && args[1].equals("init")) {
            Skeleton.initialize();
            isLeader = true;
        }
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
                if (info.isCausedByDisconnect() || info.isCausedByLeave()){
                    String nextLeader = info.getMembers()[0].toString();
                    String port = nextLeader.replaceAll("[^0-9]+","");
                    if(port.equals(args[0])){
                        isLeader = true;
                    }
                } else if (info.isRegularMembership()){
                    try {
                        System.out.println("new server " + Arrays.toString(info.getMembers()));
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

        while(true){
            Thread.sleep(5000);
            if(isLeader) System.out.println(args[0] + " is leader!");
        }

        // Disconnect from spread
        // conn.disconnect();
    }
}
