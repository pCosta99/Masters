import spread.SpreadConnection;
import spread.SpreadException;
import spread.SpreadMessage;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;

public class Experimenting {
    public static void main(String[] args) throws UnknownHostException, SpreadException, InterruptedException {
        SpreadConnection conn = new SpreadConnection();
        conn.connect(InetAddress.getByName("localhost"), 4803, "name", false, false);
        conn.add(req -> {
            SpreadMessage rep = new SpreadMessage();
            rep.setData("recebi a mensagem!".getBytes(StandardCharsets.UTF_8));
            rep.setReliable();
            rep.addGroup(req.getSender());
            System.out.println("Recebi a mensagem: " + new String(req.getData()));
            try {
                conn.multicast(rep);
            } catch (SpreadException e) {
                e.printStackTrace();
            }
        });
        Thread.sleep(Long.MAX_VALUE);
        conn.disconnect();
    }
}
