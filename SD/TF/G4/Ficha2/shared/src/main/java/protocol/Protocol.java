package protocol;

import spread.SpreadConnection;
import spread.SpreadException;
import spread.SpreadGroup;
import spread.SpreadMessage;

import java.nio.charset.StandardCharsets;

public class Protocol {
    public static void send(String message, String rep, SpreadConnection conn) throws SpreadException {
        SpreadMessage sm = new SpreadMessage();
        sm.setData(message.getBytes(StandardCharsets.UTF_8));
        sm.setSafe();
        sm.addGroup(rep);
        conn.multicast(sm);
    }

    public static void send(String message, SpreadGroup rep, SpreadConnection conn) throws SpreadException {
        SpreadMessage sm = new SpreadMessage();
        sm.setData(message.getBytes(StandardCharsets.UTF_8));
        sm.setSafe();
        sm.addGroup(rep);
        conn.multicast(sm);
    }
}
