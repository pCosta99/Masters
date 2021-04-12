import protocol.Protocol;
import spread.SpreadConnection;
import spread.SpreadException;
import spread.SpreadGroup;
import spread.SpreadMessage;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

public class Skeleton {
    private static boolean activated = false;
    private static List<SpreadMessage> pending = new ArrayList<>();
    private static ReentrantLock lock = new ReentrantLock();

    public static void initialize(){
        lock.lock();
        activated = true;
        lock.unlock();
    }

    public static void send(String message, SpreadGroup rep, SpreadConnection conn) throws SpreadException {
        System.out.println("Result message: " + message);
        Protocol.send(message, rep, conn);
        System.out.println("> Sent: " + message);
    }

    public static void parseCommand(SpreadMessage sm, SpreadConnection conn, Bank bank) throws SpreadException {
        String data = new String(sm.getData());
        String[] dataSplit = data.split(" ");

        switch (dataSplit[0]){
            case "movement":
                String valueStr = dataSplit[1].replaceAll("[^-0-9]+","");
                boolean opResult = bank.movement(Integer.parseInt(valueStr));
                send(opResult + " " + dataSplit[2], sm.getSender(), conn);
                break;
            case "balance":
                int balance = bank.balance();
                send(balance + " " + dataSplit[1], sm.getSender(), conn);
                break;
            case "server":
                break;
            default:
                System.out.println("Invalid data: " + data);
                send("Invalid option", sm.getSender(), conn);
                break;
        }
    }

    public static void handler(SpreadMessage sm, SpreadConnection conn, Bank bank, String port) throws SpreadException {
        String data = new String(sm.getData());
        String[] dataSplit = data.split(" ");

        if(activated) {
            lock.lock();
            parseCommand(sm, conn, bank);
            lock.unlock();
        }
        else {
            if(dataSplit[0].equals("server")){
                if(!dataSplit[1].equals(port)) {
                    lock.lock();
                    bank.setBalance(Integer.parseInt(dataSplit[2]));
                    activated = true;
                    for(SpreadMessage s : pending) parseCommand(s, conn, bank);
                    lock.unlock();
                }
            } else pending.add(pending.size(), sm);
        }
    }
}
