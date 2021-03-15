import protocol.Protocol;
import spread.SpreadConnection;
import spread.SpreadException;
import spread.SpreadGroup;
import spread.SpreadMessage;

public class Skeleton {

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
                send(String.valueOf(opResult), sm.getSender(), conn);
                break;
            case "balance":
                int balance = bank.balance();
                send(String.valueOf(balance), sm.getSender(), conn);
                break;
            default:
                System.out.println("Invalid data: " + data);
                send("Invalid option", sm.getSender(), conn);
                break;
        }
    }
}
