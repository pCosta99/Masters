import org.w3c.dom.ls.LSOutput;
import protocol.Protocol;
import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

public class Skeleton implements Runnable{

    private final FutureSocketChannel socket;
    private final Bank bank;

    public Skeleton(Bank bank, FutureSocketChannel socket){
        this.bank = bank;
        this.socket = socket;
    }

    public void run() {
        receiver();
    }

    public void receiver() {
        Protocol.read(socket).thenAccept(data -> {
            System.out.println("> Received: " + data);
            parseCommand(data);
            receiver();
        });
    }

    public void send(String message){
        System.out.println("Result message: " + message);
        Protocol.send(socket, message);
        System.out.println("> Sent: " + message);
    }

    private void parseCommand(String data){
        String[] dataSplit = data.split(" ");

        switch (dataSplit[0]){
            case "movement":
                String valueStr = dataSplit[1].replaceAll("[^-0-9]+","");
                boolean opResult = bank.movement(Integer.parseInt(valueStr));
                send(String.valueOf(opResult));
                break;
            case "balance":
                int balance = bank.balance();
                send(String.valueOf(balance));
                break;
            default:
                send("Invalid option");
                break;
        }
    }
}
