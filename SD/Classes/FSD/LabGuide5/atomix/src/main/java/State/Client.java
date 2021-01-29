package State;

import spullara.nio.channels.FutureSocketChannel;

public class Client {
    private FutureSocketChannel socket;
    private int messageID;      //Last sent message

    public Client(FutureSocketChannel fsc, int messageID){
        this.socket = fsc;
        this.messageID = messageID;
    }

    public FutureSocketChannel getSocket(){
        return this.socket;
    }

    public int getMessageID(){
        return messageID;
    }

    public void incrementMessageID() {
        messageID++;
    }
}
