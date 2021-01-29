package Server;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

public class Controller {
    private List<PrintWriter> pwList;

    public Controller(){
        this.pwList = new ArrayList<>();
    }

    public Controller(List<PrintWriter> pwList){
        this.pwList = pwList;
    }

    public void addWriter(Socket client) throws IOException {
        PrintWriter pw = new PrintWriter(client.getOutputStream());
        pwList.add(pw);
    }

    public void closeWriter(Socket sc){
        this.pwList.remove(sc);
    }

    public void broadcast(String s){
        pwList.forEach(w -> {
            w.println(s);
            w.flush();
        });
    }
}
