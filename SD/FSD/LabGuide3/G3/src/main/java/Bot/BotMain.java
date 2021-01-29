package Bot;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

public class BotMain {
    public static void main(String[] args) {
        try {
            Socket server = new Socket("127.0.0.1", 12345);
            BufferedReader in = new BufferedReader(new InputStreamReader(server.getInputStream()));
            BufferedWriter out = new BufferedWriter(new OutputStreamWriter(server.getOutputStream()));
            if(args.length < 3) new Writter(out, "bot.\n", Integer.parseInt(args[0]), 100).start();
            else if (args.length == 3) new Writter(out, args[2] + "\n", Integer.parseInt(args[0]), 100).start();
            else if (args.length == 4) new Writter(out, args[2] + "\n", Integer.parseInt(args[0]), Integer.parseInt(args[3])).start();
            if (args.length == 4) new Reader(in, Integer.parseInt(args[1]), Integer.parseInt(args[3])).start();
            else new Reader(in, Integer.parseInt(args[1]), 100).start();
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}
