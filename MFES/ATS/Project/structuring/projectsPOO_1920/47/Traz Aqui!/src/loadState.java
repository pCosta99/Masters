import java.io.*;
import java.util.concurrent.locks.ReentrantLock;

public class loadState extends Thread{
    private final ReentrantLock reentrantLock = new ReentrantLock();
    public loadState(){}

    public trazAqui parser() {
       trazAqui it;
        reentrantLock.lock();
        try{
            ObjectInputStream in = new ObjectInputStream(new FileInputStream("trazAqui.obj"));
           it = (trazAqui) in.readObject();
           in.close();
       } catch (Exception e) {
           System.out.println("Impossivel aceder estado anterior. Ficheiro corrompido ou inexistente.\n Carregando logs originais.");
           Parse parser = new Parse();
           it = parser.parse();
       }
        reentrantLock.unlock();
        return it;
    }

}
