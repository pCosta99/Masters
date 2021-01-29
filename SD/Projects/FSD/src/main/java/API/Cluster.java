package API;

public class Cluster {

    public static void main(String[] args) throws InterruptedException {

        new Thread(new Coordinator(12399)).start();

        for(int i=12340; i < 12349; i++) new Thread(new Server(i)).start();

        Thread.sleep(Integer.MAX_VALUE);
    }
}
