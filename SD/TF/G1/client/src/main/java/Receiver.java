import protocol.Protocol;
import spullara.nio.channels.FutureSocketChannel;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

public class Receiver implements Runnable {

    private final FutureSocketChannel socket;
    private final Map<Integer, CompletableFuture<String>> cfs;
    private Integer count;

    public Receiver(FutureSocketChannel socket, Map<Integer, CompletableFuture<String>> cfs) {
        this.socket = socket;
        this.cfs = cfs;
        count = 0;
    }

    public void receiver(){
        Protocol.read(socket).thenAccept(str -> {
            cfs.get(count++).complete(str);
            receiver();
        });
    }

    @Override
    public void run() {
        receiver();
    }
}
