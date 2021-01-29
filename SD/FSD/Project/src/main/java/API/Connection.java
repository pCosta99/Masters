package API;

import Protocol.*;
import spullara.nio.channels.FutureSocketChannel;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import static Protocol.GenericMessage.TYPE.*;

public class Connection {

    private final FutureSocketChannel socket;
    private final State state;
    private final int port;
    private final FutureSocketChannel coordinator;

    public Connection(FutureSocketChannel socket, State state, int port) throws IOException, ExecutionException, InterruptedException {
        this.socket = socket;
        this.state = state;
        this.port = port;
        coordinator = new FutureSocketChannel();
        coordinator.connect(new InetSocketAddress("localhost", 12399)).get();
    }

    public void receiver() {
        Protocol.read(socket).thenAccept(m -> {
            if(m.getType().equals(PUT)) {
                // Receive the values to put
                Map<Long,byte[]> putMap = (Map<Long, byte[]>) m.getValue();
                putMap.forEach((k,v) -> this.state.put(k,v,m.getClock()));
                GenericMessage<Integer> gm = new GenericMessage<>(PUT_DONE, port, m.getClock());
                Protocol.write(coordinator, gm).thenRun(this::receiver);
            } else {
                // Receive the values to lookup and send them back
                Map<Long,Integer> getList = (Map<Long,Integer>) m.getValue();

                Map<Long,byte[]> resp = new HashMap<>();
                getList.forEach((key,version) -> {
                    byte[] bytes = this.state.getValue(key,version);
                    if(bytes != null) resp.put(key,bytes);
                    else resp.put(key,"notFound".getBytes(StandardCharsets.UTF_8));
                });

                GenericMessage<Map<Long, byte[]>> msg = new GenericMessage<>(REPLY, resp);
                Protocol.write(socket, msg).thenRun(this::receiver);
            }
        });
    }
}