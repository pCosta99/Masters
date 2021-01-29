package API;

import spullara.nio.channels.FutureSocketChannel;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Connection {

    private final FutureSocketChannel socket;
    private final State state;

    public Connection(FutureSocketChannel socket, State state) {
        this.socket = socket;
        this.state = state;
    }

    public void receiver() {
        // Get the message size
        ByteBuffer buffSize = ByteBuffer.allocate(4);
        socket.read(buffSize).thenRun(() -> {
            ByteBuffer buff = ByteBuffer.allocate(buffSize.flip().getInt());
            // Read the message with the content
            socket.read(buff).thenRun(() -> {
                buff.flip();
                GenericMessage m = new GenericMessage(buff);
                if (m.isPut()) {
                    // Receive the values to put
                    Map<Long,byte[]> putMap = (Map<Long, byte[]>) m.getValue();
                    putMap.forEach((k,v) -> this.state.put(k,v,m.getClock()));
                    this.state.updateClock(m.getClock());
                    receiver();
                } else {
                    // Receive the values to lookup and send them back
                    List<Long> getList = (List<Long>) m.getValue();

                    Map<Long,byte[]> resp = new HashMap<>();
                    getList.forEach(key -> {
                        byte[] bytes = this.state.getValue(key);
                        if(bytes != null) resp.put(key,bytes);
                        else resp.put(key,"notFound".getBytes(StandardCharsets.UTF_8));
                    });

                    GenericMessage<Map<Long, byte[]>> msg = new GenericMessage<>("resp", resp);
                    ByteBuffer bb = msg.serialize();
                    this.socket.write(bb).thenRun(() -> {
                        bb.clear();
                        receiver();
                    });
                }
            buffSize.clear();
            buff.clear();
            });
        });
    }
}