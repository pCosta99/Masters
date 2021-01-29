package API;

import spullara.nio.channels.FutureSocketChannel;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class State {
    private final Map<Long,Map<Integer,byte[]>> data;
    private final Map<Integer,Clock> vectorClock;  //one clock per server
    private final Integer port;
    private final List<FutureSocketChannel> peers;


    public State(Integer port, List<Integer> peers) throws IOException {
        this.data = new ConcurrentHashMap<>();
        this.vectorClock = new ConcurrentHashMap<>();
        this.port = port;
        this.peers = new ArrayList<>();
        for(int i=0; i<peers.size(); i++) {
            FutureSocketChannel fs = new FutureSocketChannel();
            fs.connect(new InetSocketAddress("localhost", peers.get(i)+10));
            this.peers.add(fs);
        }

    }

    public void updateClock(Integer operation)  {
        this.vectorClock.get(this.port).setDone(operation);
        for(int i=0; i < this.peers.size(); i++) {
            ByteBuffer op = ByteBuffer.allocate(4);
            this.peers.get(i).write(op.putInt(operation).flip());
        }
    }

    public byte[] getValue(Long key) {
        Map<Integer,byte[]> values = this.data.get(key);
        Integer k = values.keySet().stream().max(Integer::compare).stream().collect(Collectors.toList()).get(0);

        return values.get(k);
    }

    public void put(Long key, byte[] data, Integer clock){
        if(!this.data.containsKey(key)) {
            this.data.put(key,new ConcurrentHashMap<>());
        }
        this.data.get(key).put(clock,data);
    }
}
