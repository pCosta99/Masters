package API;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

public class State {
    private final Map<Long,Map<Integer,byte[]>> data;
    private final ReentrantLock lock;

    public State(){
        this.data = new ConcurrentHashMap<>();
        lock = new ReentrantLock();
    }

    public byte[] getValue(Long key, int version) {
        this.lock.lock();
        Map<Integer,byte[]> values = this.data.get(key);
        if(values == null) return null;
        byte[] ret = values.get(version);

        // Delete previous entries
        garbageCollect(key, version);

        this.lock.unlock();
        return ret;
    }

    public void put(Long key, byte[] data, Integer clock){
        this.lock.lock();
        if(!this.data.containsKey(key)) {
            this.data.put(key,new ConcurrentHashMap<>());
        }
        this.data.get(key).put(clock,data);
        this.lock.unlock();
    }

    private void garbageCollect(Long key, Integer versionID){
        data.get(key).keySet().forEach(v -> {
            if( v < versionID ) data.get(key).remove(v);
        });
        System.out.println(key + " : " + data.get(key).keySet());
    }
}
