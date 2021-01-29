package ExampleUsage;

import API.API;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;

public class Example {

    public static void main(String[] args) throws IOException, InterruptedException, ExecutionException {
        // Insert some stuff
        Map<Long,byte[]> map = new HashMap<>();
        Map<Long,byte[]> map2 = new HashMap<>();

        Map<Long,byte[]> map3 = new HashMap<>();
        Map<Long,byte[]> map4 = new HashMap<>();

        for(int i = 0; i < 100; i++){
            map.put((long) i, ("teste_novo" + i).getBytes());
        }
        for(int i = 0; i < 100; i++){
            map3.put((long) i, ("chaves------teste_novo" + i).getBytes());
        }
        for(int i = 0; i < 100; i++){
            map4.put((long) i, ("chaves3---teste_novo" + i).getBytes());
        }
        for(int i = 10001; i<11000; i++) {
            map2.put((long) i, ("teste_do_map_2_" + i).getBytes());
        }
        new Thread(new Put(map, "m1")).start();
        new Thread(new Put(map4, "m2")).start();
        new Thread(new Put(map3, "m3")).start();

        new Thread(new Get(map.keySet(), "get")).start();

        Thread.sleep(Long.MAX_VALUE);
    }
}
