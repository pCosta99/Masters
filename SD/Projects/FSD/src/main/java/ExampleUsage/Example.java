package ExampleUsage;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

public class Example {

    public static void main(String[] args) throws IOException, InterruptedException, ExecutionException {
        // Insert some stuff
        Map<Long,byte[]> map = new HashMap<>();
        Map<Long,byte[]> map2 = new HashMap<>();
        Map<Long,byte[]> map3 = new HashMap<>();
        List<Long> keys = new ArrayList<>();

        for(int i = 0; i < 1000; i++){
            map.put((long) i, ("nao_e_um_teste" + i).getBytes());
            keys.add((long) i);
        }
        for(int i = 0; i < 1000; i++){
            map2.put((long) i, ("chaves------teste_novo" + i).getBytes());
        }
        for(int i = 0; i < 1000; i++){
            map3.put((long) i, ("chaves3---teste_novo" + i).getBytes());
        }

        new Thread(new Put(map, "m1")).start();
        new Thread(new Put(map2, "m2")).start();
        new Thread(new Put(map3, "m3")).start();

        new Thread(new Get(keys, "get")).start();

        Thread.sleep(Long.MAX_VALUE);
    }
}
