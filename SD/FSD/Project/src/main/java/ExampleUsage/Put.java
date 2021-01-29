package ExampleUsage;

import API.API;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ExecutionException;

public class Put implements Runnable {
    API api;
    Map<Long,byte[]> map;
    String doneMsg;

    public Put(Map<Long,byte[]> map, String doneMsg) throws IOException, ExecutionException, InterruptedException {
        this.api = new API();
        this.map = map;
        this.doneMsg = doneMsg;
    }

    public void run() {
        try {
            api.put(this.map).thenRun(() -> System.out.println(this.doneMsg));
        } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}
