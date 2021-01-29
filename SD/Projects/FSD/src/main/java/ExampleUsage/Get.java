package ExampleUsage;

import API.API;

import java.io.IOException;
import java.util.Collection;
import java.util.concurrent.ExecutionException;

public class Get implements Runnable {
    API api;
    Collection<Long> keys;
    String msg;

    public Get(Collection<Long> keys, String msg) throws IOException, ExecutionException, InterruptedException {
        this.api = new API();
        this.keys = keys;
        this.msg = msg;
    }

    public void run() {
        try {
            api.get(this.keys).thenAccept(v -> {
                v.forEach((k,val) -> {
                    System.out.println(this.msg + ":" + k + ": " + new String(val));
                });
            });
        } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}
