import protocol.Protocol;
import spread.SpreadConnection;
import spread.SpreadException;
import spread.SpreadMessage;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class Stub implements BankInterface {
    private final SpreadConnection conn;
    private final Map<Integer, CompletableFuture<String>> cfs;
    private Integer globalCounterPut;
    private Integer globalCounterGet;

    public Stub() throws UnknownHostException, SpreadException {
        cfs = new HashMap<>();
        globalCounterPut = 0;
        globalCounterGet = 0;
        // Create connection
        conn = new SpreadConnection();
        conn.connect(InetAddress.getByName("localhost"), 4803, "client1", false, false);
        conn.add(req -> {
            String data = new String(req.getData());
            if(!cfs.containsKey(globalCounterGet)) System.out.println("Key doesn't exists yet");
            // Complete the future waiting with the obtained message
            cfs.get(globalCounterGet++).complete(data);
        });
    }

    private CompletableFuture<String> send(String message) throws SpreadException {
        CompletableFuture<String> cf = new CompletableFuture<>();

        // Add completable future to map and return it
        cfs.put(globalCounterPut++, cf);

        // Send the message
        Protocol.send(message, "servers", conn);

        return cf;
    }

    public int balance() throws SpreadException, ExecutionException, InterruptedException {
        return send("balance ").thenCompose(balance -> {
            CompletableFuture<Integer> cfi = new CompletableFuture<>();
            System.out.println("Balance: " + balance);
            cfi.complete(Integer.parseInt(balance.replaceAll("[^-0-9]+", "")));
            return cfi;
        }).get();
    }

    public boolean movement(int value) throws SpreadException, ExecutionException, InterruptedException {
        return send("movement " + value).thenCompose(operationResult -> {
            CompletableFuture<Boolean> cfb = new CompletableFuture<>();
            System.out.println("Operation Result: " + operationResult);
            String booleanStr = operationResult.replaceAll("[^a-z]+","");
            cfb.complete(booleanStr.equals("true"));
            return cfb;
        }).get();
    }

    public void close() throws SpreadException {
        conn.disconnect();
        System.out.println(conn.isConnected());
    }
}
