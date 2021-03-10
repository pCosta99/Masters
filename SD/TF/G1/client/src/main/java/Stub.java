import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import protocol.Protocol;
import spullara.nio.channels.FutureSocketChannel;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicReference;

public class Stub implements BankInterface{
    private final List<Integer> ports;
    private final Map<Integer,FutureSocketChannel> servers;
    private final Map<Integer,CompletableFuture<String>> cfs;
    private final Map<Integer,CompletableFuture> writeComposer;
    private Integer globalCounter;

    public Stub(List<Integer> servers) throws IOException, ExecutionException, InterruptedException {
        System.out.println("> Client connected to server!");
        // Init variables
        this.ports = servers;
        this.servers = new HashMap<>();
        writeComposer = new HashMap<>();
        cfs = new HashMap<>();
        globalCounter = 0;
        // Create connection for each socket and run the message receiving threads
        for(Integer i: servers) {
            System.out.println("Connecting to " + i);
            FutureSocketChannel sc = new FutureSocketChannel();
            this.servers.put(i, sc);
            writeComposer.put(i, sc.connect(new InetSocketAddress("localhost",i)));
            new Thread(new Receiver(sc,cfs)).start();
        }
    }

    // Broadcasts a message to all servers
    public CompletableFuture<String> send(String message){
        CompletableFuture<String> completableFut = new CompletableFuture<>();
        this.cfs.put(globalCounter++, completableFut);
        for(Integer port : ports){
            FutureSocketChannel fsc = servers.get(port);
            Protocol.send(fsc, message);
            //writeComposer.get(port).thenCompose(ignore -> fsc.write(sizeBuff)).thenCompose(ignore2 -> fsc.write(buff.duplicate()));
        }
        return completableFut;
    }

    @Override
    public int balance() throws InterruptedException, ExecutionException {
        return send("balance ").thenCompose(balance -> {
            CompletableFuture<Integer> cfi = new CompletableFuture<>();
            System.out.println("Balance: " + balance);
            cfi.complete(Integer.parseInt(balance.replaceAll("[^-0-9]+", "")));
            return cfi;
        }).get();
    }

    @Override
    public boolean movement(int value) throws InterruptedException, ExecutionException {
        return send("movement " + value).thenCompose(operationResult -> {
            CompletableFuture<Boolean> cfb = new CompletableFuture<>();
            System.out.println("Operation Result: " + operationResult);
            String booleanStr = operationResult.replaceAll("[^a-z]+","");
            cfb.complete(booleanStr.equals("true"));
            return cfb;
        }).get();
    }
}
