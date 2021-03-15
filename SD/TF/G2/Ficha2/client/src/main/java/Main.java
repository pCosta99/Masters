import spread.SpreadException;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

public class Main {

    public static void main(String[] args) throws IOException, ExecutionException, InterruptedException, SpreadException {

        // Create an account
        Account account = new Account(0);

        List<Integer> servers = Arrays.stream(args).map(Integer::parseInt).collect(Collectors.toList());

        Workload workload = new Workload(account, new Random(), servers);

        System.out.println("\n\n> Workload Start...");
        workload.start();
    }
}
