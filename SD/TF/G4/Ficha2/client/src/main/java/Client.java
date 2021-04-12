import spread.SpreadException;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

public class Client {

    public static void main(String[] args) throws IOException, ExecutionException, InterruptedException, SpreadException {

        // Create an account
        Account account = new Account(0);

        Workload workload = new Workload(account, new Random());

        System.out.println("\n\n> Workload Start...");
        workload.start();
    }
}
