import java.io.IOException;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;

public class Workload {

    private final Account account;
    private final Random rand;
    private static final int NR_REQUESTS = 10000;
    private List<Integer> ports;

    // Client Stub
    private final Stub stub;

    public Workload(Account account, Random rand, List<Integer> ports) throws IOException, ExecutionException, InterruptedException {
        this.account = account;
        this.rand = rand;
        this.stub = new Stub(ports);
    }

    public void start() throws IOException, ExecutionException, InterruptedException {
        for(int i = 0; i < NR_REQUESTS; i++){
            Thread.sleep(1);
            System.out.println("Operation Id: " + i);
            // Generate a int value
            int value = 1 + rand.nextInt((400 - 1) + 1);

            switch (rand.nextInt(2)){
                // Deposit operation
                case 0:
                    boolean depositResult = stub.movement(value);
                    if (depositResult) account.deposit(value);
                    break;
                // Withdraw operation
                case 1:
                    boolean withdrawResult = stub.movement(-value);
                    if (withdrawResult) account.withdraw(value);
                    break;
            }
        }

        // Compare server and local account balance
        compareBalance();
    }

    private void compareBalance() throws IOException, ExecutionException, InterruptedException {
        int serverBalance = stub.balance();

        if (serverBalance == account.balance()){
            System.out.println("\n\n> Server and client side balance is equal.\n" +
                    "\t-> Server Side Balance: " + serverBalance + "\n" +
                    "\t-> Client Side Balance: " + account.balance());
        }else{
            System.err.println("\n\n> Server and client side balance is different.\n" +
                    "\t-> Server Side Balance: " + serverBalance + "\n" +
                    "\t-> Client Side Balance: " + account.balance());
        }
    }
}
