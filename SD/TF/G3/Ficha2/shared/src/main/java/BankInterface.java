import spread.SpreadException;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

public interface BankInterface {

    // Consult Balance operation
    int balance() throws IOException, InterruptedException, ExecutionException, SpreadException;

    // Movement operation (deposit / withdraw) given a value (positive / negative)
    boolean movement(int value) throws IOException, InterruptedException, ExecutionException, SpreadException;
}
