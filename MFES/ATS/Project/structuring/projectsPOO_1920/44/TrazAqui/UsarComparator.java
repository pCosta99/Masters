import java.util.Comparator;

public class UsarComparator implements Comparator<Account>{
    public int compare(Account a1,Account a2){
        if(a1.qtsClientes() > a2.qtsClientes()) return -1;
        if(a1.qtsClientes() < a2.qtsClientes()) return 1;
        return a1.compareTo(a2);
    }
}