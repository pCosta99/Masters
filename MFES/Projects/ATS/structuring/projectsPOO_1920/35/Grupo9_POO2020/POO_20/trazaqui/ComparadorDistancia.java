import java.util.Comparator;


public class ComparadorDistancia implements Comparator<EmpresaTransp> {
    
    public int compare (EmpresaTransp e1, EmpresaTransp e2) {
        double totalE1 = e1.getKms();
        double totalE2 = e2.getKms();
        
        if(totalE1 < totalE2)
            return -1;
        else if(totalE1 == totalE2)
            return 0;
        else
            return 1;
    }
}