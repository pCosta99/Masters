import java.util.Comparator;


public class ComparadorUtilizacao implements Comparator<Cliente> {
    
    public int compare (Cliente c1, Cliente c2) {
        int totalC1 = 0;
        int totalC2 = 0;
        for(Encomenda a : c1.getHist()){
            totalC1 ++;
        }
        for(Encomenda b : c2.getHist()){
            totalC2 ++;
        }
        if(totalC1 < totalC2)
            return -1;
        else if(totalC1 == totalC2)
            return 0;
        else
            return 1;
    }
}