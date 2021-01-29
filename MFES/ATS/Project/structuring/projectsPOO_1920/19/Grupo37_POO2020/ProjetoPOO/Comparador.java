import java.util.Comparator;
import java.io.Serializable;

public class Comparador implements Comparator<Cliente>, Serializable{
    public int compare(Cliente a, Cliente b){
        int r = 0;
        if(a.getNdeEnc() < b.getNdeEnc()) {
            return 1;
        }
        else if(a.getNdeEnc() > b.getNdeEnc()){
            return -1;
        }
        return 0;
    }
}
