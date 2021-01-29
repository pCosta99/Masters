
import java.io.Serializable;
import java.util.Comparator;



public class ComparatorNumeroEncUti implements Comparator<Utilizador>
{
    public int compare(Utilizador a,Utilizador b){
        if(a.getNumeroEncomendas() < b.getNumeroEncomendas()) return 1;
        if(a.getNumeroEncomendas() > b.getNumeroEncomendas()) return -1;
        return a.getCodigo().compareTo(b.getCodigo());
    }
}