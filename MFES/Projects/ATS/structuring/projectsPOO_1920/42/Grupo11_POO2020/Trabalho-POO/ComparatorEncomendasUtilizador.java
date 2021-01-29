import java.io.Serializable;
import java.util.Comparator;

public class ComparatorEncomendasUtilizador implements Comparator<Utilizador>, Serializable {

    public int compare(Utilizador d1, Utilizador d2) {
        int cont1 = 0, cont2 = 0;
        for(Encomenda e: d1.getEncomendas()){
            if(e.getaceC())
            cont1++;
        }
        for(Encomenda e: d2.getEncomendas()){
            if(e.getaceC())
                cont2++;
        }

        if(cont1  > cont2)
            return -1;
        else if(cont1 == cont2)
            return 0;
        else
            return 1;
    }
}
