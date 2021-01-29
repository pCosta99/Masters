package Model;

import java.io.Serializable;
import java.util.Comparator;

public class ComparatorUtilizador implements Comparator<Utilizador>, Serializable {

    public int compare(Utilizador u1, Utilizador u2){
        int size1 =(int) u1.getEntregues().stream().filter(Encomenda::getAceites).count();
        int size2 =(int) u2.getEntregues().stream().filter(Encomenda::getAceites).count();
        if(size1<size2) return 1;
        else if(size1>size2) return -1;
        else return u1.getCod().compareTo(u2.getCod());
    }
}
