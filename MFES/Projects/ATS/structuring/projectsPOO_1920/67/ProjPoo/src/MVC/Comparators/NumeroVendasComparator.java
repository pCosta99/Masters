package MVC.Comparators;

import MVC.Models.BaseModels.Utilizador;

import java.util.Comparator;

public class NumeroVendasComparator  implements Comparator<Utilizador> {

    public int compare(Utilizador u1, Utilizador u2){
        if(u1.getNumeroEncomendas()<u2.getNumeroEncomendas())
            return 1;
        if(u1.getNumeroEncomendas()>u2.getNumeroEncomendas())
            return -1;
        return u1.getCod().compareTo(u2.getCod());
    }
}
