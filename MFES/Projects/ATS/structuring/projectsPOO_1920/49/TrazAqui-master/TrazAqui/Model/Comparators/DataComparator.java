package Model.Comparators;

import Model.Encomenda;

import java.io.Serializable;
import java.util.Comparator;

public class DataComparator implements Comparator<Encomenda>, Serializable {

    public int compare(Encomenda e1, Encomenda e2){

        if(e1.getData().isBefore(e2.getData())) return 1;
        if(e2.getData().isBefore(e1.getData())) return -1;
        return 0;

    }
}
