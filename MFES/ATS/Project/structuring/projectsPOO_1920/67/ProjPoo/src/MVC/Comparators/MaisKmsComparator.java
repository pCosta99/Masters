package MVC.Comparators;

import MVC.Models.BaseModels.Transportadora;

import java.util.Comparator;

public class MaisKmsComparator implements  Comparator<Transportadora> {

    public int compare(Transportadora t1, Transportadora t2){
        if(t1.getKmsTotal()<t2.getKmsTotal())
            return 1;
        if(t1.getKmsTotal()>t2.getKmsTotal())
            return -1;
        return t1.getCod().compareTo(t2.getCod());
    }

}
