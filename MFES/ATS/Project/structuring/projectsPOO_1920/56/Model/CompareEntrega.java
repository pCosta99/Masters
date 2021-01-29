package Model;


import Model.Encomendas.IEncomenda;
import Model.Encomendas.IEntrega;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Comparator;

public class CompareEntrega implements Comparator<IEntrega>, Serializable {
    public int compare(IEntrega e1, IEntrega e2){
        int comparison = 0;
        comparison = compareTime(e1,e2);
        if (comparison == 0){
            comparison = compareDate(e1,e2);
        }
        return comparison;
    }

    public int compareTime(IEntrega e1, IEntrega e2){
        LocalTime t1 = e1.getEncomenda().getHoraInicial();
        LocalTime t2 = e2.getEncomenda().getHoraInicial();
        int res = t1.compareTo(t2);
        if(res>0) return -1;
        else if(res<0) return 1;
        else return 0;
    }
    public int compareDate(IEntrega e1, IEntrega e2){
        LocalDate t1 = e1.getDataEntrega();
        LocalDate t2 = e2.getDataEntrega();
        int res = t1.compareTo(t2);
        if(res>0) return -1;
        else if(res<0) return 1;
        else return 0;
    }

}
