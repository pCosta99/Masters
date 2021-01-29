package Model;

import Model.Encomendas.IEncomenda;

import java.io.Serializable;
import java.time.LocalTime;
import java.util.Comparator;

public class CompareEncomenda implements Comparator<IEncomenda>, Serializable {
    public int compare(IEncomenda e1, IEncomenda e2){
            LocalTime t1 = e1.getHoraInicial();
            LocalTime t2 = e2.getHoraInicial();
            int res = t1.compareTo(t2);
            if(res>0) return -1;
            else if(res<0) return 1;
            else return 0;
        }
}
