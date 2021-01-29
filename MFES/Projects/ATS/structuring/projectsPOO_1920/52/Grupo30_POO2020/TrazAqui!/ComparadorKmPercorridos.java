

import java.io.Serializable;
import  java.util.*;

public class ComparadorKmPercorridos implements Comparator<Empresa>, Serializable {

    public int compare(Empresa e1, Empresa e2){
        double km1 = 0;
        double km2 = 0;
        for(RealizadaEmpresa re: e1.getRe())
            km1 =+ re.getDistanciaViagem();
        for(RealizadaEmpresa rv: e2.getRe())
            km2 =+ rv.getDistanciaViagem();

        if (km1>km2) return  -1;
        else return 1;
    }
}