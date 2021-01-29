package Model.Comparators;


import Model.Atores.Transportadores.EmpresaTransportadora;

import java.util.Comparator;
import java.io.Serializable;

    public class KmComparator implements Comparator <EmpresaTransportadora>,Serializable
    {
        public int compare(EmpresaTransportadora e1,EmpresaTransportadora e2){
            if (e1.getNumeroKms()>e2.getNumeroKms())return -1;
            if (e1.getNumeroKms()<e2.getNumeroKms()) return 1;
            return 0;
        }

    }

