package Models.Comparators;

import Models.EmpresaTransportadora;

import java.util.Comparator;

public class EmpresasComparatorByDistance implements Comparator<EmpresaTransportadora> {

    public int compare(EmpresaTransportadora a, EmpresaTransportadora b) {
        double x, y;
        x = a.getDistancia_percorrida();
        y = b.getDistancia_percorrida();

        if (x == y)
            return a.getCodigo().compareTo(b.getCodigo()); //se a distancia percorrida for a mesma usa a ordem natural das Strings

        double res = y - x;
        return (int) res;


    }
}
