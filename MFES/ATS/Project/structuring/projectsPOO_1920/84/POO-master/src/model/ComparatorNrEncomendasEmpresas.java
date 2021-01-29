package model;

import java.util.Comparator;

public class ComparatorNrEncomendasEmpresas implements Comparator<EmpresaTransporte> {

    public int compare(EmpresaTransporte e1, EmpresaTransporte e2){
        int a = e1.numeroEncomendas();
        int b = e2.numeroEncomendas();

        if(a == b ) return 0;
        if(a > b) return -1;
        if(a < b) return 1;
        return 0;
    }
}