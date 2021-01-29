package Model;

import java.util.Comparator;

/**
 * Classe que compara os kms feitos entre duas Empresas Transportadoras
 */

public class ComparatorKmsFeitos implements Comparator<EmpresasTrans> {

    public int compare ( EmpresasTrans e1, EmpresasTrans e2){
        if ((e1.getNrKmsFeitos() < e2.getNrKmsFeitos())  ) return 1;
        if ((e1.getNrKmsFeitos() > e2.getNrKmsFeitos())  ) return -1;
        return e1.getNome().compareTo(e2.getNome());
    }

}