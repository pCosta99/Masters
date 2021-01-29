package Comparadores;

import Users.Utilizador;

import java.io.Serializable;
import java.util.Comparator;

public class QtdEncomendasReverseComparator implements Comparator<Utilizador>, Serializable {
    /**
     * Método que comparada dois Utilizadores pelo total de encomendas feitas.
     * @param u a ser comparado.
     * @param t1 a ser comparado.
     * @return int com o resultado da comparação.
     */
    @Override
    public int compare(Utilizador u, Utilizador t1) {
        if(t1.getEncomendasFeitas().size() > u.getEncomendasFeitas().size()) return 1;
        else if(t1.getEncomendasFeitas().size() < u.getEncomendasFeitas().size()) return -1;
        else return u.getCodigo().compareTo(t1.getCodigo());
    }
}
