package Model;

import java.util.Comparator;

/**
 * Classe que compara o numero de Encomendas realizadas entre dois utilizadores
 */

public class ComparatorNumeroEncomendas implements Comparator<User> {

    public int compare ( User e1, User e2){
        if ((e1.getEncFeitas().size() < e2.getEncFeitas().size())  ) return 1;
        if ((e1.getEncFeitas().size() > e2.getEncFeitas().size())  ) return -1;
        return e1.getNome().compareTo(e2.getNome());
    }

}