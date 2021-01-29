package Models.Comparators;


import Models.Utilizador;

import java.util.Comparator;

public class UserComparatorByOrders implements Comparator<Utilizador> {

    public int compare(Utilizador a, Utilizador b) {
        int x, y;
        x = a.getnEnc();
        y = b.getnEnc();
        if (x == y)
            return a.getCod().compareTo(b.getCod()); //caso tenham o mesmo nº de encomendas aplica a Ordem Natural das Strings

        else {
            return y - x;
        }
    }
}
