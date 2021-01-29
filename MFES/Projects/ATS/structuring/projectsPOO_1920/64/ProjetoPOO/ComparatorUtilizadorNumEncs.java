/**
 * Write a description of class ComparatorUtilizadorNumEncs here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 1 - 10-06-2020
 */

import java.util.Comparator;

public class ComparatorUtilizadorNumEncs implements Comparator<UtilizadorTrazAqui>
{
    public int compare (UtilizadorTrazAqui u1, UtilizadorTrazAqui u2)
    {   if (u1.getNumEncs() > u2.getNumEncs()) return 1;
        if (u1.getNumEncs() < u2.getNumEncs()) return -1;
        return u1.getCodigoU().compareTo(u2.getCodigoU());
    }
}