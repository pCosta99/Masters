/**
 * Write a description of class ComparatorEmpresaKms here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 1 - 10-06-2020
 */

import java.util.Comparator;

public class ComparatorEmpresaKms implements Comparator<EmpresaTransportadora>
{
    public int compare (EmpresaTransportadora e1, EmpresaTransportadora e2)
    {   if (e1.getNumKmsPercorridos() > e2.getNumKmsPercorridos()) return 1;
        if (e1.getNumKmsPercorridos() < e2.getNumKmsPercorridos()) return -1;
        return e1.getCodigoE().compareTo(e2.getCodigoE());
    }
}