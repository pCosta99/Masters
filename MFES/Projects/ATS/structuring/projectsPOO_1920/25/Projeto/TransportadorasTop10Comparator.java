
/**
 * Escreva a descrição da classe UtilizadoresTop10Comparator aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.Comparator;
import java.io.Serializable;
public class TransportadorasTop10Comparator implements Comparator<Transporte>, Serializable
{
    public int compare(Transporte t1, Transporte t2){
        EmpresasTransportadoras ep1 = (EmpresasTransportadoras) t1;
        EmpresasTransportadoras ep2 = (EmpresasTransportadoras) t2;
        double NumKms1 = ep1.getNumKms();
        double NumKms2 = ep2.getNumKms();
        if (NumKms1 < NumKms2) return 1;
        if (NumKms1 > NumKms2) return -1;
        return 0;
    }
}
