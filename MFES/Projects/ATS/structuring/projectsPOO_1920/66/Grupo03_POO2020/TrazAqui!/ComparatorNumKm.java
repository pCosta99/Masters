import java.io.Serializable;
import java.util.Comparator;

/**
 * Comparator usado para organizar por km
 */
public class ComparatorNumKm implements Comparator<RegistoTULV>, Serializable{
    /**
     * Compara os km de duas Transportadoras
     * @param v1 Transportadoras
     * @param v2 Transportadoras
     * @return Resultado invertido da comparação
     */
    public int compare(RegistoTULV v1, RegistoTULV v2) {
        Transportadora v1n = (Transportadora) v1;
        Transportadora v2n = (Transportadora) v2;
        return -Double.compare(v1n.getNumKm(),v2n.getNumKm());
    }
}
