import java.io.Serializable;
import java.util.Comparator;

/**
 * Comparator usado para organizar por número de encomendas realizadas
 */
public class ComparatorQuantidadeEnc implements Comparator<RegistoTULV>, Serializable {
    /**
     * Compara o número de encomendas realizadas
     * @param v1 Utilizador
     * @param v2 Utilizador
     * @return Resultado invertido da comparação
     */
    public int compare(RegistoTULV v1, RegistoTULV v2) {
        Utilizador v1n = (Utilizador) v1;
        Utilizador v2n = (Utilizador) v2;
        return -Integer.compare(v1n.getEncsSize(),v2n.getEncsSize());
    }
}
