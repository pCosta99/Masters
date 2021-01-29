
/**
 * Escreva a descrição da classe ComparatorKmsPercorridos aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.Comparator;
import java.io.*;
public class ComparatorKmsPercorridos implements Comparator<EmpresaTransportadora>,Serializable {
    public int compare(EmpresaTransportadora a1, EmpresaTransportadora a2) {
      return a2.getNumKms()-a1.getNumKms();
    }
}
