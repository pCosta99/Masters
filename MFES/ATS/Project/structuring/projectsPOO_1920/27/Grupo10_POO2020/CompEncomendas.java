import java.util.Comparator;
import java.io.Serializable;
/**
 * Compara duas encomendas pelo preço da encomenda. Se forem iguais, compara pelo código de encomenda.
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611 
 */

public class CompEncomendas implements Comparator<Encomenda>, Serializable
{
    public int compare(Encomenda a, Encomenda b){
        if(a.getPreco() < b.getPreco()) return 1;
        if(a.getPreco() > b.getPreco()) return -1;
        return a.compareTo(b);
    }
}
