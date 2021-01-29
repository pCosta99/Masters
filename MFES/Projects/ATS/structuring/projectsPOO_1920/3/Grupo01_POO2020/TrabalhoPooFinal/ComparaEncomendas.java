import java.util.Comparator;
/**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
/**
 * Comparator entre os Utilizadores para ver qual e o que faz mais encomendas 
 */
public class ComparaEncomendas implements Comparator<Utilizador>{
  
    public int compare(Utilizador a, Utilizador b){
        if(a.getNencomendas().size() >= b.getNencomendas().size()) return -1;
        if(a.getNencomendas().size()<b.getNencomendas().size()) return 1;
        return 0;
    }
}
