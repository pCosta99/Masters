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
 * Comparator entre as Transportadoras para ver qual e o que faz mais encomendas 
 */
public class ComparaEncomendasT implements Comparator<Transportadora>{
    
  
    public int compare(Transportadora a, Transportadora b){
        if(a.getKmsTotal() >= b.getKmsTotal()) return -1;
        if(a.getKmsTotal() < b.getKmsTotal()) return 1;
        return 0;
    }
}

