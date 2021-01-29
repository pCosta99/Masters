package src.model;

import java.util.Comparator;

/**
 * Classe de comparação entre transportadoras
 */
public class ComparatorTransKms implements Comparator<Transportadora> {
    /**
     * Compara duas isntâncias de transportadoras a partir dos kilometros percorridos por cada uma delas
     * @param t1 Transportadora a ser comparada
     * @param t2 Transportadora a ser comparada
     * @return interio com o resultado da comparação
     */
    public int compare(Transportadora t1, Transportadora t2) {
    	double ent1,ent2;
    	ent1 = t1.getKmsPercorridos();
    	ent2 = t2.getKmsPercorridos();
        
        if (ent1 > ent2) {
        	return 1;
        }

        else if (ent2 > ent1) {
        	return -1;
        }
        return t1.getUsername().compareTo(t2.getUsername());
    }
}
