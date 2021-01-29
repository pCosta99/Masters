package src.model;

import java.util.Comparator;

/**
 * Classe de comparação entre Utilizadores
 */
public class ComparatorUtilizadoresEntregas implements Comparator<Utilizador> {
    /**
     * Compara duas isntâncias de utilizadores a partir do número de entregas recebidas por cada uma delas
     * @param u1 utilizador a ser comparado
     * @param u2 utilizador a ser comparado
     * @return interio com o resultado da comparação
     */
    public int compare(Utilizador u1, Utilizador u2) {
    	int ent1,ent2;
    	ent1 = u1.getHistoricoEntregas().size();
    	ent2 = u2.getHistoricoEntregas().size();
        
        if (ent1 > ent2) {
        	return 1;
        }

        else if (ent2 > ent1) {
        	return -1;
        }
        return u1.getUsername().compareTo(u2.getUsername());
    }
}
