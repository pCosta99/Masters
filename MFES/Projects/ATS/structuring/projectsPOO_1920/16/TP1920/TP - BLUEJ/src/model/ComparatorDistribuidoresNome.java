package src.model;

import java.util.Comparator;





/**
 * Classe de comparação entre distribuidores
 */
public class ComparatorDistribuidoresNome implements Comparator<Distribuidor> {
	/**
	 * Compara as duas instâncias de distribuidor a partir do nome de cada uma delas
	 * @param d1 distribuidora a ser comparada
	 * @param d2 distribuidora a ser comparada
	 * @return retorna o nome com resultado da comparacao
	 */
    public int compare(Distribuidor d1, Distribuidor d2) {
        return d1.getNome().compareTo(d2.getNome());
    }
}



