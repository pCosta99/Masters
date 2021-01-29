package src.model;

import java.util.Comparator;

/**
 * Classe de comparação entre lojas
 */
public class ComparatorLojaNome implements Comparator<Loja> {
/**
 * Compara duas isntâncias de Loja a partir do nome associado a cada uma delas
 * @param l1 loja a ser comparada
 * @param l2 loja a ser comparada
 * @return interio com o resultado da comparação
 */
    public int compare(Loja l1, Loja l2) {
        return l1.getNome().compareTo(l2.getNome());
    }
}
