package Projeto.Exceptions;
/**
 * Classe que lança uma excepçao caso uma lista qualquer se encontre vazia.
 */
public class ListaVaziaException extends Exception {
    public ListaVaziaException(String msg) {
        super(msg);
    }
}
