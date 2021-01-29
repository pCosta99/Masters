package Projeto.Exceptions;
/**
 * Classe que lança uma excepçao caso a entidade em questao nao exista.
 */
public class EntidadeNaoExisteException extends Exception {
    public EntidadeNaoExisteException(String msg) {
        super(msg);
    }
}
