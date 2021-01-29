package Projeto.Exceptions;
/**
 * Classe que lança uma excepçao caso haja um id repetido.
 */
public class IdRepetidoException extends Exception{
    public IdRepetidoException(String msg) {
        super(msg);
    }
}