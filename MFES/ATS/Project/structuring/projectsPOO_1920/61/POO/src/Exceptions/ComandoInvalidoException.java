package Exceptions;

public class ComandoInvalidoException extends Exception{
    public ComandoInvalidoException() {
        super();
    }

    public ComandoInvalidoException(String message) {
        super(message);
    }
}
