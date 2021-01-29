package exceptions;

public class EmailJaExisteException extends Exception {
    public EmailJaExisteException() { super("O email introduzido já existe!"); }

    public EmailJaExisteException(String message) {
        super(message);
    }
}
