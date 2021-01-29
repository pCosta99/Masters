package exceptions;

public class NomeInvalidoException extends Exception{
    public NomeInvalidoException() { super("O nome não é válido"); }
    public NomeInvalidoException(String message) {
        super(message);
    }
}
