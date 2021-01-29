package exceptions;

public class EmailInexistenteException extends Exception{
    public EmailInexistenteException() { super("O email introduzido não se encontra registado!"); }

    public EmailInexistenteException(String message) { super(message);}
}
