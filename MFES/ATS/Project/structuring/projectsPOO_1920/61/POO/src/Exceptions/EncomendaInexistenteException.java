package Exceptions;

public class EncomendaInexistenteException extends Exception {
    public EncomendaInexistenteException() {
        super();
    }

    public EncomendaInexistenteException(String message) {
        super(message);
    }
}
