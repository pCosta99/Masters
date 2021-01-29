package Exceptions;

/** Exceção para inputs inválidos. */
public class InvalidInputException extends Exception {
    private static final long serialVersionUID = 21L;
    
    /**
     * Exceção para inputs inválidos.
     * @param errorMessage Mensagem de erro.
     */
    public InvalidInputException(String errorMessage) {
        super(errorMessage);
    }
}