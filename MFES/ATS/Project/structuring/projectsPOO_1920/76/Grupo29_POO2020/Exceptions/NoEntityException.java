package Exceptions;

public class NoEntityException extends Exception{
    private static final long serialVersionUID = 21L;
    
    /**
     * Exceção para quando não existe uma entidade.
     * @param errorMessage Mensagem de erro.
     */
    public NoEntityException(String errorMessage) {
        super(errorMessage);
    }
}