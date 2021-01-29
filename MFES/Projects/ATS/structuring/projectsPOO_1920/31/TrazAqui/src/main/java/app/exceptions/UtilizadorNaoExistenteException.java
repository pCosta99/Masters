package app.exceptions;

public class UtilizadorNaoExistenteException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 6L;

    /**
     * Método construtor não parametrizado
     */
    public UtilizadorNaoExistenteException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public UtilizadorNaoExistenteException(String s) {
        super(s);
    }
}
