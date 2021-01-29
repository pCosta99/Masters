package app.exceptions;

public class UtilizadorJaExistenteException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 2L;

    /**
     * Método construtor não parametrizado
     */
    public UtilizadorJaExistenteException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public UtilizadorJaExistenteException(String s) {
        super(s);
    }
}
