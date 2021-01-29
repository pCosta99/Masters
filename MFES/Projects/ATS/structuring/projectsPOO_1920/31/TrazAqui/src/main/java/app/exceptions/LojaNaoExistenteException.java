package app.exceptions;

public class LojaNaoExistenteException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 16L;

    /**
     * Método construtor não parametrizado
     */
    public LojaNaoExistenteException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public LojaNaoExistenteException(String s) {
        super(s);
    }
}
