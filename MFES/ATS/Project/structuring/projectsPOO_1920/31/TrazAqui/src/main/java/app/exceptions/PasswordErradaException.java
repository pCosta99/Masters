package app.exceptions;

public class PasswordErradaException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Método construtor não parametrizado
     */
    public PasswordErradaException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public PasswordErradaException(String s) {
        super(s);
    }
}
