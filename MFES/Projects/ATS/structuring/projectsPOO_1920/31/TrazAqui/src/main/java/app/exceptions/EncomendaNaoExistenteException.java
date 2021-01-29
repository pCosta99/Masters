package app.exceptions;

public class EncomendaNaoExistenteException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 3L;

    /**
     * Método construtor não parametrizado
     */
    public EncomendaNaoExistenteException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public EncomendaNaoExistenteException(String s) {
        super(s);
    }
}
