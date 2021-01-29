package app.exceptions;

public class EstadoRegressivoException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 5L;

    /**
     * Método construtor não parametrizado
     */
    public EstadoRegressivoException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public EstadoRegressivoException(String s) {
        super(s);
    }
}
