package app.exceptions;

public class SemPermissaoTransporteEncException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 10L;

    /**
     * Método construtor não parametrizado
     */
    public SemPermissaoTransporteEncException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public SemPermissaoTransporteEncException(String s) {
        super(s);
    }
}
