package app.exceptions;

public class SemPermissaoEstadoTransportadorException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 17L;

    /**
     * Método construtor não parametrizado
     */
    public SemPermissaoEstadoTransportadorException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public SemPermissaoEstadoTransportadorException(String s) {
        super(s);
    }
}
