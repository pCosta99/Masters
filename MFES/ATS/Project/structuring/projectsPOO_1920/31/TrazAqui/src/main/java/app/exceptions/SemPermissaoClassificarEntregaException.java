package app.exceptions;

public class SemPermissaoClassificarEntregaException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 12L;

    /**
     * Método construtor não parametrizado
     */
    public SemPermissaoClassificarEntregaException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public SemPermissaoClassificarEntregaException(String s) {
        super(s);
    }
}
