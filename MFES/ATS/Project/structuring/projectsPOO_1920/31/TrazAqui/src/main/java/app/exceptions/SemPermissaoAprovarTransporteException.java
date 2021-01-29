package app.exceptions;

public class SemPermissaoAprovarTransporteException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 11L;

    /**
     * Método construtor não parametrizado
     */
    public SemPermissaoAprovarTransporteException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public SemPermissaoAprovarTransporteException(String s) {
        super(s);
    }
}
