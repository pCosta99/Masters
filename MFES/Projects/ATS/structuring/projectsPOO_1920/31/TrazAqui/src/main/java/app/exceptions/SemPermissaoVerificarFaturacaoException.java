package app.exceptions;

public class SemPermissaoVerificarFaturacaoException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 13L;

    /**
     * Método construtor não parametrizado
     */
    public SemPermissaoVerificarFaturacaoException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public SemPermissaoVerificarFaturacaoException(String s) {
        super(s);
    }
}
