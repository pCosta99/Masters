package app.exceptions;

public class SemPermissaoEfectuarEncomendaException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 7L;

    /**
     * Método construtor não parametrizado
     */
    public SemPermissaoEfectuarEncomendaException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public SemPermissaoEfectuarEncomendaException(String s) {
        super(s);
    }
}
