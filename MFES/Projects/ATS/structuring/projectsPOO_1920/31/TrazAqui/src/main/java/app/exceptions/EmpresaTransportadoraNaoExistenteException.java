package app.exceptions;

public class EmpresaTransportadoraNaoExistenteException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 15L;

    /**
     * Método construtor não parametrizado
     */
    public EmpresaTransportadoraNaoExistenteException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public EmpresaTransportadoraNaoExistenteException(String s) {
        super(s);
    }
}
