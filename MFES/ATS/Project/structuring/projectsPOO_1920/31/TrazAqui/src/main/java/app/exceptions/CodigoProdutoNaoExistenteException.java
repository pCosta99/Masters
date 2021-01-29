package app.exceptions;

public class CodigoProdutoNaoExistenteException extends Exception {

    /**
     *
     */
    private static final long serialVersionUID = 21L;

    /**
     * Método construtor não parametrizado
     */
    public CodigoProdutoNaoExistenteException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public CodigoProdutoNaoExistenteException(String s) {
        super(s);
    }

}
