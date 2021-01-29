package app.exceptions;

public class CodigoProdutoJaExistenteException extends Exception {

    /**
     *
     */
    private static final long serialVersionUID = 20L;

    /**
     * Método construtor não parametrizado
     */
    public CodigoProdutoJaExistenteException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public CodigoProdutoJaExistenteException(String s) {
        super(s);
    }

}
