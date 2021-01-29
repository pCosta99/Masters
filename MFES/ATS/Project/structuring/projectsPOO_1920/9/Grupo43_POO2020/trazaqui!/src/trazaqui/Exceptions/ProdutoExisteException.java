package trazaqui.Exceptions;

public class ProdutoExisteException extends Exception {
    /**
     * Construtor vazio
     */
    public ProdutoExisteException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public ProdutoExisteException(String msg)
    {
        super(msg);
    }
}

