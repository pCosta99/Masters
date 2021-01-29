package trazaqui.Exceptions;

public class ProdutoNaoExisteException extends Exception {
    /**
     * Construtor vazio
     */
    public ProdutoNaoExisteException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public ProdutoNaoExisteException(String msg)
    {
        super(msg);
    }

}
