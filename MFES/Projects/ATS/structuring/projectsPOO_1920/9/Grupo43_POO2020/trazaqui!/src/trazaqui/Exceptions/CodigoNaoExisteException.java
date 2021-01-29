package trazaqui.Exceptions;

public class CodigoNaoExisteException extends  Exception {
    /**
     * Construtor vazio
     */
    public CodigoNaoExisteException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public CodigoNaoExisteException(String msg)
    {
        super(msg);
    }

}