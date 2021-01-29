package trazaqui.Exceptions;

public class TransportadoraNaoExisteException extends Exception {

    /**
     * Construtor vazio
     */
    public TransportadoraNaoExisteException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public TransportadoraNaoExisteException(String msg)
    {
        super(msg);
    }
}