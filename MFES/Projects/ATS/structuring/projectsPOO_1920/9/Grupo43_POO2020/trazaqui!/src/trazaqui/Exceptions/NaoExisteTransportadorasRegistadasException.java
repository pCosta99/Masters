package trazaqui.Exceptions;

public class NaoExisteTransportadorasRegistadasException extends Exception {

    /**
     * Construtor vazio
     */
    public NaoExisteTransportadorasRegistadasException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public NaoExisteTransportadorasRegistadasException(String msg)
    {
        super(msg);
    }
}