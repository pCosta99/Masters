package trazaqui.Exceptions;

public class VoluntarioNaoExisteException extends Exception {

    /**
     * Construtor vazio
     */
    public VoluntarioNaoExisteException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public VoluntarioNaoExisteException(String msg)
    {
        super(msg);
    }
}