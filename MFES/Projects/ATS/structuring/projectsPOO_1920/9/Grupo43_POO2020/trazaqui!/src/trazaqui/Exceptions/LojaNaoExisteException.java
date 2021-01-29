package trazaqui.Exceptions;

public class LojaNaoExisteException extends Exception {

    /**
     * Construtor vazio
     */
    public LojaNaoExisteException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public LojaNaoExisteException(String msg)
    {
        super(msg);
    }
}
