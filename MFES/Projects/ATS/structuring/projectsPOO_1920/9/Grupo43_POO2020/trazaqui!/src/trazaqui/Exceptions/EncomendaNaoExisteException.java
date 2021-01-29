package trazaqui.Exceptions;

public class EncomendaNaoExisteException extends Exception {
    /**
     * Construtor vazio
     */
    public EncomendaNaoExisteException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public EncomendaNaoExisteException(String msg)
    {
        super(msg);
    }

}
