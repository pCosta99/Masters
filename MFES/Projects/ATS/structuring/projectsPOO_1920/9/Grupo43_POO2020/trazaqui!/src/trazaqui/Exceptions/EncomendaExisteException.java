package trazaqui.Exceptions;

public class EncomendaExisteException extends Exception {
    /**
     * Construtor vazio
     */
    public EncomendaExisteException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public EncomendaExisteException(String msg)
    {
        super(msg);
    }

}