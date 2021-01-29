package trazaqui.Exceptions;

public class NaoExisteLojasRegistadasException extends Exception {

    /**
     * Construtor vazio
     */
    public NaoExisteLojasRegistadasException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public NaoExisteLojasRegistadasException(String msg)
    {
        super(msg);
    }
}