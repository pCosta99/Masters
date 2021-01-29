package trazaqui.Exceptions;

public class LojaExisteException extends Exception {
    /**
     * Construtor vazio
     */
    public LojaExisteException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public LojaExisteException(String msg)
    {
        super(msg);
    }

}
