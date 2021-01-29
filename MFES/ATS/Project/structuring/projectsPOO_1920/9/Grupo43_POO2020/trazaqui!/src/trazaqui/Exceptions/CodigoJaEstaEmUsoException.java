package trazaqui.Exceptions;

public class CodigoJaEstaEmUsoException extends Exception {
    /**
     * Construtor vazio
     */
    public CodigoJaEstaEmUsoException()
    {
        super();
    }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public CodigoJaEstaEmUsoException(String msg)
    {
        super(msg);
    }
}