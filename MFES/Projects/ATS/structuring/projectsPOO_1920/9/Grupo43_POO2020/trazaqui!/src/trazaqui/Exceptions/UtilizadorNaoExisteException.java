package trazaqui.Exceptions;

public class UtilizadorNaoExisteException extends Exception{
    /**
     * Construtor vazio
     */
    public UtilizadorNaoExisteException() { super(); }

    /**
     * Construtor parametrizado
     *
     * @param msg A mensagem a exibir
     */
    public UtilizadorNaoExisteException(String msg) { super(msg); }
}
