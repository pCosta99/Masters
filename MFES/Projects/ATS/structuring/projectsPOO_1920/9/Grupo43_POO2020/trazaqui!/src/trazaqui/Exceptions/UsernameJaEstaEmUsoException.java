package trazaqui.Exceptions;

public class UsernameJaEstaEmUsoException extends Exception {
    /**
     * Construtor vazio
     */
    public UsernameJaEstaEmUsoException() { super(); }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public UsernameJaEstaEmUsoException(String msg) { super(msg); }
}