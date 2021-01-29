package trazaqui.Exceptions;

import trazaqui.Voluntario;

public class VoluntarioExisteException extends Exception{
    /**
     * Construtor vazio
     */
    public VoluntarioExisteException() { super(); }

    /**
     * Construtor parametrizado
     * @param msg A mensagem a exibir
     */
    public VoluntarioExisteException(String msg) { super(msg); }
}