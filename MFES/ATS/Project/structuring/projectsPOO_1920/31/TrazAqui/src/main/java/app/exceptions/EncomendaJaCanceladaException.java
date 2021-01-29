package app.exceptions;

public class EncomendaJaCanceladaException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 4L;

    /**
     * Método construtor não parametrizado
     */
    public EncomendaJaCanceladaException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public EncomendaJaCanceladaException(String s) {
        super(s);
    }
}
