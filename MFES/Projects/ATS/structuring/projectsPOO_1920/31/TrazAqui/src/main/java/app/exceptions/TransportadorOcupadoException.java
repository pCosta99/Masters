package app.exceptions;

public class TransportadorOcupadoException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 14L;

    /**
     * Método construtor não parametrizado
     */
    public TransportadorOcupadoException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public TransportadorOcupadoException(String s) {
        super(s);
    }
}
