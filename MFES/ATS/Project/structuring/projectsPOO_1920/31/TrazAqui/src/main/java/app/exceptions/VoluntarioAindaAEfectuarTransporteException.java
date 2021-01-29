package app.exceptions;

public class VoluntarioAindaAEfectuarTransporteException extends Exception {

    /**
    *
    */
    private static final long serialVersionUID = 18L;

    /**
     * Método construtor não parametrizado
     */
    public VoluntarioAindaAEfectuarTransporteException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public VoluntarioAindaAEfectuarTransporteException(String s) {
        super(s);
    }

}
