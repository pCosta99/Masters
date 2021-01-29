package app.exceptions;

public class SemPermissaoTransporteEncMedicaException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 9L;

    /**
     * Método construtor não parametrizado
     */
    public SemPermissaoTransporteEncMedicaException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public SemPermissaoTransporteEncMedicaException(String s) {
        super(s);
    }
}
