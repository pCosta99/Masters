package app.exceptions;

public class SemPermissaoEfectuarAgendamentoEnvioException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 8L;

    /**
     * Método construtor não parametrizado
     */
    public SemPermissaoEfectuarAgendamentoEnvioException() {
        super();
    }

    /**
     * Método construtor parametrizado
     * 
     * @param String s
     */
    public SemPermissaoEfectuarAgendamentoEnvioException(String s) {
        super(s);
    }
}
