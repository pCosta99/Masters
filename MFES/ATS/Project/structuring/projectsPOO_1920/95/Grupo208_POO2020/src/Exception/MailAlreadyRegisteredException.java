package Exception;

/**
 * Exception que é invocada quando ocorrem erros com o mail.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class MailAlreadyRegisteredException extends Exception {

    /**
     * Utilizada quando o mail já se encontra registado.
     */
    public MailAlreadyRegisteredException(){
        super("O mail já se encontra associado a uma conta!");
    }

    /**
     * Utilizada quando o mail já se encontra registado.
     * @param s, Mail já registado.
     */
    public MailAlreadyRegisteredException(String s){
        super(s);
    }
}
