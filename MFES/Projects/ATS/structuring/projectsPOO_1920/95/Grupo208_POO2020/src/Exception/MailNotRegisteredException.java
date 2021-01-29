package Exception;

/**
 * Exception que é invocada quando ocorrem erros com o mail.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class MailNotRegisteredException extends Exception{

    /**
     * Utilizada quando mail ainda não se encontra registado.
     */
    public MailNotRegisteredException(){
        super("O mail dado não está registado!");
    }

    /**
     * Utilizada quando o mail ainda não se encontra registado.
     * @param s, Mail ainda não registado.
     */
    public MailNotRegisteredException(String s){
        super(s);
    }
}
