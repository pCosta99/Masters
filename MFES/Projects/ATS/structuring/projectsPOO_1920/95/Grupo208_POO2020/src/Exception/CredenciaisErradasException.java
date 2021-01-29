package Exception;

/**
 * Exception que é invocada quando ocorrem erros com as credenciais.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class CredenciaisErradasException extends Exception{

    /**
     * Utilizada quando as credenciais estão erradas.
     */
    public CredenciaisErradasException(){
        super("O mail ou a password não estão corretos");
    }

    /**
     * Utilizada quando as credenciais estão erradas.
     * @param s, Parametro das crenciais erradas.
     */
    public CredenciaisErradasException(String s){
        super(s);
    }
}