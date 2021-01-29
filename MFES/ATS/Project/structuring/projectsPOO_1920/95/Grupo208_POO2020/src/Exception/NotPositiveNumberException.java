package Exception;

/**
 * Exception que é invocada quando ocorrem erros com a leitura de números.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class NotPositiveNumberException extends Exception {

    /**
     * Utilizada quando o número não é positivo.
     */
    public NotPositiveNumberException(){
        super("O número indicado não é positivo!");
    }

    /**
     * Utilizada quando o número não é positivo.
     * @param d, Número não positivo.
     */
    public NotPositiveNumberException(double d){
        super("" + d + " não é positivo!");
    }

    /**
     * Utilizada quando o número não é positivo.
     * @param i, Número não positivo.
     */
    public NotPositiveNumberException(int i){
        super("" + i + " não é positivo!");
    }

    /**
     * Utilizada quando o número não é positivo.
     * @param s, Número não positivo.
     */
    public NotPositiveNumberException(String s){
        super(s);
    }
}
