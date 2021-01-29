package Exception;

/**
 * Exception que é invocada quando ocorrem erros com o número de argumentos.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class NumberArgumentsLineException extends Exception {

    /**
     * Utilizada quando o número de argumentos é inválido.
     */
    public NumberArgumentsLineException(){
        super("Linha com argumentos insuficientes");
    }

    /**
     * Utilizado quando número de argumentos é inválido.
     * @param s, Número de argumentos inválido.
     */
    public NumberArgumentsLineException(String s){
        super(s);
    }
}
