package Exception;

/**
 * Exception que é invocada quando ocorrem erros com o transportador..
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class TransportadorNotFoundException extends Exception{

    /**
     * Utilizado quando o transportador é inválido.
     */
    public TransportadorNotFoundException(){
        super("Transportador Inválido!");
    }

    /**
     * Utilizada quando o código de transportador é inválido.
     * @param s, Código do transportador.
     */
    public TransportadorNotFoundException(String s){
        super("Transportador " + s + " não encontrado!");
    }
}
