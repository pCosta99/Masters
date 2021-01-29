package Exception;

/**
 * Exception que é invocada quando ocorrem erros com as encomendas.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class EncomendaInvalidaException extends Exception{

    /**
     * Utilizada quando a encomenda é inválida.
     */
    public EncomendaInvalidaException(){
        super("A encomenda tem algum(ns) parâmetro(s) errado(s)");
    }

    /**
     * Utilizada quando a encomenda é inválida.
     * @param s, Código da encomenda inválida.
     */
    public EncomendaInvalidaException(String s){
        super(s);
    }
}
