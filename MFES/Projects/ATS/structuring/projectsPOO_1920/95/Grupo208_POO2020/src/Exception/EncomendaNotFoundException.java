package Exception;

import Enums.Estado;

/**
 * Exception que é invocada quando ocorrem erros com a encomenda.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class EncomendaNotFoundException extends Exception{

    /**
     * Utilizada quando a encomenda é inválida.
     */
    public EncomendaNotFoundException(){
        super("Encomenda Inválida!");
    }

    /**
     * Utilizada quando o código da encomenda não foi encontrado.
     * @param s, Código da encomenda.
     */
    public EncomendaNotFoundException(String s){
        super("Encomenda " + s + " não encontrada!");
    }

    /**
     * Utilizada quando a encomenda nao pode ser entregue por causa do seu estado.
     * @param e, Estado da encomenda.
     */
    public EncomendaNotFoundException(Estado e){
        super("Encomenda não pode ser entregue! Estado: " + e);
    }
}
