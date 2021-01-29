package Exception;

/**
 * Exception que é invocada quando ocorrem erros com os utilizadores.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class UtilizadorNotFoundException extends Exception {

    /**
     * Utilizada quando o utilizador é inválido.
     */
    public UtilizadorNotFoundException(){
        super("Utilizador Inválido!");
    }

    /**
     * Utilizada quando o utilizador é inválido.
     * @param s, Código do utilizador.
     */
    public UtilizadorNotFoundException(String s){
        super("Utilizador " + s + " não encontrado!");
    }
}