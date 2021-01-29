package Exception;

/**
 * Exception que é invocada quando ocorrem erros com a Loja.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class LojaNotFoundException extends Exception {

    /**
     * Utilizada quando a loja é inválida.
     */
    public LojaNotFoundException(){
        super("Loja Inválida!");
    }

    /**
     * Utilizada quando o código da loja não foi encontrado.
     * @param s, Código da loja.
     */
    public LojaNotFoundException(String s){
        super("Loja " + s + " não encontrada!");
    }
}