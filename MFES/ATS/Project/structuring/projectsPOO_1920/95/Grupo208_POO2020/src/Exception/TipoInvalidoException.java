package Exception;

/**
 * Exception que é invocada quando ocorrem erros com o tipo de dados.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class TipoInvalidoException extends Exception{

    /**
     * Utilizada quando o tipo de dados é inválido.
     */
    public TipoInvalidoException(){
        super("Não existe o tipo dado");
    }

    /**
     * Utilizado quando o tipo de dados é inválido.
     * @param i, Tipo de dados inválidos.
     */
    public TipoInvalidoException(int i){
        super("Não existe tipo com o identificador " + i);
    }
}
