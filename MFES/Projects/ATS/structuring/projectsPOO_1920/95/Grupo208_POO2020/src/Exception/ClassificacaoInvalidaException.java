package Exception;

/**
 * Exception que é invocada quando ocorrem erros com a classificação.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class ClassificacaoInvalidaException extends Exception{

    /**
     * Utilizada quando uma classificação é inválida.
     */
    public ClassificacaoInvalidaException(){
        super("A classificaçao deve ser um inteiro entre 1-10!");
    }

    /**
     * Utilizada quando uma classificação é inválida.
     * @param num, Classificação inválida.
     */
    public ClassificacaoInvalidaException(int num){
        super("A classificação " + num + " não é válida. Deve ser um inteiro entre 1-10");
    }
}
