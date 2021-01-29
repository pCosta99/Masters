package Exception;

/**
 * Exception que é invocada quando ocorrem erros com a conversão de dados.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class TypeConvertionException extends Exception {

    /**
     * Utilizada quando é impossivel converter dados.
     */
    public TypeConvertionException(){
        super("Impossivel converter dado(s)");
    }

    /**
     * Utilizada quando é impossível de converter dados.
     * @param s, Dados impossiveis de serem convertidos.
     */
    public TypeConvertionException(String s){
        super(s);
    }

    /**
     * Utilizada quando é impossível converter dados.
     * @param s, Dados impossiveis de converter.
     * @param d, Dados em que não é possível converter.
     */
    public TypeConvertionException(String s, String d){
        super("Impossivel converter " + s + " em " + d);
    }
}
