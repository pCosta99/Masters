package Model.Exceptions;

public class EncomendaNaoAceiteUtilizadorException extends Exception{
    /**
     * Construtor vazio
     */
    public EncomendaNaoAceiteUtilizadorException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public EncomendaNaoAceiteUtilizadorException (String s){
        super(s);
    }
}
