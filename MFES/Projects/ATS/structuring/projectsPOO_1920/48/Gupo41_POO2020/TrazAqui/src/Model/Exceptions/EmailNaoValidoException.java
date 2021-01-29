package Model.Exceptions;

public class EmailNaoValidoException extends Exception{
    /**
     * Construtor vazio
     */
    public EmailNaoValidoException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public EmailNaoValidoException(String s){
        super(s);
    }
}
