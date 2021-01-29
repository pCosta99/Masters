package Model.Exceptions;

public class EmailExistenteException extends Exception{

    /**
     * Construtor vazio
     */
    public EmailExistenteException (){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public EmailExistenteException (String s){
        super(s);
    }
}
