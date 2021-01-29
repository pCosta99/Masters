package Model.Exceptions;

public class PasswordNaoValidoException extends Exception {
    /**
     * Construtor vazio
     */
    public PasswordNaoValidoException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public PasswordNaoValidoException(String s){
        super(s);
    }
}
