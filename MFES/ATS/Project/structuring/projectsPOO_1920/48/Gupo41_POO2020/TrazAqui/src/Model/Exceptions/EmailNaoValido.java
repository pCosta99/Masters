package Model.Exceptions;

public class EmailNaoValido extends Exception{
    /**
     * Construtor vazio
     */
    public EmailNaoValido (){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public EmailNaoValido (String s){
        super(s);
    }
}
