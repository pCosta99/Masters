package Model.Exceptions;

public class PasswordNaoValido extends Exception {
    /**
     * Construtor vazio
     */
    public PasswordNaoValido (){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public PasswordNaoValido (String s){
        super(s);
    }
}
