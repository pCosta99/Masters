package Model.Exceptions;

public class LojaInexistenteException extends Exception{

    /**
     * Construtor vazio
     */
    public LojaInexistenteException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public LojaInexistenteException(String s){
        super(s);
    }
}
