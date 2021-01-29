package Model.Exceptions;

public class VoluntarioInexistenteException extends Exception{
    /**
     * Construtor vazio
     */
    public VoluntarioInexistenteException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public VoluntarioInexistenteException(String s){
        super(s);
    }
}
