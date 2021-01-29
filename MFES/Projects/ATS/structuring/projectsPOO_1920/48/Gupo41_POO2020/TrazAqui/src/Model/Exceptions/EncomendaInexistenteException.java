package Model.Exceptions;

public class EncomendaInexistenteException extends Exception    {
    /**
     * Construtor vazio
     */
    public EncomendaInexistenteException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public EncomendaInexistenteException(String s){
        super(s);
    }
}
