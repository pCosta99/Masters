package Model.Exceptions;

public class EncomendaNaoTransportadaException extends Exception{
    /**
     * Construtor vazio
     */
    public EncomendaNaoTransportadaException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public EncomendaNaoTransportadaException (String s){
        super(s);
    }
}
