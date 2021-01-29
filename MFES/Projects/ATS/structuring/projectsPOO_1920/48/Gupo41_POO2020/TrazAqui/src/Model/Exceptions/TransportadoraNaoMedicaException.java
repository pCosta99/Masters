package Model.Exceptions;

public class TransportadoraNaoMedicaException extends Exception {
    /**
     * Construtor vazio
     */
    public TransportadoraNaoMedicaException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public TransportadoraNaoMedicaException(String s){
        super(s);
    }
}
