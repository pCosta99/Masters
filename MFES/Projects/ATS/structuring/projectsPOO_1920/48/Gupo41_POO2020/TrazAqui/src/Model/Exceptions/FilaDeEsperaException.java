package Model.Exceptions;

public class FilaDeEsperaException extends Exception {
    /**
     * Construtor vazio
     */
    public FilaDeEsperaException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public FilaDeEsperaException (String s){
        super(s);
    }
}
