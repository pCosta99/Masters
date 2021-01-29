package Model.Exceptions;

public class EmpresaInexistenteException extends Exception{
    /**
     * Construtor vazio
     */
    public EmpresaInexistenteException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public EmpresaInexistenteException (String s){
        super(s);
    }
}
