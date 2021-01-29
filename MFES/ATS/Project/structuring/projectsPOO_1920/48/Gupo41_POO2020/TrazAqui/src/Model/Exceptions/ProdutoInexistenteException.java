package Model.Exceptions;

public class ProdutoInexistenteException extends Exception{
    /**
     * Construtor vazio
     */
    public ProdutoInexistenteException(){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public ProdutoInexistenteException(String s){
        super(s);
    }
}
