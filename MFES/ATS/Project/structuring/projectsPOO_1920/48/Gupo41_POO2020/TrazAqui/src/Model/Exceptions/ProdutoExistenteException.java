package Model.Exceptions;

public class ProdutoExistenteException extends Exception{
    /**
     * Construtor vazio
     */
    public ProdutoExistenteException (){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public ProdutoExistenteException (String s){
        super(s);
    }
}
