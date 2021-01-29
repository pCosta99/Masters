package Model.Exceptions;

public class ProdutosNaoExisteException extends Exception {

    /**
     * Construtor vazio
     */
    public ProdutosNaoExisteException (){
        super();
    }

    /**
     * Construtor parametrizado
     */
    public ProdutosNaoExisteException (String s){
        super(s);
    }


}
