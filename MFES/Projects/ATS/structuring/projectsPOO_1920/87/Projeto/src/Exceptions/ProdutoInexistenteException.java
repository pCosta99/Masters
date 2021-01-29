package Exceptions;

import java.io.Serializable;

public class ProdutoInexistenteException extends Exception implements Serializable {

    /**
     * Método que identifica uma exceção.
     */
    public ProdutoInexistenteException(){
        super();
    }

    /**
     * Método que identifica uma exceção e apresenta uma mensagem de erro.
     * @param m Mensagem de erro.
     */
    public ProdutoInexistenteException(String m){
        super(m);
    }
}
