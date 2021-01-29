package Exceptions;

import java.io.Serializable;

public class UserInexistenteException extends Exception implements Serializable {

    /**
     * Método que identifica uma exceção.
     */
    public UserInexistenteException(){
        super();
    }

    /**
     * Método que identifica uma exceção e apresenta uma mensagem de erro.
     * @param m Mensagem de erro.
     */
    public UserInexistenteException(String m){
        super(m);
    }

}
