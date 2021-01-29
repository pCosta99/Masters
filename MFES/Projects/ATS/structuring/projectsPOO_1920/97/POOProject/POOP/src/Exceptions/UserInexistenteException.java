package Exceptions;
public class UserInexistenteException extends Exception {

    public UserInexistenteException(){
        super();
    }
    public UserInexistenteException(String cd){
        super(cd);
    }
}
