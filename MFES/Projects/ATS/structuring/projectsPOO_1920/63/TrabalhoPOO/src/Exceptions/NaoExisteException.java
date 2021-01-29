package Exceptions;

public class NaoExisteException extends Exception{
    public NaoExisteException(){
        super();
    }
    public NaoExisteException(String s){
        super(s);
    }

}
