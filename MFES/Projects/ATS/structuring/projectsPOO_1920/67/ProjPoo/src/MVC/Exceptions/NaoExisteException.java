package MVC.Exceptions;

public class NaoExisteException extends Exception {
    public NaoExisteException(){
        super();
    }

    public NaoExisteException(String s){
        super(s);
    }

    public NaoExisteException(String s1, String s2){
        super(s1 + ", " + s2);
    }
}
