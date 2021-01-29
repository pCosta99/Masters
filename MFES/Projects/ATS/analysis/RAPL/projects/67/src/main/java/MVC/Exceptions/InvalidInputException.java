package MVC.Exceptions;

public class InvalidInputException extends Exception{
    public InvalidInputException(){
        super();
    }

    public InvalidInputException(String s){
        super(s);
    }
}
