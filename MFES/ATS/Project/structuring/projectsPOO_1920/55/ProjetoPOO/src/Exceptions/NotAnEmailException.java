package Exceptions;

public class NotAnEmailException extends Exception {
    public NotAnEmailException(String msg){
        super(msg);
    }
}