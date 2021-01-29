package src.exceptions;

public class LoginErrorException extends Exception{
	public LoginErrorException(){
		super();
	}

	public LoginErrorException(String msg){
		super(msg);
	}
}
