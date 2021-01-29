package src.exceptions;

public class ValorInvalidoException extends Exception{
	public ValorInvalidoException(){
		super();
	}

	public ValorInvalidoException(double valor){
		super("O valor dado: " + valor + " não é válido!");
	}
}
