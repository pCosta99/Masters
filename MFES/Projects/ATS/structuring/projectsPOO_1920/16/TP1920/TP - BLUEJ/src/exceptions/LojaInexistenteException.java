package src.exceptions;

public class LojaInexistenteException extends Exception{
	public LojaInexistenteException(){
		super();
	}

	public LojaInexistenteException(String codLoja){
		super("A Loja " + codLoja + " não existe");
	}
}
