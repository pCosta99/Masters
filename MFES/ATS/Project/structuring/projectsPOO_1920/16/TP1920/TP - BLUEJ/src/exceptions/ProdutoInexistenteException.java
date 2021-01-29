package src.exceptions;

public class ProdutoInexistenteException extends Exception{
	public ProdutoInexistenteException(){
		super();
	}

	public ProdutoInexistenteException(String codProd){
		super("o Produto " + codProd + " não existe");
	}
}
