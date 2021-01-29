package exceptions;

public class ExcecaoProdutoInexistente extends Exception {

    public ExcecaoProdutoInexistente(){ super(); }

    public ExcecaoProdutoInexistente(String s){
        super(s);
    }
}
