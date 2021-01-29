package exceptions;

public class ExcecaoPedidoEncomenda extends Exception {

    public ExcecaoPedidoEncomenda(){ super(); }

    public ExcecaoPedidoEncomenda(String s){
        super(s);
    }
}
