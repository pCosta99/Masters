package Projeto;

public class Pedido {
    private Loja loja;
    private Encomenda encomenda;


    public Pedido(Loja loja, Encomenda encomenda){
        this.loja = loja;
        this.encomenda = encomenda;
    }

    public Loja getLoja() {
        return loja;
    }

    public Encomenda getEncomenda() {
        return encomenda;
    }

    @Override
    public String toString() {
        return "Pedido{" +
                "loja=" + loja +
                ", encomenda=" + encomenda +
                '}';
    }
}
