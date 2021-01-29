package Projeto;

import java.util.ArrayList;

public class Pedidos {
    private ArrayList<Encomenda> pedidos;

    public Pedidos(){
        this.pedidos = new ArrayList<Encomenda>();
    }

    public void addPedido(Encomenda pedido){
        this.pedidos.add(pedido);
    }
}


