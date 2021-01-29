package view;

import exceptions.ExcecaoPedidoEncomenda;
import model.Coordenadas;
import model.Encomenda;
import model.Loja;

public class EscolheEncomenda {
    private final Loja loja;
    private final Coordenadas destino;
    private final boolean pagar;

    public EscolheEncomenda(Loja loja,Coordenadas dest, boolean pagar) throws ExcecaoPedidoEncomenda {
        this.loja = loja;
        this.destino = dest;
        this.pagar = pagar;

    }


    public Coordenadas getDestino() {
        return destino;
    }


    public boolean isPagar() {
        return pagar;
    }

    public Loja getLoja(){return this.loja;}
}
