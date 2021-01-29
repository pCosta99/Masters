package View;

import java.io.Serializable;

public class ApresentacaoLoja implements Serializable {
    private final Output out;

    public ApresentacaoLoja() {
        out = new Output();
    }

    /**
     * Apresenta menu loja
     */
    public void printMenuLoja() {
        out.printMenus(new String[]{"Atualizar Tempo de Fila de Espera", "Atualizar tamanho da Fila de Espera", "Aceitar compra","Histórico de encomendas"},"MENU LOJA",1);
    }

    /**
     * Apresenta mensagem loja indisponivel
     */
    public void printMenuLojaIndisponivel() {
        out.printMenus(new String[]{"Aceitar compra", "Histórico de encomendas"},"MENU LOJA",1);
    }

    /**
     * Apresenta mensagem compra aceite
     * @param encCode encCode
     */
    public void printCompraAceite(String encCode) {
        System.out.println("A compra " + encCode + " foi aceite.");
    }

    /**
     * Apresenta mensagem compra recusada
     * @param encCode encCode
     */
    public void printCompraRecusada(String encCode) {
        System.out.println("A compra " + encCode + "foi recusada.");
    }
}
