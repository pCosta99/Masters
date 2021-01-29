package View;

import java.io.Serializable;

public class ApresentacaoVoluntarioTransportadora implements Serializable {
    private final Output out;

    public ApresentacaoVoluntarioTransportadora() {
        out = new Output();
    }

    /**
     * Apresenta menu voluntário
     */
    public void printMenuVoluntario() {
        out.printMenus(new String[]{"Sinalizar como disponivel/indisponivel para entregar encomendas", "Aceitar Encomenda", "Aceitar/Recusar Encomendas Médicas","Aceder às encomendas", "Classificação"},"MENU VOLUNTÁRIO",1);
    }

    /**
     * Apresenta menu transportadora
     */
    public void printMenuTransportadora() {
        out.printMenus(new String[]{"Sinalizar como disponivel/indisponivel para entregar encomendas", "Aceitar/Recusar Encomendas Médicas", "Aceder às encomendas", "Total faturado", "Classificação","Definir rota de entregas"},"MENU TRANSPORTADORA",1);
    }

    /**
     * Apresenta mensagem estafeta disponivel
     */
    public void printEstafetaDisponivel() {
        System.out.println("Está disponivel para entregar encomendas");
    }

    /**
     * Apresenta mensagem estafeta indisponivel
     */
    public void printEstafetaIndisponivel() {
        System.out.println("Está indisponivel para entregar encomendas");
    }

    /**
     * Apresenta mensagem preço
     * @param preco preço
     */
    public void printEstafetaPreco(double preco) {
        System.out.println("Preço: " + String.format("%.2f", preco) + " €");
    }

    /**
     * Apresenta mensagem estafeta faturação
     * @param faturacao faturaçao
     */
    public void printEstafetaFaturacao(double faturacao) {
        System.out.println("Faturação: " + String.format("%.2f", faturacao) + " €");
    }

    /**
     * Apresenta mensagem estafeta classificação
     * @param classificacao classificação
     */
    public void printEstafetaClassicacao(double classificacao) {
        System.out.println("Classificação: " + String.format("%.2f", classificacao));
    }

    /**
     * Apresenta mensagem sem encomenda
     */
    public void printSemEncomendas(){
        System.out.println("Não existem encomendas para entrega!");
    }

    /**
     * Apresenta mensagem encomenda recusada
     */
    public void printEncRecusada(){
        System.out.println("Encomenda recusada com sucesso!");
    }

}
