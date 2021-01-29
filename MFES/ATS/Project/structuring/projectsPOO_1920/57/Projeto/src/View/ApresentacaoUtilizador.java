package View;

import Model.Encomenda;
import Model.LinhaEncomenda;

import java.io.Serializable;
import java.util.List;

public class ApresentacaoUtilizador implements Serializable {
    private final Output out;

    public ApresentacaoUtilizador() {
        out = new Output();
    }

    /**
     * Apresenta menu utilizador
     */
    public void printMenuUtilizador() {
        out.printMenus((new String[]{"Solicitar entrega de uma encomenda","Fazer encomenda","Aceder às encomendas entregues","Estado das encomendas"}),"MENU UTILIZADOR",1);
    }

    /**
     * Apresenta mensagem e list arr
     * @param message   mensagem
     * @param arr       list de strings
     */
    public void printEncomendas(String message, List<Encomenda> arr)  {
        System.out.println("\n" + message);

        for(Encomenda line : arr)
            System.out.println(line);

        System.out.print("\n");
    }

    /**
     * Apresenta fatura
     * @param enc encomenda
     */
    public void printFatura(Encomenda enc)  {
        System.out.print("\n");

        for(LinhaEncomenda line : enc.getLinha())
            System.out.println(line);

        System.out.println("Preço: " + String.format("%.2f", enc.getPreco()) + " €\n");
    }

    /**
     * Apresenta mensagem erro de entrega
     */
    public void printErroEntrega() {
        System.out.println("Sem mais estafetas disponiveis!");
    }

    /**
     * Apresenta mensagem sem encomendas
     */
    public void printErroSemEncomenda(){
        System.out.println("Não existem encomendas");
    }

    /**
     * Apresenta mensagem encomenda entregue
     * @param code  user code
     * @param nome  nome
     * @param tempo tempo de entrega
     */
    public void printEncomendaEntregueVol(String code, String nome, double tempo) {
        System.out.println("A encomenda foi entregue ao utilizador " + code + ": " + nome);
        System.out.println("Tempo de entrega: " + String.format("%.2f", tempo) + " min");
    }

    /**
     * Apresentação mensagem de encomenda entregue
     * @param code  transpCode
     * @param tipo  tipo de transportador
     * @param nome  nome de transportador
     * @param preco preço
     * @param tempo tempo de entrega
     */
    public void printEncomendaEntregue(String code, String tipo, String nome, double preco, double tempo) {
        System.out.println("A sua encomenda foi entregue pelo(a) " + tipo + " " + code + ": " + nome);
        if(tipo.equals("Transportadora"))
            System.out.println("Preço: " + String.format("%.2f", preco) + " €");
        System.out.println("Tempo de entrega: " + String.format("%.2f", tempo) + " min");
    }

    /**
     * Apresenta mensagem encomenda aceite
     */
    public void printEncomendaAceite() {
        System.out.println("A sua encomenda foi aceite pela loja e precisa de ser solicitada");
    }

    /**
     * Apresenta mensagem compra cancelada
     */
    public void printCompraCancelada() {
        System.out.println("A compra foi cancelada");
    }

    /**
     * Apresenta mensagem compra em espera
     */
    public void printCompraEspera() {
        System.out.println("A compra está à espera de aceitação da loja");
    }

    /**
     * Apresenta mensagem pedir para classificar
     */
    public void printPedirClassificar() {
        System.out.println("Pretende classificar o serviço?(S/N)");
    }

    /**
     * Apresenta mensagem encomenda em standBy
     * @param code codigo de voluntario
     */
    public void printEncomendaStandBy(String code){
        System.out.println("A sua encomenda está à espera de ser aceite pelo voluntário " + code);
    }

    /**
     * Apresenta mensagem pedir encomenda
     *
     * @return String com a mensagem
     */
    public String pedirEncomenda() {
        return "Escolha uma encomenda: ";
    }

    /**
     * Apresenta mensagem encomenda inválida
     */
    public void printEncomendaInvalida() {
        System.out.println("Encomenda Inválida");
    }
}
