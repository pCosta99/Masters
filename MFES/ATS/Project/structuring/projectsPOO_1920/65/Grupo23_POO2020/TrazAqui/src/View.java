import com.sun.source.util.SourcePositions;

import java.util.List;
import java.util.Map;

/**
 * Interface de prints necessarios a apresentaçao de informaçao
 */
public interface View {

    /**
     * Funçao que da print ao menu principal
     */
    public static void printMenu(){
        System.out.println("\n\n\n################################ MENU ################################");
        System.out.println("1 -> Registar um utilizador, voluntario, transportadora ou uma loja.");
        System.out.println("2 -> Login Utilizador.");
        System.out.println("3 -> Login Voluntario.");
        System.out.println("4 -> Login Transportadora.");
        System.out.println("5 -> Login Loja.\n");

        System.out.println("6 -> Top Utilizadores.");
        System.out.println("7 -> Top Transportadoras.");
        System.out.println("8 -> Total faturado por uma transportadora\n");

        System.out.println("S -> Save State.");
        System.out.println("L -> Load State.\n");

        System.out.println("Q -> Quit.\n");
    }

    /**
     * Funçao que da print ao menu da query 1
     */
    public static void showOpcoesRegisto(){
        System.out.println("1 -> Registar Utilizador.");
        System.out.println("2 -> Registar Voluntario.");
        System.out.println("3 -> Registar Transportadora.");
        System.out.println("4 -> Registar Loja.");
    }

    /**
     * Print do menu do Utilizador
     */
    public static void showUserMenu(){
        System.out.println("\n\n1 -> Solicitar a entrega de uma encomenda.");
        System.out.println("2 -> Aceitar serviços de entrega.");
        System.out.println("3 -> Aceder a informaçao das entregas efetuadas.");
        System.out.println("4 -> Classificar o serviço de entrega.\n");

        System.out.println("Q -> Sair\n");
    }

    /**
     * Funçao que da print ao menu de fazer encomendas
     */
    public static void showEncomendaMaking(){
        System.out.println("\nE -> Adicionar um produto a lista.\n");
        System.out.println("Q -> Exit\n");
    }

    /**
     * Print das lojas para o user
     */
    public static void showLojasUser(Map<String, Loja> lojas, Utilizador u){
        System.out.println("Lojas: ");
        for(Map.Entry<String, Loja> m : lojas.entrySet()){
            System.out.println("\t" + m.getKey() + " | "
                    + m.getValue().getPosicao().distancia(u.getPosicao()) + " kms away.");
        }
        System.out.println();
    }

    /**
     * Funçao que da print ao menu de entregas de voluntario ou transportadora
     */
    public static void showEncsVT(){
        System.out.println("\n\n1 -> Encomendas entregues por Voluntarios");
        System.out.println("2 -> Encomendas entregues por Transportadoras\n");

        System.out.println("Q -> Sair\n");
    }

    /**
     * Funçao de print para a query de top utilizadores
     */
    public static void showTopUtilizadores(List<Utilizador> l){
        for(int i = 0; i < l.size(); i++){
            System.out.println(l.get(i).getCodUtilizador() + " -> "
                    + l.get(i).nEncsEntregues() + "Encomendas.");
        }
    }

    /**
     * Funçao que mostra as ofertas de um utilizador
     */
    public static void showOfertaUtilizador(String s){
        String[] campos = s.split(",");

        System.out.println("Codigo de empresa: " + campos[0]);
        System.out.println("Codigo de encomenda: " + campos[1]);
        System.out.println("Portes: " + campos[2] + "€");
    }

    /**
     * Print das encomendas do Utilizador
     */
    public static void showEncomendas(Map<String, Encomenda> encs){
        for(Map.Entry<String, Encomenda> e: encs.entrySet()){
            System.out.print(e.getKey() + "->");
            for(int i = 0; i < 3 && i < e.getValue().getProdutos().size(); i++) {
                System.out.println(e.getValue().getProdutos().get(i).getDescricao());
            }
        }
    }


    /**
     * Print das encomendas disponiveis na loja
     */
    public static void showEncomendas(List<Encomenda> encsM, List<Encomenda> encsN){
        if(encsM != null && encsM.size() != 0){
            System.out.println("Encomendas Medicas: ");
            for(Encomenda e : encsM){
                System.out.print("\t" + e.getCodEncomenda() + " | "
                        + e.getPeso() + " | ");
                for(int i = 0; i < 2 && i < e.getProdutos().size(); i++){
                    System.out.print(e.getProdutos().get(i).getDescricao());
                }
                System.out.print("...");
                System.out.println("");
            }
        }
        System.out.println("Encomendas Normais: ");
        for(Encomenda en : encsN){
            System.out.print("\t" + en.getCodEncomenda() + " | "
                    + en.getPeso() + " | ");
            for(int i = 0; i < 2 && i < en.getProdutos().size(); i++){
                System.out.print(en.getProdutos().get(i).getDescricao() + ", ");
            }
            System.out.print("...");
            System.out.println("");
        }
    }

    /**
     * Funçao que da print as encomendas numa lista
     */
    public static void showEncomendas(List<Encomenda> list){
        for(Encomenda e : list){
            System.out.print(e.getCodEncomenda() + " | ");
            for(int i = 0; i < 2 && i < e.getProdutos().size(); i++){
                System.out.print(e.getProdutos().get(i).getDescricao() + ", ");
            }
            System.out.print("...");
            System.out.println("");
        }
    }

    /**
     * Funçao que da print a info das encomendas
     */
    public static void showInfoEncomenda(Encomenda e){
        System.out.println();
        System.out.println("Codigo Encomenda: " + e.getCodEncomenda());
        System.out.println("Codigo Utilizador: " + e.getCodUtilizador());
        System.out.println("Loja: " + e.getCodLoja());
        System.out.println("Peso: " + e.getPeso());
        System.out.println("Portes: " + e.getPortes());
        System.out.println("Data de Entrega: " + e.getDataChegada());
        System.out.print("Produtos: ");
        for(int i = 0; i < e.getProdutos().size(); i++){
            System.out.print(e.getProdutos().get(i));
        }

    }

    /**
     * Print do menu de Voluntario
     */
    public static void showVoluntarioMenu(boolean medico){
        System.out.println("\n\n1 -> Sinalizar que esta disponivel.");
        System.out.println("2 -> Escolher ir buscar uma encomenda a uma loja.");
        System.out.println("3 -> Fazer o transporte de uma encomenda.");
        System.out.println("4 -> Ver os registos.");

        if(medico) {
            System.out.println("\nM -> Sinalizar que esta disponivel para transporte medico.");
        }

        System.out.println("\nQ -> Exit\n");
    }

    /**
     * Print do estado do Voluntario
     */
    public static void showVoluntarioAvailability(Voluntario v){
        System.out.println("Availability status -> " + v.btoString(v.getDisponivel()));
        System.out.println("Pretende mudar?");
    }

    public static void showVoluntarioMAvailability(Voluntario v){
        System.out.println("Availability status -> " + v.btoString(v.getMDisponivel()));
        System.out.println("Pretende mudar?");
    }


    /**
     * Show Transportadora Menu
     */
    public static void showTransportadoraMenu(boolean medico){
        System.out.println("\n1 -> Mudar disponibilidade.");
        System.out.println("2 -> Determinar o preço de entrega por distancia e tempo na loja");
        System.out.println("3 -> Fazer o transporte da encomenda e registar quanto tempo demorou e o custo");
        System.out.println("4 -> Ver os registos.\n");

        if(medico) {
            System.out.println("M -> Sinalizar que esta disponivel para transporte medico.");
        }

        System.out.println("\nQ -> Exit");
    }

    public static void showDisponibilidade(Transportadora t){
        System.out.println("\n\nAvailability status -> " + t.btoString(t.getAvailability()) + "\n");
    }

    /**
     * Funçao de print para a query de top Transportadoras
     */
    public static void showTopTransportadoras(List<Transportadora> l){
        for(int i = 0; i < l.size(); i++){
            System.out.println(l.get(i).getCodEmpresa() + " -> "
                    + l.get(i).getKms() + " kms.");
        }
    }

    /**
     * Funçao que da print as encomendas e os seus tempos respetivos
     */
    public static void showEncomendasTimerEntrega(Map<String, Double> map){
        for(Map.Entry<String, Double> m : map.entrySet()){
            System.out.println(m.getKey() + " | " + m.getValue() + "horas.");
        }
        System.out.println();
    }

    /**
     * Print do preço de transporte
     */
    public static void showPrecoTransporte(double preco){
        System.out.println(preco + " €");
    }

    /**
     * Funçao que da print do intervalo de entrega
     */
    public static void showIntervaloEntrega(double temp){
        System.out.println(temp + "horas");
    }


    /**
     * Print do menu das lojas
     */
    public static void showLojaMenu(){
        System.out.println("1 -> Sinalizar que existe uma encomenda para ser entregue.");
        System.out.println("2 -> Quantidade de pessoas em fila para serem atendidas.");
        System.out.println("3 -> Ver os registos.\n");

        System.out.println("Q -> Exit.\n");
    }

    /**
     * Funçao que da print as lojas (Transportadora)
     */
    public static void showLojas(List<Loja> l, Transportadora t){
        for(int i = 0; i < l.size(); i++){
            System.out.println(l.get(i).getCodLoja() + " | " + l.get(i).getNome() + " -> "
                    + l.get(i).getFilaDeRecolha().size() + " encomendas "
                    + " | "
                    + l.get(i).getPosicao().distancia(t.getPosicao()) + "kms away.");
        }
    }

    /**
     * Funçao que da print as lojas (Voluntario)
     */
    public static void showLojas(List<Loja> l, Voluntario v){
        for(int i = 0; i < l.size(); i++){
            System.out.println(l.get(i).getCodLoja() + " | " + l.get(i).getNome() + " -> "
                    + l.get(i).getFilaDeRecolha().size() + " encomendas "
                    + " | "
                    + l.get(i).getPosicao().distancia(v.getPosicao()) + "kms away.");
        }
    }

    /**
     * Print do size da fila de espera
     */
    public static void showFilaDeRecolha(Loja l){
        if(l.getFilaDeRecolha().size() == 0) System.out.println("Nao ha ninguem a espera.");
        System.out.println(l.getFilaDeRecolha().size() + "Pessoas.");
    }


    /**
     * Print final de classificaçao atribuida
     */
    public static void showClassificacaoDada(){
        System.out.println("Classificaçao atribuida!");
    }

    /**
     * Print de erros
     */
    public static void showError(String s){
        System.out.println("Error: " + s);
    }

    /**
     * Print de exit
     */
    public static void showBB(){
        System.out.println("Bye cruel world!");
    }

    /**
     * Print de Pedido de inserçao
     */
    public static void showInsercao(String s){
        System.out.print("Insira " + s);
    }

    public static void show(String s){System.out.print(s);}

    public static void showSure(){
        System.out.println("De certeza que quer mudar?");
    }
}
