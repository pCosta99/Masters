package hedgehog.view;

import hedgehog.control.Database;
import hedgehog.control.QueryHandler;
import hedgehog.model.client.Client;
import hedgehog.model.emissary.Firm;
import hedgehog.model.store.Store;
import hedgehog.util.point.Point;

import java.time.LocalDateTime;

public class GUI {
    private QueryHandler query_handler;

    public GUI(final QueryHandler query_handler) {
        this.query_handler = query_handler;
    }

//menus top e bottom iguais em todas as janelas

    /**
     * imprime na tela a parte superior do menu
     */
    public void menuTop() {
        System.out.println("*****************************************************************************************************************************");
        System.out.println("*                                                        Traz Aqui!                                        EXIT - Ctrl + C  *");
    }

    /**
     * imprime na tela a parte superior do menu dentro das queries
     */
    public void menuTopQueries() {
        System.out.println("*****************************************************************************************************************************");
        System.out.println("*                                                         Traz Aqui!                                        EXIT - Ctrl + C *");
    }

    /**
     * imprime na tela a parte superior do menu com opção "Next" e "Previous".
     */
    public void menuTopNextPrevious() {
        System.out.println("*****************************************************************************************************************************");
        System.out.println("*                                         PREVIOUS - P   Traz Aqui!  NEXT - N                               EXIT - Ctrl + C *");
    }

    /**
     * imprime na tela a parte superior do menu com opção "Next".
     */
    public void menuTopNext() {
        System.out.println("*****************************************************************************************************************************");
        System.out.println("*                                                        Traz Aqui!  NEXT - N                              EXIT - Ctrl + C  *");
    }

    /**
     * imprime na tela a parte superior do menu com opção "Previous".
     */
    public void menuTopPrevious() {
        System.out.println("*****************************************************************************************************************************");
        System.out.println("*                                         PREVIOUS - P   Traz Aqui!                                        EXIT - Ctrl + C  *");
    }

    public void menuBotStats() {
        System.out.print("* 10 Clientes mais utilizam sistema: ");
        this.query_handler.top_n_clients(10).forEach(client -> System.out.printf("u%d    ", client.code));
        System.out.println("                  *");

        System.out.print("* 10 Empresas com mais Distância Percorrida: ");
        this.query_handler.top_n_firms(10).forEach(firm -> System.out.printf("t%d    ", firm.code));
        System.out.println("                                             *");

        System.out.println("*****************************************************************************************************************************\n");
    }
    public void menuMidEmpresa() {
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     Selecione a opção pretendida:                                                         *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     1 - Login                                                                             *");
        System.out.println("*                                     2 - Registar                                                                          *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
    }

    public void menuEncomendasFeitas(final LocalDateTime begin, final LocalDateTime end) {
        System.out.println("*                                                                                                                           *");
        this.query_handler.orders_made_during(begin, end).forEach(encomenda ->
                System.out.println("*         " + encomenda.code + " - " + encomenda.store_of_origin().account().username() + "                            *"));
        System.out.println("*                                                                                                                           *");
    }

    public void menuBot() {
        System.out.println("*                                                                                                                           *\n");
        System.out.println("*                                                                                                                           *\n");
        System.out.println("*****************************************************************************************************************************\n");
    }

    /**
     * imprime na tela a parte central do menu da main
     */
    public void menuMidMainStart() {
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     Selecione Opção:                                                                      *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     Y - Usar Ficheiro de Logs à Escolha                                                   *");
        System.out.println("*                                     N - Ignorar                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
    }

    public void menuMidMain() {
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     Selecione Opção:                                                                      *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     1 - Cliente                                                                           *");
        System.out.println("*                                     2 - Voluntário                                                                        *");
        System.out.println("*                                     3 - Empresa                                                                           *");
        System.out.println("*                                     4 - Loja                                                                              *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
    }

    /**
     * imprime na tela a parte central do menu com um pedido de input do utilizador
     */
    public void menuMidIO(String io) {
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.print("*          " + io);
        for (int i = 0; i < 113 - io.length(); i++) {
            System.out.print(" ");
        }
        System.out.println("*");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
    }

    public void menuMidCliente() {
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     Selecione a opção pretendida:                                                         *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     1 - Login                                                                             *");
        System.out.println("*                                     2 - Registar                                                                          *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
    }

    public void menuMidClienteLog() {
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     Selecione a opção pretendida:                                                         *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     1 - Solicitar Entrega                                                                 *");
        System.out.println("*                                     2 - Efetuar Encomenda                                                                 *");
        System.out.println("*                                     3 - Classificar Serviço de Entrega                                                    *");
        System.out.println("*                                     4 - Aceder Informações de Entregas Efetuadas                                          *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
    }


//     public void menuMidSolicitarEncomenda(Firm f, PointView pointView, Store store, User user) {

    //         System.out.println("*                                                                                                                           *");
//         System.out.println("*                                     Selecione a opção pretendida:                                                         *");
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*                                     1 - Aceitar Serviço de Transporte de Empresa:                                          *");
//         System.out.println("*                                     " + (f.distance_fare * (pointView.distance_to(f.location, store.location) + pointView.distance_to((store.location, user.location))))      + "                                      *");
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*                                     2 - Voltar à página anterior                                                          *");
//     }
    public void menuMidVoluntario() {
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     Selecione a opção pretendida:                                                         *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     1 - Login                                                                             *");
        System.out.println("*                                     2 - Registar                                                                          *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
    }

    public void menuMidVoluntarioLog() {
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     Selecione a opção pretendida:                                                         *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                     1 - Sinalizar Disponibilidade                                                         *");
        System.out.println("*                                     2 - Ver Encomendas Disponibilizadas por Lojas                                         *");
        System.out.println("*                                     3 - Registar Duração da Entrega                                                       *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
        System.out.println("*                                                                                                                           *");
    }



     public void menuMidEmpresaLog(/* final Firm f, final Store s, final Client c */) {
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                     Selecione a opção pretendida:                                                         *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                     1 - Sinalizar Disponibilidade                                                         *");
         System.out.println("*                                     2 - Determinar Preço de Encomenda                                                     *");
         System.out.println("*                                     3 - Registar Duração e Custo da Entrega                                               *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
        //  System.out.println("* Total faturado: " + f.distance_fare() * (f.location().distance_to(s.location()) + s.location().distance_to(c.location()) ) + "                                                            *");
         System.out.println("*                                                                                                                           *");
     }

     public void menuMidLoja() {
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                     Selecione a opção pretendida:                                                         *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                     1 - Login                                                                             *");
         System.out.println("*                                     2 - Registar                                                                          *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
     }

     public void menuMidLojaLog() {
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                     Selecione a opção pretendida:                                                         *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                     1 - Sinalizar Encomenda Disponível                                                    *");
         System.out.println("*                                     2 - Indicar Quantidade de Pessoas em Fila                                             *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
         System.out.println("*                                                                                                                           *");
    }
     

//     public void menuMidAcederInformacoes(List<String> infoEntregasEfetuadas, String deliver, int timegap) {
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*  <- P                                      Informação de Entregas Efetuadas por:  " + deliver + /*empresa ou voluntario*/ + "Em: " + Integer.toString(timegap) "                                 N ->  *");
//         for (String s : infoEntregasEfetuadas) {
//             System.out.println("*         " + s + "                                                                                                            *");
//         }
//         System.out.println("*                                                                                                                           *");
//     }

//    public void menuSinalizarDisponibilidadeVoluntario(){

//        System.out.println("*                                                                                                                           *");
//        System.out.println("*                                     Selecione (Y) caso esteja disponível, (N) em caso contrário:                          *");
//        System.out.println("*                                                                                                                           *");
//        System.out.println("*                                                                                                                           *");
//        System.out.println("*                                                                                                                           *");
//        System.out.println("*                                     Y - Disponível                                                                        *");
//        System.out.println("*                                     N - Indisponível                                                                      *");
//        System.out.println("*                                                                                                                           *");
//        System.out.println("*                                                                                                                           *");
//        System.out.println("*                                                                                                                           *");
//        System.out.println("*                                                                                                                           *");
//        System.out.println("*                                                                                                                           *");
//        System.out.println("*                                                                                                                           *");
//    }

//     public void menuMidEncomendasDisponiveis(List<String> encomendasDisponiveis) {
//         System.out.println("*                                                                                                                           *");
//         System.out.println("*  <- P                                      Lista de Encomendas Disponíveis:                                         N ->  *");
//         for (String s : encomendasDisponiveis) {
//             System.out.println("*         " + s + "                                                                                                            *");
//         }
//         System.out.println("*                                                                                                                           *");
//     }
}