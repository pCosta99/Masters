package src.view;

import src.controller.VoluntarioController;
import src.exceptions.EncomendaInvalidaException;
import src.exceptions.MaximaCapacidadeEntregasException;
import src.model.Distribuidor;
import src.model.Entrega;

import java.time.DateTimeException;
import java.util.Collection;
import java.util.Scanner;
import java.util.Set;

public class VoluntarioView {
    private VoluntarioController controller;
    private Scanner sc;

    public VoluntarioView(VoluntarioController c, Scanner sc){
        this.controller = c;
        this.sc = sc;
    }

    public void run(){
        int x = 1;
        while(x != 0){
            System.out.println("Escolha uma opção:");
            System.out.println("0 - Sair");
            System.out.println("1 - Altera estado de recolha");
            System.out.println("2 - Verificar Entregas a serem feitas");
            System.out.println("3 - Realizar Entrega");
            System.out.println("4 - Verificar Entregas no histórico");
            System.out.println("5 - Verificar Perfil");
            System.out.println("6 - Obter os 10 utilizadores que mais usam o sistema");
            try{
                x = sc.nextInt();
                sc.nextLine();

                switch(x){

                    case (1):
                        this.alteraEstadoDeRecolhaSc();
                        break;

                    case (2):
                        try{
                            System.out.println(this.controller.getEntregaAtiva().toString());
                        }
                        catch(NullPointerException e){
                            System.out.println("Não possui entregas ativas no momento");
                        }                       
                        break;

                    case (3):
                        this.realizarEntregaSc();
                        break;

                    case (4):
                        this.getHistoricoEntregas();
                        break;

                    case(5):
                        this.showPerfil(this.controller.getPerfil());
                        break;

                    case(6):
                        this.top10Utilizadores();
                        break;

                    case (0):
                        System.out.println("Até breve!");
                        break;

                    default:
                        System.out.println("Insira uma opção válida");
                }
            }

            catch(RuntimeException e){
                System.out.println("Não foi possível Realizar o solicitado!");
                System.out.println("Input inválido!");
                sc.nextLine();
            }
        }
    }

    public void alteraEstadoDeRecolhaSc() throws RuntimeException{
            int x;
            System.out.println("Escolha uma opção:");
            System.out.println("0 - Não Posso Recolher");
            System.out.println("1 - Posso Recolher");
            System.out.println("Outro valor - Cancelar");
            x = sc.nextInt();

            try {
                switch (x) {

                    case (0):
                        this.controller.sinalizaRecolhaEncomendas(false);
                        break;

                    case (1):
                        this.controller.sinalizaRecolhaEncomendas(true);
                        break;

                    default:
                        break;
                }
            }

        catch(MaximaCapacidadeEntregasException e){
            System.out.println("Atingiu a sua capacidade máxima de entregas, tente mais tarde!");
        }

    }

    public void realizarEntregaSc(){
        try{
            this.controller.fazEntrega();
            System.out.println("Entrega realizada");
        }

        catch (EncomendaInvalidaException e){
            System.out.println("Não possui entregas para realizar neste momento!");
            System.out.println(e.getMessage());
        }
    }

    public void showEntregas(Collection<Entrega> printables){
        int r = 0;
        for (Entrega e : printables){
            System.out.println(r + "-" + e);
            r++;
        }
        System.out.println("");

    }

    public void showPerfil(Distribuidor atual){
        System.out.println("Código de user:");
        System.out.println(atual.getUsername());
        System.out.println("Nome:");
        System.out.println(atual.getNome());
        System.out.println("Email:");
        System.out.println(atual.getEmail());
        System.out.println("Localização:");
        System.out.println(atual.getLocalizacao());
        System.out.println("Estado de Recolha: " + atual.estaARecolher());
        System.out.println("Raio de ação:");
        System.out.println(atual.getRaio());
        System.out.println("Classificação:");
        System.out.println(atual.getClassificacao().getValorClass());
    }

    public Scanner getScanner(){
        return this.sc;
    }

    public void getHistoricoEntregas(){
        int ano1,mes1,dia1,ano2,mes2,dia2;
        System.out.println("Insira a data a partir da qual deseja ver o valor faturado:");
        System.out.println("Insira o ano:");
        ano1 = sc.nextInt();
        System.out.println("Insira o mês:");
        mes1 = sc.nextInt();
        System.out.println("Insira o dia:");
        dia1 = sc.nextInt();
        System.out.println("Insira a data até qual deseja ver o valor faturado:");
        System.out.println("Insira o ano:");
        ano2 = sc.nextInt();
        System.out.println("Insira o mês:");
        mes2 = sc.nextInt();
        System.out.println("Insira o dia:");
        dia2 = sc.nextInt();

        try {
            this.showEntregas(this.controller.getHistoricoEntregas(ano1, mes1, dia1, ano2, mes2, dia2));
        }

        catch (DateTimeException d){
            System.out.println("Valores Inválidos! Tente novamente!");
        }
    }

    public void top10Utilizadores(){
        System.out.println(this.controller.getTop10UserNumEntregas().toString());
    }
}
