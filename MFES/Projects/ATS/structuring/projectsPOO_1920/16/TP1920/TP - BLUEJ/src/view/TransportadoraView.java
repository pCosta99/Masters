package src.view;

import src.controller.TransportadoraController;
import src.exceptions.EncomendaInvalidaException;
import src.exceptions.MaximaCapacidadeEntregasException;
import src.exceptions.ValorInvalidoException;
import src.model.Distribuidor;
import src.model.Encomenda;
import src.model.Entrega;
import src.controller.LojaController;

import java.time.DateTimeException;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.Scanner;




public class TransportadoraView{
	private TransportadoraController controller;
	private Scanner sc;

	public TransportadoraView(TransportadoraController controller, Scanner sc){
		this.controller = controller;
		this.sc = sc;
	}

	public void run(){
		int x = 1;
		while(x != 0){
			System.out.println("Escolha uma opção:");
			System.out.println("0 - Sair");
			System.out.println("1 - Altera estado de recolha");
			System.out.println("2 - Alterar Preço para distância e tempo");
			System.out.println("3 - Verificar Entregas a serem feitas");
			System.out.println("4 - Realizar Entrega");
			System.out.println("5 - Verificar Entregas no histórico");
            System.out.println("6 - Verificar Valor faturado");
            System.out.println("7 - Verificar Perfil");
			System.out.println("8 - Obter os 10 utilizadores que mais usam o sistema");
            try{
    			x = sc.nextInt();
    			sc.nextLine();

    			switch(x){

    				case (1): 
                        this.alteraEstadoDeRecolhaSc();
                        break;

                   case (2): 
                        this.alterarPrecosSc();
                        break;

                   case (4):
                        this.realizarEntregaSc();
                        break;

                   case (3):
                        this.showEntregas(this.controller.getEntregasAtivas());
                        //System.in.read();
                        break;

                   case (5): 
                        this.getHistoricoEntregas();
                        //System.in.read();
                        break;

                    case (6): 
                        this.getValorFaturado();
                        //System.in.read();
                        break;

                    case(7):
                        this.showPerfil(this.controller.getPerfil());
                        break;

                    case(8):
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


    public void alteraEstadoDeRecolhaSc(){
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
            catch (MaximaCapacidadeEntregasException e){
                System.out.println("Atingiu a sua capacidade máxima de entregas, tente mais tarde!");
            }

    }



    public void alterarPrecosSc(){
        double x,y;

            System.out.println("Insira o novo preço por KM:");
            x = sc.nextDouble();
            System.out.println("Insira o novo preço por minuto de espera:");
            y = sc.nextDouble();
            try {
                this.controller.setPrecoTempoDistancia(y, x);
            }
            catch (ValorInvalidoException exc){
                System.out.println("Não foi possível Realizar o solicitado!");
                System.out.println("Input inválido!");
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



    public void realizarEntregaSc(){
        Set<Entrega> ents = this.controller.getEntregasAtivas();
        this.showEntregas(ents);
        String sel;
        System.out.println("Selecione o código da Entrega a realizar:");
        sel = this.sc.nextLine();
        try {
            this.controller.fazEntregaTransportadora(sel);
            System.out.println("Entrega realizada");
        }
        
        catch(EncomendaInvalidaException e){
            System.out.println("Não foi possível realizar o solicitado;");
            System.out.println(e.getMessage());
        }
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

    public void getValorFaturado(){
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
            double val = this.controller.getValorFaturadoTrans(ano1, mes1, dia1, ano2, mes2, dia2);
            System.out.println("O valor faturado foi de :");
            System.out.println("€ " + val);
        }

        catch (DateTimeException d){
            System.out.println("Valores Inválidos! Tente novamente!");
        }


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
