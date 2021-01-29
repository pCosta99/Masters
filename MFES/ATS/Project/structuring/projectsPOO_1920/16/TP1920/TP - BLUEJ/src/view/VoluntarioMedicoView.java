package src.view;


import src.controller.VoluntarioMedicoController;
import src.exceptions.MaximaCapacidadeEntregasException;

import java.util.Scanner;

public class VoluntarioMedicoView extends VoluntarioView{
	 private VoluntarioMedicoController controller;

    public VoluntarioMedicoView(VoluntarioMedicoController c, Scanner sc){
        super(c,sc);
        this.controller = c;
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

            try{
                x = super.getScanner().nextInt();
                super.getScanner().nextLine();

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
                        super.realizarEntregaSc();
                        break;

                    case (4):
                        this.getHistoricoEntregas();
                        break;

                    case(5):
                        super.showPerfil(this.controller.getPerfil());
                        System.out.println("Aceitação de Medicamentos: " + this.controller.estouAAceitarMed());
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
                super.getScanner().nextLine();
            }
        }
    }

    public void alteraEstadoDeRecolhaSc() throws RuntimeException{
        int x;
        System.out.println("Escolha uma opção:");
        System.out.println("0 - Não Posso Recolher");
        System.out.println("1 - Posso Recolher");
        System.out.println("2 - Não Posso Recolher medicamentos");
        System.out.println("3 - Posso Recolher medicamentos");
        System.out.println("Outro valor - Cancelar");

        try{
            x = super.getScanner().nextInt();

            switch(x){

                case (0):
                    this.controller.sinalizaRecolhaEncomendas(false);
                    break;

                case (1):
                    this.controller.sinalizaRecolhaEncomendas(true);
                    break;

                case (2):
                    this.controller.sinalizaRecolhaMedicamentos(false);
                    break;

                case(3):
                    this.controller.sinalizaRecolhaMedicamentos(true);
                    break;


                default:
                    break;
            }
        }

        catch(MaximaCapacidadeEntregasException e){
            System.out.println("Atingiu a sua capacidade máxima de entregas, tente mais tarde!");
        }

    }
}
