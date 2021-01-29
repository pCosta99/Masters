package src;

import src.controller.*;
import src.exceptions.LogsInvalidosException;
import src.model.TrazAqui;
import src.view.*;

import java.io.IOException;
import java.util.Map;
import java.util.Scanner;

public class TrazAquiApp {
    /**
     * Método que executa a aplicação TrazAqui! Desde sua inicialização até sua finalização, tentando ler e escrever o estado para ficheiro;
     */
    public static void main(String[] args){
        TrazAqui sistema;

        try {
             sistema = TrazAqui.carregaEstado("trazAquiApp.obj");
        }

        catch (Exception ex ){
            System.out.println("Impossível carregar Estado!");
            System.out.println(ex.getMessage());

            try{
                sistema = new TrazAqui("logs.csv");
            }

            catch (LogsInvalidosException e){
                System.out.println("Erro");
                sistema = new TrazAqui();
            }
        }

        Scanner sc = new Scanner(System.in);
        LoginController logcont = new LoginController(sistema);
        LoginView logview = new LoginView(logcont,sc);
        Map<String,Boolean> cod;

        while(true) {
            cod = logview.run();
            String key = cod.keySet().stream().findFirst().orElse(null);
            if (key != null) {
                boolean med = cod.get(key);

                switch (key.charAt(0)) {
                    case ('l'):
                        LojaController lojaCont = new LojaController(key, sistema);
                        LojaView lojaView = new LojaView(lojaCont, sc);
                        lojaView.run();
                        break;

                    case ('t'):

                        if (!med) {
                            TransportadoraController transCont = new TransportadoraController(key, sistema);
                            TransportadoraView transView = new TransportadoraView(transCont, sc);
                            transView.run();
                        } else {
                            TransportadoraMedicaController transMedCont = new TransportadoraMedicaController(key, sistema);
                            TransportadoraMedicaView transMedView = new TransportadoraMedicaView(transMedCont, sc);
                            transMedView.run();
                        }

                        break;

                    case ('u'):
                        UtilizadorController utiCont = new UtilizadorController(key, sistema);
                        UtilizadorView utiView = new UtilizadorView(utiCont, sc);
                        utiView.run();
                        break;

                    case ('v'):

                        if (!med) {

                            VoluntarioController volCont = new VoluntarioController(key, sistema);
                            VoluntarioView volView = new VoluntarioView(volCont, sc);
                            volView.run();
                        } else {
                            VoluntarioMedicoController volMedCont = new VoluntarioMedicoController(key, sistema);
                            VoluntarioMedicoView volMedView = new VoluntarioMedicoView(volMedCont, sc);
                            volMedView.run();
                        }

                        break;
                }
            }

                System.out.println("Prima 0 para sair, qualquer outra tecla para continuar:");
               try{
                   if(sc.nextInt() == 0) {
                       sistema.salvaEstado("trazAquiApp.obj");
                       System.out.println("Estado gravado com sucesso!");
                       break;
                   }
               }
               catch (RuntimeException e){
                   sc.nextLine();
               }

               catch (IOException e){
                   System.out.println("Oops! Algo correu mal ao gravar o estado!");
                   System.out.println(e.getMessage());
               }

        }

    }
}
