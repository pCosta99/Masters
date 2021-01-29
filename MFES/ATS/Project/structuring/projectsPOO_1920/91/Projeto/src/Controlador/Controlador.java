/**
 * Esta classe é o controlador geral
 */
package Controlador;
import Modelo.CarregarFicheiro;
import Modelo.Parse;
import Modelo.SistemaGestaoEntregas;
import Visualizador.Visualizador;

import java.io.Serializable;

public class Controlador implements IControlador , Serializable {
    SistemaGestaoEntregas sge;

    public Controlador() {
        this.sge = new SistemaGestaoEntregas();
    }

    /**
     * Método que imprime o menu inicial
     */
    @Override
    public void menu(){
        Visualizador.printString("MENU INICIAL ");
        Visualizador.printString("1| Entrar como utilizador.");
        Visualizador.printString("2| Entrar como voluntário.");
        Visualizador.printString("3| Entrar como empresa transportadora.");
        Visualizador.printString("4| Entrar como loja.");
        Visualizador.printString("5| Print SGE.");
        Visualizador.printString("6| Gravar um ficheiro.");
        Visualizador.printString("7| Carregar de um ficheiro.");
        Visualizador.printString("8| Consultas.");
        Visualizador.printString("9| Sair");
    }

    /**Método que recebe o input do utilizador
     *
     */
    @Override
    public void run() {
        Parse p = new Parse();
        p.parse(sge);
        CarregarFicheiro novo = new CarregarFicheiro();
        Visualizador.printString("Loaded");
        IControladorUtilizador c = new ControladorUtilizador(sge);
        IControladorLoja cl = new ControladorLoja(sge);
        IControladorVoluntario cv = new ControladorVoluntario(sge);
        IControladorTransportadora ct = new ControladorTransportadora(sge);
        String input = "";
        while(!(input.equalsIgnoreCase("9"))){
            menu();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1" :
                    c.run();
                    break;
                case "2" :
                    cv.run();
                    break;
                case "3":
                    ct.run();
                    break;
                case "4":
                    cl.run();
                    break;
                case "5":
                    Visualizador.printString(this.sge.toString());
                    break;
                case "6":
                    novo.guardar("src/SistemaGestaoEntrega.dat" , sge);
                    Visualizador.printString("Ficheiro guardado.");
                    break;
                case "7":
                    sge = novo.carregar("src/SistemaGestaoEntrega.dat");
                    Visualizador.printString("Ficheiro carregado.");
                    break;
                case "8":
                    runConsultas();
                    break;
                case "9":
                    Visualizador.printString("Obrigado. Volte sempre.");
                    break;
                default:
                    Visualizador.printString("Comando Inválido. Tente de novo.");
                    break;
            }
        }
    }

    public void menuConsulas(){
        Visualizador.printString("1| Top 10 utilizadores.");
        Visualizador.printString("2| Top 10 transportadoras.");
        Visualizador.printString("3| Voltar ao menu anterior.");
    }

    public void runConsultas(){
        String input = "";
        boolean valida = true;
        while(!(input.equalsIgnoreCase("3"))){
            menuConsulas();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1":
                    Visualizador.printString("Os 10 utilizadores mais frequentes são: \n" + sge.top10Utilizadores(sge.getUtilizadores()).toString());
                    break;
                case "2":
                    Visualizador.printString("As 10 transportadoras mais frequentes são: \n" + sge.top10Transportadora(sge.getEmpresas()));
                    break;
                case "3":
                    break;
                default:
                    Visualizador.printString("Comando inválido. Tente novamente.");
            }
        }

    }
}
