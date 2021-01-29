/**
 * Esta classe é o controlador voluntario
 */
package Controlador;
import Modelo.Gps;
import Modelo.Parse;
import Modelo.SistemaGestaoEntregas;
import Visualizador.Visualizador;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ControladorVoluntario implements IControladorVoluntario, Serializable {
    SistemaGestaoEntregas sge;

    public ControladorVoluntario(SistemaGestaoEntregas sge) {
        this.sge = sge;
    }

    /**
     * Menu inicial antes de dar sign in
     */
    @Override
    public void menu() {
        Visualizador.printString("MENU VOLUNTÁRIO");
        Visualizador.printString("1| Sign in.");
        Visualizador.printString("2| Sign up.");
        Visualizador.printString("3| Voltar ao menu.");
    }

    /**
     * Método que recebe o input do utilizador
     */
    @Override
    public void run() {
        String input = "";
        boolean flag = true;
        double gpsx = 0 , gpsy = 0;
        int cod =0 ;

            while (!(input.equalsIgnoreCase("3"))) {
            menu();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1":
                    Visualizador.printString("Introduza o seu código de Voluntario");
                    String codVoluntario = Input.lerString();
                    Visualizador.printString("Introduza a palavra passe");
                    String pass = Input.lerString();
                    if (!this.sge.existeVoluntario(codVoluntario, pass)) {
                        Visualizador.printString("Código de Voluntario ou palavra passe incorretas.");
                    }
                    else  runVoluntario(codVoluntario, pass);
                    break;
                case "2":
                    while(flag) {
                        Visualizador.printString("Introduza um código de Voluntario (0 a 100)");
                        cod = Input.lerInt();
                        if (cod<=0 || cod>=100 || sge.temCodVoluntarios("v" + cod)) {
                            Visualizador.printString("Código incorreto ou já existente. Introduza um novo , por favor.");
                        } else {
                                flag = false;
                                Visualizador.printString("Código aceite.");
                        }
                    }
                    Visualizador.printString("Introduza um nome");
                    String nome = Input.lerString();
                    flag = true;
                    while (flag) {
                        Visualizador.printString("Insira a latitude da sua empresa.(de -180 a 180)");
                        gpsx = Input.lerDouble();
                        if (gpsx > -180 && gpsx < 180) flag = false;
                        else Visualizador.printString("Valor incorreto de latitude.");
                    }
                    flag = true;

                    while (flag) {
                        Visualizador.printString("Insira a longitude da sua empresa. (de -180 a 180)");
                        gpsy = Input.lerDouble();
                        if (gpsy > -181 && gpsy < 181) flag = false;
                        else Visualizador.printString("Valor incorreto de longitude.");
                    }
                    Visualizador.printString("Por favor, introduza o seu raio");
                    double raio = Input.lerDouble();
                    Visualizador.printString("Por favor, intruduza a sua palavra passe");
                    String passe = Input.lerString();
                    Visualizador.printString("Têm qualificações para tranporte de medicamentos?");
                    boolean med = Input.lerBoolean();
                    this.sge.newVoluntario("v"+cod, nome, new Gps(gpsx, gpsy), raio, new HashSet<>(), passe, true, med);
                    Visualizador.printString("Criação de Voluntario com sucesso");
                    break;
                case "3":
                    break;
                default:
                    Visualizador.printString("Comando Inválido. Tente de novo.");
                    break;

            }
        }
    }

    /**
     * Menu após dar sign in
     */
    public void menuVoluntario(){
        Visualizador.printString("Escolha uma opção:");
        Visualizador.printString("1| Aceitar uma encomenda.");
        Visualizador.printString("2| Histórico de entregas.");
        Visualizador.printString("3| Alterar a palavra passe.");
        Visualizador.printString("4| Voltar ao menu anterior.");


    }

    /**
     * Método que recebe o input do menu após dar sign in
     * @param codVoluntario
     * @param pass
     */
    public void runVoluntario(String codVoluntario, String pass){
        String input = "";
        String nova = "";
        while(!(input.equalsIgnoreCase("4"))) {
            menuVoluntario();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1":
                    boolean valida = true;
                    String resposta = "", encomenda = "";
                    List<String> enc = sge.voluntarioEncomendasNEntregues(codVoluntario);
                    while(valida){
                        if(enc.size() == 0) {
                            Visualizador.printString("Já aceitou/recusou todas as entregas.");
                            valida = false;
                        }
                        else {
                            encomenda = enc.get(0);
                            Visualizador.printString("Aceita esta entrega: " + encomenda.toString() + " ? (responda Y ou N)");
                            resposta = Input.lerString();
                            if (resposta.equalsIgnoreCase("Y")) {
                                sge.encomendasEntregues(encomenda);
                                this.sge.addTempoVol(encomenda, codVoluntario);
                                enc.remove(0);


                            } else if (resposta.equalsIgnoreCase("N")) {
                                enc.remove(0);
                                sge.retirarEncVoluntario(encomenda , codVoluntario);
                            }
                            else
                                Visualizador.printString("Comando incorreto. Repita, por favor.");
                        }
                    } break;
                case "2":
                    if(sge.historicoVol(codVoluntario).size() == 0) Visualizador.printString("Não há encomendas no histórico.");
                    else {
                        Visualizador.printString("Histórico de encomendas.");
                        Visualizador.printString(sge.historicoVol(codVoluntario).toString());
                    } break;
                case "3":
                    int r = 1;
                    Visualizador.printString("Intruduza a sua palavra passe");
                    String antiga = Input.lerString();
                    while (r == 1) {
                        if (antiga.equals(pass)) {
                            Visualizador.printString("Intruduza a sua nova palavra passe");
                            nova = Input.lerString();
                            Visualizador.printString("Intruza novamente a sua palavra passe");
                            String same = Input.lerString();
                            if (nova.equals(same)) {
                                Visualizador.printString("Palavra passe modificada com sucesso");
                                this.sge.setVoluntariosPass(codVoluntario ,nova);
                                r = 0;
                            } else Visualizador.printString("As palavras passes não correspondem, tente novamente");
                        }
                        else Visualizador.printString("Palavra passe incorreta, tente novamente");
                    } sge.alterarPassVoluntario(codVoluntario, nova);
                    break;
                case "4":
                    break;
                default:
                    Visualizador.printString("Comando Inválido. Tente de novo.");
                    break;


            }
        }
    }

}
