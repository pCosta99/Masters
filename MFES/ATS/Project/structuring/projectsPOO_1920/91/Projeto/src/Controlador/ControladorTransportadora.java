/**
 * Esta classe é o controlador Transportadora
 */
package Controlador;

import Modelo.Gps;
import Modelo.Transportadora;
import Modelo.SistemaGestaoEntregas;
import Visualizador.Visualizador;

import java.io.Serializable;
import java.security.SecureRandomParameters;
import java.util.TreeSet;

public class ControladorTransportadora implements IControladorTransportadora, Serializable {
    SistemaGestaoEntregas sge;
    public ControladorTransportadora (SistemaGestaoEntregas sge){
        this.sge = sge;
    }

    /**
     * Menu inicial da transportadora
     */
    @Override
    public void menu(){
        Visualizador.printString("MENU TRANSPORTADORA");
        Visualizador.printString("1| Sign in.");
        Visualizador.printString("2| Sing up.");
        Visualizador.printString("3| Voltar ao menu.");
    }

    /**
     * Método que recebe o input do utilizador
     */
    @Override
    public void run(){
        String input = "";
        boolean valida = true;
        while(!(input.equalsIgnoreCase("3"))){
            menu();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1":
                    valida=true;
                    String codEmp = "", pass = "";
                    while (valida) {
                        Visualizador.printString("Introduza o seu código da empresa");
                        codEmp = Input.lerString();
                        Visualizador.printString("Introduza a palavra passe");
                        pass = Input.lerString();
                        if(!this.sge.existeTransportadora(codEmp, pass))
                            Visualizador.printString("Código de empresas ou palavra passe incorretos.");
                        else
                            valida = false;
                    }
                    runEmpresa(codEmp, pass);
                    break;
                case "2":
                    registarTransportadora(sge);
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
     * Método que regista uma nova transportadora
     * @param sge sistema gestao encomendas
     */
    public void registarTransportadora(SistemaGestaoEntregas sge) {
        String nome, codemp = "", pass1 = "", pass2 = "", medico = "";
        int cod;
        double raio, precokm, latitude = 0, longitude = 0, nif = 0, precokg;
        boolean flag = true;
        boolean ismedico = false;
        boolean disponivel = true;


        while (flag) {
            Visualizador.printString("Insira o código da sua empresa.(de 0 a 100)");
            cod = Input.lerInt();
            codemp = "t" + cod;
            if (cod<=0 || cod>=100|| sge.containsTransportadora(codemp)) {
                flag = true;
                Visualizador.printString("Código incorreto ou já existente. Introduza um diferente.");
            } else {
                flag = false;
                Visualizador.printString("Código aceite.");
            }
            flag = true;

            Visualizador.printString("Insira o nome da sua empresa.");
            nome = Input.lerString();

            while (flag) {
                Visualizador.printString("Insira a latitude da sua empresa.(de -180 a 180)");
                latitude = Input.lerDouble();
                if (latitude > -180 && latitude < 180) flag = false;
                else Visualizador.printString("Valor incorreto de latitude.");
            }
            flag = true;

            while (flag) {
                Visualizador.printString("Insira a longitude da sua empresa. (de -180 a 180)");
                longitude = Input.lerDouble();
                if (longitude > -181 && longitude < 181) flag = false;
                else Visualizador.printString("Valor incorreto de longitude.");
            }
            flag = true;

            while (flag) {
                Visualizador.printString("Insira o seu nif.");
                nif = Input.lerDouble();
                if (nif > 100000000 && nif < 999999999) flag = false;
                else Visualizador.printString("Nif incorreto , insira outro valor");
            }
            flag = true;
            String nifEmp = String.valueOf(nif);

            Visualizador.printString("Insira o raio no qual a sua empresa se movimenta.");
            raio = Input.lerDouble();

            Visualizador.printString("Insira o preço por Km.");
            precokm = Input.lerDouble();

            Visualizador.printString("Insira o preço por Kg.");
            precokg = Input.lerDouble();

            flag = true;
            while (flag) {
                Visualizador.printString("Insira a sua password.");
                pass1 = Input.lerString();

                Visualizador.printString("Insira novamente a sua password.");
                pass2 = Input.lerString();
                if (pass1.equals(pass2)) flag = false;
                else Visualizador.printString("As passwords não coincidem, tente novamente.");
            }
            flag = true;
            while (flag) {

                Visualizador.printString("Pretende transportar Encomendas Médicas (responda: Y ou N)");
                medico = Input.lerString();

                if (medico.equalsIgnoreCase("Y")) {
                    ismedico = true;
                    flag = false;
                } else if (medico.equalsIgnoreCase("N")) {
                    ismedico = false;
                    flag = false;
                } else Visualizador.printString("Resposta inválida. Tente novamente.");
            }

            disponivel = true;
            sge.newTransportadora(codemp, nome, latitude, longitude, nifEmp, raio, precokm, pass1, precokg, ismedico, disponivel, new TreeSet<>(),0, 0);
        }
    }


    /**
     * Menu da transportadora após dar sign in
     */
    public void menuTransportadora(){
        Visualizador.printString("Escolha uma opção:");
        Visualizador.printString("1| Mudar o status de transportadora médica (passar a transportar ou não encomendas médicas).");
        Visualizador.printString("2| Alterar disponibilidade.");
        Visualizador.printString("3| Alterar a palavra passe.");
        Visualizador.printString("4| ");
        Visualizador.printString("5| Voltar ao menu.");

    }

    /**
     * Método que recebe o input do utilizador após este ter dado sign in
     * @param codEmp codigo empresa
     * @param pass password
     */
    public void runEmpresa(String codEmp, String pass){
        String input = "";
        boolean flag = true;
        String resposta = "";
        while(!(input.equalsIgnoreCase("5"))){
            menuTransportadora();
            input = Input.lerString();
            switch (input.toLowerCase()){
                case "1" :
                    Visualizador.printString("Pretende transportar Encomendas Médicas (responda: Y ou N)");
                    resposta = Input.lerString();
                    while(flag){
                        if(resposta.equalsIgnoreCase("y")){
                            sge.getEmpresas().get(codEmp).setMedico(true);
                            flag = false;
                        }
                        if(resposta.equalsIgnoreCase("n")){
                            sge.getEmpresas().get(codEmp).setMedico(false);
                            flag = false;
                        }
                        else Visualizador.printString("Resposta inválida. Tente novamente.");
                    }
                    flag = true;
                    break;
                case "2" :
                    Visualizador.printString("Está disponível para entregas? (responda: Y ou N)");
                    flag = true;
                    resposta = Input.lerString();
                    while(flag){
                        if(resposta.equalsIgnoreCase("y")){
                            sge.getEmpresas().get(codEmp).setDisponivel(true);
                            flag = false;
                        }
                        if(resposta.equalsIgnoreCase("n")){
                            sge.getEmpresas().get(codEmp).setDisponivel(false);
                            flag = false;
                        }
                        else Visualizador.printString("Resposta inválida. Tente novamente.");
                    } break;
                case "3":
                    String password = "";
                    Visualizador.printString("Introduza a palavra passe antiga.");
                    password = Input.lerString();
                    while (!password.equals(pass)) {
                        Visualizador.printString("Palavra passe incorreta.");
                        Visualizador.printString("Introduza a palavra passe antiga.");
                        password = Input.lerString();
                    }
                    Visualizador.printString("Introduza a nova palavra passe");
                    String nova = Input.lerString();
                    Visualizador.printString("Introduza novamente a nova palavra passe");
                    String novaAgain = Input.lerString();
                    while (!nova.equals(novaAgain)) {
                        Visualizador.printString("Palavras passe diferentes.");
                        Visualizador.printString("Introduza a nova palavra passe.");
                        nova = Input.lerString();
                        Visualizador.printString("Introduza novamente a nova palavra passe.");
                        novaAgain = Input.lerString();
                    }
                    this.sge.alterarPassTransportadora(codEmp, nova);
                    break;
                case "4":
                    break;
                case "5":
                    break;
                default:
                    Visualizador.printString("Comando inválido. Tente de novo.");
            }
        }
    }
}

