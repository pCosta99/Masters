 /**
 * Esta classe é o controlador loja, depois de entrar comouma loja no controlador geral
 */
package Controlador;

import Modelo.Gps;
import Modelo.Produto;
import Modelo.SistemaGestaoEntregas;
import Visualizador.Visualizador;

import java.io.Serializable;
import java.util.*;

public class ControladorLoja implements IControladorLoja, Serializable {
    SistemaGestaoEntregas sge;

    public ControladorLoja(SistemaGestaoEntregas sge) {
        this.sge = sge;
    }

    /**
     * Método que recebe o input do Menu lojas
     */
    @Override
    public void run() {
        String input = "";
        while (!(input.equalsIgnoreCase("3"))) {
            menu();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1":
                    Visualizador.printString("Introduza o seu código da loja");
                    String codLoja = Input.lerString();
                    Visualizador.printString("Introduza a sua password");
                    String password = Input.lerString();
                    while (!this.sge.existeLoja(codLoja, password)) {
                        Visualizador.printString("Código de loja ou palavra passe incorretos");
                        Visualizador.printString("Introduza o seu código da loja");
                        codLoja = Input.lerString();
                        Visualizador.printString("Introduza a sua password");
                        password = Input.lerString();
                    }
                        runLoja(codLoja ,password);
                    break;
                case "2":
                    registar();
                    break;
                case "3":
                    break;
                default:
                    Visualizador.printString("Comando inválido. Tente novamente.");
                    break;
            }

        }
    }

    /**
     * Método que imprime o menu lojas
     */
    @Override
    public void menu() {
        Visualizador.printString("MENU LOJAS");
        Visualizador.printString("1| Sign in.");
        Visualizador.printString("2| Sign up.");
        Visualizador.printString("3| Voltar ao menu anterior.");
    }

    /**
     * Método que regista uma nova loja
     */
    public void registar() {
        boolean valida = true;
        boolean flag = true;
        boolean val = true;
        int codLoja =0;
        double gpsx = 0 , gpsy=0;
        int codProduto = 0;
        Produto p = new Produto();
        String descricao ="";
        boolean medico = false;
        double valorUnitário = 0;
        Set<Produto> catalogo = new HashSet<>();
        String nome = "";
        String resposta = "";
        while (valida) {
            Visualizador.printString("Introduza um código para a sua loja (de 0 a 100)");
            codLoja = Input.lerInt();
            if (codLoja<=0 || codLoja>=100 || sge.temCodLoja("l"+codLoja)){
                Visualizador.printString("Código incorreto ou já existente. Introduza outro por favor.");
            }
            else {
                Visualizador.printString("Código aceite. O seu código é l" + codLoja + ".");
                Visualizador.printString("Introduza o nome da sua loja");
                nome = Input.lerString();
                while (val) {
                    Visualizador.printString("Insira a latitude da sua empresa.(de -180 a 180)");
                    gpsx = Input.lerDouble();
                    if (gpsx > -180 && gpsx < 180) val = false;
                    else Visualizador.printString("Valor incorreto de latitude.");
                }
                val = true;

                while (val) {
                    Visualizador.printString("Insira a longitude da sua empresa. (de -180 a 180)");
                    gpsy = Input.lerDouble();
                    if (gpsy > -181 && gpsy < 181) val = false;
                    else Visualizador.printString("Valor incorreto de longitude.");
                }
                valida = false;
            }
        }
        valida = true;
        boolean disponivel = false;
        while(valida ){
            Visualizador.printString("Pretende sinalizar já que está disponível? (Y/N)");
            resposta = Input.lerString();
            if (resposta.equalsIgnoreCase("y")){
                disponivel = true;
                Visualizador.printString("Sinalizado como disponível..");
                valida = false;
            }
            else if (resposta.equalsIgnoreCase("n")){
                Visualizador.printString("Sinalizado como indisponível.");
                valida = false;
            }
            else Visualizador.printString("Comando inválido. Responda com Y/N.");
        }

        Visualizador.printString("Introduza uma palavra passe");
        String password = Input.lerString();
        Visualizador.printString("Introduza novamente a palavra passe.");
        String passwordAgain = Input.lerString();
        while (!password.equals(passwordAgain)) {
            Visualizador.printString("Palavras passe diferentes.");
            Visualizador.printString("Introduza uma palavra passe");
            password = Input.lerString();
            Visualizador.printString("Introduza novamente a palavra passe.");
            passwordAgain = Input.lerString();
        }
        Visualizador.printString("Acrescente o seu catálogo.");
        valida = true;
        while(flag){
            Visualizador.printString("Novo produto.");
            Visualizador.printString("Introduza um código de produto (0 a 100)");
            codProduto = Input.lerInt();
            if(codProduto<=0 || codProduto>=100 || sge.temCodProduto("p"+codProduto)){
                Visualizador.printString("Código de produto incorreto , ou já existente. Introduza outro , por favor.");
            }
            else
            { Visualizador.printString("Código aceite. Este produto tem o código p" + codProduto + ".");
                Visualizador.printString("Introduza a descrição desse produto.");
                descricao=Input.lerString();
                Visualizador.printString("Introduza o valor unitário desse produto.");
                valorUnitário = Input.lerDouble();
                while (valida){
                    Visualizador.printString("Caracteriza este produto como um produto médico? (Responda com Y/N)");
                    resposta = Input.lerString();
                    if(resposta.equalsIgnoreCase("y")){
                        medico = true;
                        valida = false;
                    }
                    else if (resposta.equalsIgnoreCase("n")){
                        valida = false;
                    }
                    else{
                        Visualizador.printString("Comando inválido. Responda com Y/N.");
                    }
                }
                p = sge.newProduto("p"+codProduto ,descricao , valorUnitário , medico );
                catalogo.add(p);
                valida = true;
                while (valida) {
                    Visualizador.printString("Deseja acrescentar mais produtos ao seu catálogo?");
                    resposta = Input.lerString();
                    if (resposta.equalsIgnoreCase("n")){
                        flag = false;
                        valida = false;
                    }
                    else if (resposta.equalsIgnoreCase("y")) valida = false;
                    else {
                        Visualizador.printString("Comando inválido. Responda com Y/N.");
                    }
                }
            }

        }

        this.sge.newLoja(("l"+ codLoja), nome, new Gps(gpsx, gpsy), disponivel, password, catalogo);
    }

    /**
     * Menu da loja após dar sign in
     */
    public void menuLoja() {
        Visualizador.printString("Escolha uma opção:");
        Visualizador.printString("1| Estado de disponibilidade.");
        Visualizador.printString("2| Aceitar encomenda.");
        Visualizador.printString("3| Quantidade de pessoas em fila de espera.");
        Visualizador.printString("4| Total faturado.");
        Visualizador.printString("5| Alterar a palavra passe.");
        Visualizador.printString("6| voltar ao menu.");
    }

    /**
     * Método que recebe o input da loja após dar sign in
     * @param codLoja
     * @param pass
     */
    public void runLoja(String codLoja, String pass) {
        String input = "";
        boolean valida = true;

        while (!(input.equalsIgnoreCase("6"))) {
            menuLoja();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1":
                    valida = true;
                    while(valida ){
                        Visualizador.printString("Pretende alterar o seu estado de disponibilidade? (Y/N)");
                        String resposta = Input.lerString();
                        if (resposta.equalsIgnoreCase("y")){
                            this.sge.sinalizarDisponibilidade(codLoja);
                            Visualizador.printString("Disponibilidade alterada.");
                            valida = false;
                        }
                        else if (resposta.equalsIgnoreCase("n")){
                            Visualizador.printString("Estado de disponibilidade inalterado.");
                            valida = false;
                        }
                        else Visualizador.printString("Comando inválido. Responda com Y/N.");
                    }

                    this.sge.sinalizarDisponibilidade(codLoja);
                    break;

                case "2":
                    valida = true;
                    String resposta = "", encomenda = "";
                    List<String> enc = sge.lojaEncomendasNAceites(codLoja);
                    while(valida){
                        if(enc.size() == 0) {
                            Visualizador.printString("Já aceitou/recusou todas as encomendas.");
                            valida = false;
                        }
                        else {
                            encomenda = enc.get(0);
                            Visualizador.printString("Aceita a encomenda " + enc.toString() + " ? (responda Y ou N)");
                            resposta = Input.lerString();
                            if (resposta.equalsIgnoreCase("Y")) {
                                sge.addAceite(encomenda);
                                sge.addEncomendas(codLoja , encomenda);
                                enc.remove(0);

                            } else if (resposta.equals("N")) {
                            enc.remove(0);
                            }
                            else Visualizador.printString("Comando incorreto, tente de novo.");
                        }
                    } break;
                case "3":
                    Visualizador.printString("A quantidade de pessoas em fila de espera é " + sge.tamanhoFilaEspera(codLoja));
                    break;
                case "4":
                    Visualizador.printString("Total faturado: " + sge.totalFaturado(codLoja));
                    break;
                case "5":
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
                    while (!nova.equals(novaAgain)){
                        Visualizador.printString("Palavras passe diferentes.");
                        Visualizador.printString("Introduza a nova palavra passe.");
                        nova = Input.lerString();
                        Visualizador.printString("Introduza novamente a nova palavra passe.");
                        novaAgain = Input.lerString();
                    }
                    this.sge.alterarPassLoja(nova , codLoja);
                    break;
                case "6":
                    break;
                default:
                    Visualizador.printString("Comando inválido.");
            }
        }
    }
}

