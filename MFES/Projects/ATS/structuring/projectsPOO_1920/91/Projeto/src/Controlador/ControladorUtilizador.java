/**
 * Esta classe é o controlador do utilizador
 */
package Controlador;

import Modelo.*;
import Visualizador.Visualizador;

import javax.xml.stream.events.EndElement;
import java.io.Serializable;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;
import java.util.Random;

public class ControladorUtilizador implements IControladorUtilizador, Serializable {
    SistemaGestaoEntregas sge;

    public ControladorUtilizador(SistemaGestaoEntregas sge) {
        this.sge = sge;
    }

    /**
     * Menu inicial antes de dar sign in
     */
    @Override
    public void menu() {
        Visualizador.printString("MENU UTILIZADOR");
        Visualizador.printString("1| Sign in.");
        Visualizador.printString("2| Sing up.");
        Visualizador.printString("3| Voltar ao menu.");
    }

    /**
     * Método que recebe o input do utilizador
     */
    @Override
    public void run() {
        String input = "";
        boolean valida = true;
        while (!(input.equalsIgnoreCase("3"))) {
            menu();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1":
                    valida = true;
                    String codUtilizador = "", pass = "";
                    while (valida) {
                        Visualizador.printString("Introduza o seu código de utilizador");
                        codUtilizador = Input.lerString();
                        Visualizador.printString("Introduza a palavra passe");
                        pass = Input.lerString();
                        if (!this.sge.existeUtilizador(codUtilizador, pass))
                            Visualizador.printString("Código de utilizador ou palavra passe incorretas.");
                        else
                            valida = false;
                    }
                    runCliente(codUtilizador, pass);
                    break;
                case "2":
                    registar();
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
     * Método que escolhe a transposrtadora que começa por escolher voluntarios e se n estiver nenhum disponivel passa para as empresas
     * @param codUtilizador
     * @param encCode
     */
    public void escolherTransportador(String codUtilizador, String encCode) {
        Set<String> voluntarios = sge.possiveisVoluntarios(encCode);
        Set<String> transp = sge.possiveisTransportadoras(encCode);
        String code;
        boolean val = true;

        if (voluntarios.size() != 0) {
            code = sge.escolheVoluntario(encCode, voluntarios);
            Visualizador.printString("Foi solicitado o voluntário " + code + ".");
            sge.adicionaEncVoluntario(encCode, code);

        } else {
            while (val) {

                if (transp.size() == 0) {
                    Visualizador.printString("Sem transportadores disponiveis!");
                    val = false;
                } else {
                    code = sge.escolheTransportadora(encCode, transp);
                    Visualizador.printString("Aceita o transportador " + code + " pelo preço de " + sge.precoEncomenda(encCode, code) + "? (Y/N) ");
                    String input = Input.lerString();
                    if (input.equalsIgnoreCase("y")) {
                        Visualizador.printString("A encomenda " + encCode + " foi entregue pela transportadora " + code + " pelo preço de " + sge.precoEncomenda(encCode, code));
                        this.sge.addTempoTrans(encCode, code);
                        Visualizador.printString("Deseja classificar a empresa ? (Y/N)");
                        String input2 = Input.lerString();
                        if (input.equalsIgnoreCase("y")) {
                            Visualizador.printString("De 0 a 5 quanto deseja classificar esta transportadora?");
                            int classificacao = Input.lerInt();
                            while (classificacao < 0 || classificacao > 5) {
                                Visualizador.printString("A classificação tem de ser entre 0 e 5, por favor tente novamente");
                                classificacao = Input.lerInt();
                            }
                            sge.classificacao(code, classificacao);
                            Visualizador.printString("Obrigado pela sua cooperação");
                        }
                        val = false;
                        sge.addEntregueTransportadora(code, encCode);
                        sge.setDataEntrega(encCode);
                    } else if (input.equalsIgnoreCase("n")) {
                        sge.removeTransportadora(transp, code);
                        Visualizador.printString("Vamos escolher outra transportadora.");
                    } else Visualizador.printString("Comando inválido. Responda com Y/N.");

                }
            }
        }
    }

    /**
     * Método que regista o utilizador
     */
    public void registar() {
        boolean valida = true;
        int cod = 0;
        double gpsx = 0;
        double gpsy = 0;
        boolean flag = true;
        String nome = "";
        while (valida) {
            Visualizador.printString("Introduza um código de utilizador (de 0 a 100)");
            cod = Input.lerInt();
            if(cod<=0 || cod>=100 || sge.temCodUtilizador("u"+cod) ){
                Visualizador.printString("Código incorreto ou já utilizado , introduza outro por favor.");
            }
            else {
                Visualizador.printString("Código aceite. O seu código de utilizador é u" + cod + ".");
                Visualizador.printString("Introduza um nome");
                nome = Input.lerString();
                while (flag) {
                    Visualizador.printString("Insira a latitude da sua empresa.(de -180 a 180)");
                    gpsx = Input.lerDouble();
                    if (gpsx > -180 && gpsx < 180) flag = false;
                    else {
                        Visualizador.printString("Valor incorreto de latitude.");
                    }
                }
                    flag = true;
                while (flag) {
                    Visualizador.printString("Insira a longitude da sua empresa. (de -180 a 180)");
                    gpsy = Input.lerDouble();
                    if (gpsy > -181 && gpsy < 181) flag = false;
                    else Visualizador.printString("Valor incorreto de longitude.");
                }
                valida = false;
            }

        }
        valida = true;

        while (valida) {
            Visualizador.printString("Introduza uma palavra passe");
            String password = Input.lerString();
            Visualizador.printString("Introduza novamente a palavra passe");
            String passwordAgain = Input.lerString();
            if (!password.equals(passwordAgain))
                Visualizador.printString("Palavras passe diferentes.");
            else
                valida = false;
        }
        this.sge.newUtilizador(("u"+ cod), nome, new Gps(gpsx, gpsy), "u"+cod, new TreeSet<>());
    }

    /**
     * Menu cliente após dar sign in
     */
    public void menuCliente() {
        Visualizador.printString("Escolha uma opção:");
        Visualizador.printString("1| Fazer nova encomenda.");
        Visualizador.printString("2| Histórico de encomendas.");
        Visualizador.printString("3| Alterar a palavra passe.");
        Visualizador.printString("4| Lojas disponíveis. ");
        Visualizador.printString("5| Solicitar entrega. ");
        Visualizador.printString("6| Voltar ao menu anterior.");
    }

    /**
     * Método que recebe o input do menu após dar sign in
     * @param codUtilizador
     * @param pass
     */
    public void runCliente(String codUtilizador, String pass) {
        String input = "";
        while (!(input.equalsIgnoreCase("6"))) {
            menuCliente();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1":
                    novaEncomenda(codUtilizador);
                    break;
                case "2":
                    runHistórico(codUtilizador);
                    break;
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
                    this.sge.alterarPassword(nova, codUtilizador);
                    break;
                case "4":
                        Visualizador.printString(sge.lojasDisponiveis());
                    break;
                case "5":
                    boolean val = true;
                    while (val) {
                        List<String> string = this.sge.encomendasPorEntregar(codUtilizador);
                        Visualizador.printString(string.toString());
                        if(string.size() == 0) Visualizador.printString("Não há encomendas por entregar.");
                        Visualizador.printString("Escolha uma encomenda para ser entregue.");
                        String encomenda = Input.lerString();
                        if (string.contains(encomenda)) {
                            escolherTransportador(codUtilizador, encomenda);
                            val = false;
                        } else {
                            Visualizador.printString("Essa encomenda não está disponível para ser entregue.");
                        }
                    }
                    break;
                case "6":
                    break;
                default:
                    Visualizador.printString("Comando inválido. Tente de novo.");
            }
        }
    }

    /**
     * Método que faz uma nova encomenda
     * @param codUtilizador
     */
    public void novaEncomenda(String codUtilizador) {
        boolean val = true;
        boolean condicao = true;
        int i = 1 ;
        while (val) {
            Encomenda e = new Encomenda();
            Visualizador.printString("Lojas disponíveis: \n");
            Map<String, String> lojas = this.sge.lojasDisponiveis();
            Visualizador.printString(lojas.toString());
            Visualizador.printString("Escolha uma loja. (Pelo código de loja) ");
            String input = Input.lerString();
            if (lojas.containsKey(input)) {
                if (this.sge.imprimeCatalogo(input).size() == 0){
                    Visualizador.printString("Esta loja não tem produtos.");
                    i=0;
                }
                if (i == 1) {
                    Visualizador.printString("Produtos da loja: ");
                    Visualizador.printString(this.sge.imprimeCatalogo(input).toString());
                    Set<String> codProdutos = this.sge.imprimeCatalogo(input).stream().map(Produto::getCodProduto).collect(Collectors.toSet());
                    while (condicao) {
                        LinhaEncomenda linhaEncomenda = new LinhaEncomenda();
                        Visualizador.printString("Escolha um produto : ");
                        String produto = Input.lerString();
                        if (codProdutos.contains(produto)) {
                            Visualizador.printString("Qual é a quantidade que pretende encomendar?");
                            double quantidade = Input.lerDouble();
                            linhaEncomenda = this.sge.novaLinhaEncomenda(this.sge.imprimeCatalogo(input), produto, quantidade);
                            e.addLinhaEncomenda(linhaEncomenda);
                            Visualizador.printString("Pretende escolher mais produtos? (Se sim, responda com Y)");
                            String resposta = Input.lerString();
                            if (!resposta.equalsIgnoreCase("Y")) {
                                condicao = false;
                                Random rand = new Random();
                                int codEncomenda = rand.nextInt(10000);
                                boolean codigo = true;
                                while (codigo) {
                                    if (this.sge.containsKey((double)codEncomenda)) {
                                        codEncomenda = rand.nextInt();
                                    } else codigo = false;
                                }
                                e.setCodEncomenda("e" + codEncomenda);
                                e.setCodUtilizador(codUtilizador);
                                e.setEntregue(false);
                                e.setCodLoja(input);
                                e.setAceite(false);
                                double peso = rand.nextDouble();
                                e.setPeso(peso);
                                e.setMedico(e.isMedico());
                                this.sge.addEncomenda(e);
                                this.sge.addEncomenda(codUtilizador, input, e);
                                Visualizador.printString("Encomenda adicionada. Se quiser solicitar a sua entrega, escolha essa opção no menu.");
                                val = false;
                            }
                        } else Visualizador.printString("Código incorreto de produto, escolha um produto desta loja.");
                    }
                }
            } else Visualizador.printString("Código de loja incorreto , escolha um dos códigos disponíveis.");
        }
    }

    /**
     * Menu do historico de encomendas
     */
    public void menuHistorico() {
        Visualizador.printString("1| Histórico total.");
        Visualizador.printString("2| Histórico num período do tempo.");
        Visualizador.printString("3| Menu anterior.");
    }

    /**
     * Método que recebe o input do menu historico de encomendas
     * @param codUtilizador
     */
    public void runHistórico(String codUtilizador) {
        String input = "";
        boolean valida = true;
        LocalDate min = LocalDate.now();
        LocalDate max = LocalDate.now();
        while (!(input.equalsIgnoreCase("3"))) {
            menuHistorico();
            input = Input.lerString();
            switch (input.toLowerCase()) {
                case "1":
                    Visualizador.printString(sge.encomendasUtilizador(codUtilizador).toString());
                    break;
                case "2" :
                    valida = true;
                    while (valida){
                        try {
                            Visualizador.printString("Introduza o ano , mês e dia do mês. (Data mínima)");
                            int ano = Input.lerInt();
                            int mes = Input.lerInt();
                            int dia = Input.lerInt();
                            min = LocalDate.of(ano, mes, dia);
                            Visualizador.printString("Introduza o ano , mês e dia do mês. (Data máxima)");
                            int ano1 = Input.lerInt();
                            int mes1 = Input.lerInt();
                            int dia1 = Input.lerInt();
                            max = LocalDate.of(ano1, mes1, dia1);
                            valida = false;
                        } catch(DateTimeException dateTimeException){
                            Visualizador.printString("Data/Datas incorretas, tente de novo.");
                        }
                        Visualizador.printString("Encomendas no período de tempo escolhido.");
                        Visualizador.printString(sge.encomendasPorData(codUtilizador , max , min).toString());

                    }
                    break;
                    case "3":
                        break;
                    default:
                      Visualizador.printString("Comando inválido. Tente de novo.");
            }
        }
    }
}

