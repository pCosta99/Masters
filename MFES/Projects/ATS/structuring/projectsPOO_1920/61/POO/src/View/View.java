package View;

import Controller.*;
import Exceptions.ComandoInvalidoException;
import Model.*;

import java.io.IOException;
import java.security.InvalidParameterException;
import java.time.LocalDateTime;
import java.util.*;

public class View implements Viewable {

    private boolean sair;
    private Controller controller;

    /**
     * Construtor da classe
     */
    public View() {
        sair = false;
    }

    /**
     * Define o controlador da classe
     */
    public void setController(Controller controller) {
        this.controller = controller;
    }

    /**
     * Retorna o controlador da classe
     */
    public Controller getController() {
        return this.controller;
    }

    /**
     * Funcao que imprime a lista de comandos disponiveis para o utilizador
     */
    public void printInfo() {
        System.out.println("Para dar login efetue: login username password");
        System.out.println("Para registar uma nova conta efetue: registo email password");
        System.out.println("Para carregar os dados de um ficheiro insira: load");
        System.out.println("Para guardar os dados num ficheiro insira: save");
        System.out.println("Para sair: sair");
    }

    /**
     * Função que pede ao utilizador os dados requiridos para a criação de uma conta
     * @param dados iniciais
     * @return dados necessários ao registo
     */
    public List<String> registo(List<String> dados) {
        Scanner scan = new Scanner(System.in);
        String letra;
        boolean bool;

        System.out.println("Insira o seu nome completo");
        String nome = scan.nextLine();
        while(nome.equals("\n")) {
            System.out.println("Compo vazio. Por favor insira um valor válido.");
            nome = scan.nextLine();
        }
        dados.add(nome);

        System.out.println("Para ativar a sua conta tem de permitir o uso da sua localização.");
        System.out.println("Escreva [S] para aceitar ou [N] para recusar");
        letra = scan.nextLine();
        while(!letra.equals("S") && !letra.equals("s") && !letra.equals("N") && !letra.equals("n")) {
            System.out.println("Valor inválido. Por favor insira um dos valores válidos.");
            letra = scan.nextLine();
        }
        if (letra.equals("N") || letra.equals("n")) {
            ArrayList<String> ret = new ArrayList<>();
            ret.add("O seu registo foi cancelado");
            return ret;
        }

        System.out.println("Insira o tipo de conta a criar de entre os tipos disponieveis:\nU -> Utilizador;\nV -> Voluntario;\nT -> Transportadora;\nL -> Loja;");
        String c = scan.nextLine();
        dados.add(String.valueOf(c));

        LocalCodeName user;
        Double raio;

        switch (c) {
            case "U":
            case "u":
                break;
            case "V":
            case "v":
                System.out.println("Insira o seu raio de entrega");
                raio = scan.nextDouble();
                scan.nextLine();
                dados.add(String.valueOf(raio));

                System.out.println("Deseja realizar entregas de encomendas especiais?");
                System.out.println("Escreva [S] caso deseje ou [N] caso contrario");
                letra = scan.nextLine();
                while(!letra.equals("S") && !letra.equals("s") && !letra.equals("N") && !letra.equals("n")) {
                    System.out.println("Valor inválido. Por favor insira um dos valores válidos.");
                    letra = scan.nextLine();
                }
                bool = (letra.equals("S") || letra.equals("s"));
                dados.add(String.valueOf(bool));

                break;
            case "T":
            case "t":
                do {
                    raio = 0.0;
                    System.out.println("Insira o seu raio de entrega");
                    try {
                        raio = scan.nextDouble();
                        scan.nextLine();
                        dados.add(String.valueOf(raio));
                    } catch (Exception e) {
                        scan.nextLine();
                        printException("Valor inválido. Tente novamente.");
                    }
                } while(raio.equals(0.0));


                System.out.println("Insira o seu NIF");
                String nif = scan.nextLine();
                while(nif.equals("\n")) {
                    System.out.println("Compo vazio. Por favor insira um valor válido.");
                    nif = scan.nextLine();
                }
                dados.add(nif);

                Double prec_km;
                do {
                    System.out.println("Insira o seu preco por km");
                    prec_km = 0.0;
                    try {
                        prec_km = scan.nextDouble();
                        scan.nextLine();
                        dados.add(String.valueOf(prec_km));
                    } catch (Exception e) {
                        scan.nextLine();
                        printException("Valor inválido. Tente novamente.");
                    }
                } while(prec_km.equals(0.0));
                System.out.println("Deseja realizar entregas de encomendas especiais?");
                System.out.println("Escreva [S] caso deseje ou [N] caso contrario");
                letra = scan.nextLine();
                while(!letra.equals("S") && !letra.equals("s") && !letra.equals("N") && !letra.equals("n")) {
                    System.out.println("Valor inválido. Por favor insira um dos valores válidos.");
                    letra = scan.nextLine();
                }
                bool = (letra.equals("S") || letra.equals("s"));
                dados.add(String.valueOf(bool));

                break;
            case "L":
            case "l":
                System.out.println("Deseja partilhar a informacao da sua fila de espera?");
                System.out.println("Escreva [S] caso aceite ou [N] caso contrario");
                letra = scan.nextLine();
                while(!letra.equals("S") && !letra.equals("s") && !letra.equals("N") && !letra.equals("n")) {
                    System.out.println("Valor inválido. Por favor insira um dos valores válidos.");
                    letra = scan.nextLine();
                }
                bool = (letra.equals("S") || letra.equals("s"));
                dados.add(String.valueOf(bool));

                break;
            default:
                ArrayList<String> ret = new ArrayList<>();
                ret.add("Opcao Invalida: O seu registo foi cancelado");
                return ret;
        }

        return dados;
    }

    /**
     * Dá print a uma string a vermelho
     * @param string String a imprimir
     */
    public static void printException(String string) {
        System.out.println("\u001B[31m" + string + "\u001B[0m");
    }

    /**
     * Funcao responsavel pela execução dos comandos
     */
    public void run() {
        Scanner s = new Scanner(System.in);

        String command;
        String[] split;
        List<String> dados = new ArrayList<>();
        List<String> ret;

        while (!sair) {
            printInfo();
            command = s.nextLine();
            split = command.split(" ");
            dados.clear();
            dados.addAll(Arrays.asList(split));

            switch (dados.get(0)) {
                case "registo":
                    ret = registo(dados);
                    try {
                        ret = controller.execute(ret);
                    } catch (IOException e) {
                        printException("O username inserido já existe!");
                        break;
                    } catch (InvalidParameterException e) {
                        printException("registo username password");
                        break;
                    } catch (ComandoInvalidoException e) {
                        printException("Comando inválido!");
                        break;
                    }
                    System.out.println("\n");
                    System.out.println(ret.get(0));
                    System.out.println();
                    System.out.println("|X|--------------------------------------------------|X|");
                    break;
                case "login":
                    try {
                        ret = controller.execute(dados);
                    } catch (InvalidParameterException e) {
                        printException("Por favor insira o seu username e password!");
                        break;
                    } catch (IOException t) {
                        printException("Username ou password inválido!");
                        break;
                    } catch (ComandoInvalidoException e) {
                        printException("Comando inválido!");
                        break;
                    }

                    System.out.println("\n");
                    System.out.println(ret.get(0));
                    System.out.println();
                    System.out.println("|X|--------------------------------------------------|X|");

                    Viewable newView;
                    Controller newController;

                    switch (ret.get(1)) {
                        case "Utilizador":
                            newView = new ViewUtilizador();
                            newController = new ControllerUtilizador(controller.getSistema(), ret.get(2));
                            break;
                        case "Voluntario":
                            newView = new ViewVoluntario();
                            newController = new ControllerVoluntario(controller.getSistema(), ret.get(2));
                            break;
                        case "Transportadora":
                            newView = new ViewTransportadora();
                            newController = new ControllerTransportadora(controller.getSistema(), ret.get(2));
                            break;
                        default:
                            newView = new ViewLoja();
                            newController = new ControllerLoja(controller.getSistema(), ret.get(2));
                    }
                    newView.setController(newController);
                    newView.run();
                    controller.setSistema(newView.getController().getSistema());
                    break;
                case "load":
                case "save":
                    executeAndPrint(false,dados,controller);
                    break;
                case "sair":
                    sair = true;
                    break;
                default:
                    printException("Comando invalido");
                    break;
            }
        }
    }

    /**
     * Função que executa um comando e dá print ao seu resultado
     * @param cancelada Informação se o comando é para ser cancelado
     * @param dados Dados a passar ao controlador
     * @param controller Controlador
     */
    public static void executeAndPrint(boolean cancelada, List<String> dados, Controller controller) {
        System.out.println("\n");
        if(!cancelada) {
            List<String> toPrint = null;
            try {
                toPrint = controller.execute(dados);
            } catch (IOException e) {
                View.printException("Email ou password errados!");
                return;
            } catch (ComandoInvalidoException e) {
                View.printException("Comando Inválido!");
                return;
            }
            toPrint.forEach(System.out::println);
            System.out.println();
        }
        System.out.println("|X|--------------------------------------------------|X|");
    }

    /**
     * Função que dá clear ao terminal
     */
    public static void clearScreen() {
        System.out.println("|X|--------------------------------------------------|X|");
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }

    /**
     * Função que permite ao utilizador dizer quando quer retroceder
     * @param sair caso o utilizador esteja a sair nao precisa de retroceder
     * @param s scanner a utilizar
     */
    public static void retroceder(boolean sair, Scanner s) {
        if(!sair) {
            System.out.println("Para retroceder pressione 0.");
            s.nextLine();
            View.clearScreen();
        }
    }

    /**
     * Função que cria uma LocalDateTime baseada numa string inserida pelo utilizador
     * @param toPrint informação sobre a data a criar
     * @return Data
     */
    public static LocalDateTime escolherData(String toPrint) {
        Scanner s = new Scanner(System.in);
        String[] split;
        LocalDateTime ret;

        System.out.println("Insira a data " + toPrint + ", no formato DD/MM/YYYY.");
        split = s.nextLine().split("/");
        System.out.println("\n");
        while(split.length!=3) {
            View.printException("Data inválida!");
            System.out.println("Insira a data " + toPrint + ", no formato DD/MM/YYYY.");
            split = s.nextLine().split("/");
            System.out.println("\n");
        }


        try {
            if (toPrint.equals("inicial")) ret = LocalDateTime.of(Integer.parseInt(split[2]), Integer.parseInt(split[1]), Integer.parseInt(split[0]), 0, 0);
            else ret = LocalDateTime.of(Integer.parseInt(split[2]), Integer.parseInt(split[1]), Integer.parseInt(split[0]), 23, 59);
        } catch (Exception e) {
            View.printException("Data inválida!");
            ret = escolherData(toPrint);
        }

        return ret;
    }

}