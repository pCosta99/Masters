package View;

import Controller.*;
import Exceptions.ComandoInvalidoException;

import java.io.IOException;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.util.*;

public class ViewUtilizador implements Viewable {

    private boolean sair;
    private Controller controller;

    /**
     * Funcao que imprime a lista de comandos disponiveis para o utilizador
     */
    public ViewUtilizador() {
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
        System.out.println("Para visualizar o seu perfil insira: perfil");
        System.out.println("Para efetuar uma nova encomenda efetue: encomendar");
        System.out.println("Para classificar uma encomenda (entre 0 e 10) insira: rank num_encomenda classificacao");
        System.out.println("Para visualizar as suas encomendas ativas insira: ativas");
        System.out.println("Para visualizar as suas encomendas entegues insira: entregues");
        System.out.println("Para visualizar os detalhes de uma encomenda insira: info numero_de_encomenda");
        System.out.println("Para visualizar a sua morada insira: morada");
        System.out.println("Para alterar a sua morada insira: altera morada");
        System.out.println("Para visualizar os tops insira: top [user,transp]");
        System.out.println("Para retroceder efetue: logout");
    }

    /**
     * Funcao responsavel pela execução dos comandos
     */
    public void run() {
        Scanner s = new Scanner(System.in);
        String command;
        String[] split;
        List<String> dados = new ArrayList<>();

        while (!sair) {
            printInfo();
            command = s.nextLine();
            split = command.split(" ");
            dados.clear();
            dados.addAll(Arrays.asList(split));

            boolean cancelada = false;

            switch (split[0]) {
                case "logout":
                    sair = true;
                    cancelada = true;
                    break;
                case "encomendar":

                    while(dados.size()>1) {
                        dados.remove(dados.size()-1);
                    }
                    System.out.println("|X|--------------------------------------------------|X|");
                    System.out.println("A sua encomenda é especial?");
                    System.out.println("Escreva [S] caso seja ou [N] caso contrario");
                    String letra = s.nextLine();
                    while(!letra.equals("S") && !letra.equals("s") && !letra.equals("N") && !letra.equals("n")) {
                        System.out.println("|X|--------------------------------------------------|X|");
                        System.out.println("Valor inválido. Por favor insira um dos valores válidos.");
                        letra = s.nextLine();
                    }
                    boolean especial = (letra.equals("S") || letra.equals("s"));
                    dados.add(String.valueOf(especial));

                    List<String> transpTemp = new ArrayList<>();
                    transpTemp.add("transportes");
                    transpTemp.add(String.valueOf(especial));

                    String transporte = null;

                    try {
                       transporte  = escolherTransp(controller.execute(transpTemp));
                    } catch (IOException | ComandoInvalidoException ignored) {}

                    if(transporte==null) {
                        View.printException("Encomenda cancelada.");
                        cancelada = true;
                        break;
                    }

                    dados.add(transporte);

                    boolean aEncomendar = true;
                    String temp;

                    while (aEncomendar) {
                        System.out.println("|X|--------------------------------------------------|X|");
                        System.out.println("Insira ANULAR para remover o ultimo produto.");
                        System.out.println("Insira CHECKOUT para finalizar a encomenda.");
                        System.out.println("Insira CANCELAR para cancelar a encomenda.");
                        System.out.println("-----------------------------------------------");
                        System.out.println("Insira o nome do produto que deseja encomendar.");
                        temp = s.nextLine();

                        switch (temp) {
                            case "ANULAR":
                                if (dados.size() > 3) {
                                    dados.remove(dados.size() - 1);
                                    dados.remove(dados.size() - 1);
                                    System.out.println("Produto anulado com sucesso.");
                                }
                                else System.out.println("Não há produtos para anular.");
                                break;
                            case "CHECKOUT":
                                if(dados.size() > 3) {
                                    System.out.println("A processar a sua encomenda...");
                                    aEncomendar = false;
                                }
                                else {
                                    System.out.println("Não é possivel fazer o checkout de uma encomenda vazia.");
                                }
                                break;
                            case "CANCELAR":
                                System.out.println("A sua encomenda foi cancelada.");
                                aEncomendar = false;
                                cancelada = true;
                                break;
                            default:
                                dados.add(temp);
                                boolean bool = true;
                                while(bool){
                                    try {
                                        System.out.println("Insira a quantidade do produto que deseja encomendar.");
                                        dados.add("" + Double.parseDouble(s.nextLine()));
                                        bool = false;
                                    }catch(Exception e){
                                        View.printException("Valor inválido!");
                                    }
                                }

                        }
                    }
                    break;
                case "info":
                case "top":
                    if(dados.size()<2) {
                        System.out.println("Erro: poucos argumentos.");
                        cancelada = true;
                    }
                    break;
                case "rank":
                    if(dados.size()<3) {
                        System.out.println("Erro: poucos argumentos.");
                        cancelada = true;
                    }
                    break;
                case "altera":
                    System.out.println("Para alterar a sua morada tem de permitir o uso da sua localização.");
                    System.out.println("Escreva [S] para aceitar ou [N] para recusar");
                    letra = s.nextLine();
                    while(!letra.equals("S") && !letra.equals("s") && !letra.equals("N") && !letra.equals("n")) {
                        System.out.println("Valor inválido. Por favor insira um dos valores válidos.");
                        letra = s.nextLine();
                    }
                    if (letra.equals("N") || letra.equals("n")) {
                        cancelada = true;
                        View.printException("Operação cancelada");
                    }
                    break;
                case "entregues":
                    while(dados.size()>1) {
                        dados.remove(dados.size()-1);
                    }

                    System.out.println("Deseja ver todas as encomendas ou só as efetuadas num determinado intervalo de tempo?");
                    System.out.println("Insira [1] para ver todas e [2] para ver num determinado intervalo de tempo ");

                    int opcao = s.nextInt();
                    s.nextLine();

                    switch(opcao) {
                        case 1:
                            break;
                        case 2:
                            System.out.println();
                            LocalDateTime di = View.escolherData("inicial");
                            LocalDateTime df = View.escolherData("final");
                            if(di.compareTo(df)>0) {
                                View.printException("Erro: A data final não pode preceder a data inicial.");
                                cancelada = true;
                            }
                            else {
                                dados.add(di.toString());
                                dados.add(df.toString());
                            }
                            break;
                        default:
                            View.printException("Opção Inválida!");
                            cancelada = true;
                    }
                default:
            }

            View.executeAndPrint(cancelada,dados,controller);

            View.retroceder(sair,s);

        }
    }

    /**
     * Função que faz o utilizador escolher um transporte de uma lista de transportes
     * @param transportes Lista de transportes
     * @return Transporte escolhido pelo utilizador
     */
    public String escolherTransp(List<String> transportes) {

        if(transportes.size()==0) {
            View.printException("Não há transportadoras ou voluntários disponiveis.");
            return null;
        }

        Scanner s = new Scanner(System.in);
        String[] split;
        String ret = null;
        boolean flag = true;
        int size = transportes.size(), i = 1;
        String temp;

        while(flag) {

            split = transportes.get(i-1).split(" ");
            StringBuilder sb = new StringBuilder();
            DecimalFormat df = new DecimalFormat();
            df.applyPattern("#0.##");

            for(int j=1;j<split.length-2;j++) {
                sb.append(split[j]).append(" ");
            }

            System.out.println();
            System.out.println("| |-----------------------------------------------------------------------| |");
            temp = df.format(Double.parseDouble(split[split.length-2]));
            System.out.println("Transporte " + i + " / " + size + ": " + sb.toString());
            System.out.println("Preço total do transporte: " + temp + "€");
            System.out.println("Classificação do transporte: " + split[split.length-1] + ".");
            System.out.println();
            System.out.println("Insira 1 para selecionar este transporte.");
            System.out.println("Insira 0 para cancelar a encomenda.");
            System.out.println("Insira + para ver o proximo ou - para ver o anterior.");


            String letra = s.nextLine();
            switch(letra) {
                case "1":
                    ret = split[0];
                    flag = false;
                    break;
                case "0":
                    flag = false;
                    break;
                case "+":
                    if(i<size) i++;
                    break;
                case "-":
                    if(i>1) i--;
                default:
            }
        }

        return ret;
    }

}
