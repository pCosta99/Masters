package View;

import Controller.*;
import Exceptions.ComandoInvalidoException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;



public class ViewVoluntario implements Viewable {

    private boolean sair;
    private Controller controller;


    public ViewVoluntario() {
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
        System.out.println("Para verificar o tempo previsto de espera na fila de uma loja insira: espera codigo_loja");
        System.out.println("Para sinalizar o levantamento de uma encomenda insira: levantar codigo_loja");
        System.out.println("Para sinalizar a entrega de uma encomenda insira: confirmar");
        System.out.println("Para visualizar a  encomenda na sua posse insira: encomenda");
        System.out.println("Para visualizar a lista de lojas onde tem encomendas insira: lojas");
        System.out.println("Para visualizar as suas encomendas entegues insira: entregues");
        System.out.println("Para visualizar o seu raio de entregua atual insira: raio");
        System.out.println("Para alterar o seu raio de entrega insira: raio novo_valor");
        System.out.println("Para verificar se faz encomendas especiais insira: especiais");
        System.out.println("Para começar ou deixar de fazer encomendas especiais insira: altera especiais");
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

        while(!sair) {
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
                case "espera":
                case "levantar":
                case "top":
                case "clientes":
                case "altera":
                    if(dados.size()<2) {
                        System.out.println("Erro: poucos argumentos.");
                        cancelada = true;
                    }
                    break;
                default:
            }

            View.executeAndPrint(cancelada,dados,controller);

            View.retroceder(sair,s);

        }
    }
}