package View;

import Controller.*;
import Exceptions.ComandoInvalidoException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class ViewLoja implements Viewable {

    private boolean sair;
    private Controller controller;


    public ViewLoja() {
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
        System.out.println("Para sinalizar que a ultima encomenda esta pronta para levantar insira: levantar");
        System.out.println("Para sinalizar que todas as encomendas estão prontas para levantar insira: levantar todas");
        System.out.println("Para sinalizar que um transporte levantou as suas encomendas insira: levantadas codigo_transporte");
        System.out.println("Para visualizar as suas encomendas ativas insira: ativas");
        System.out.println("Para visualizar as suas encomendas prontas insira: prontas");
        System.out.println("Para visualizar todos os clientes na loja insira: clientes");
        System.out.println("Para verificar se partilha a sua fila de espera insira: partilha");
        System.out.println("Para alterar a partilha da sua fila de espera insira: altera partilha");
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
        boolean tryException;

        while (!sair) {
            tryException = true;
            printInfo();
            command = s.nextLine();
            split = command.split(" ");
            dados.clear();
            dados.addAll(Arrays.asList(split));


            String temp;

            boolean aSinalizar = true, cancelada = false;


            switch (split[0]) {
                case "logout":
                    sair = true;
                    cancelada = true;
                    break;
                case "levantadas":
                case "altera":
                case "top":
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
