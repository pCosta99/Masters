/**
 * Classe que controla o programa
 */
package Controler;

import Files.GuardarCarregarEstado;
import Model.*;
import View.Apresentacao;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;


public class Interpretador implements Serializable, IInterpretador{
    private final Input in;
    private final InterpretadorLogin intL;
    private final InterpretadorUtilizador intU;
    private final InterpretadorVoluntario intE;
    private final InterpretadorTransportadora intT;
    private final InterpretadorLoja intLj;

    /**
     *  construtor da classe Interpretador
     */
    public Interpretador() {
        in = new Input();
        intL = new InterpretadorLogin();
        intU = new InterpretadorUtilizador();
        intE = new InterpretadorVoluntario();
        intT = new InterpretadorTransportadora();
        intLj = new InterpretadorLoja();
    }

    /**
     * Interpretador das consultas
     *
     * @param c GestTransAqui
     * @param a Apresentação
     */
    private void interpretadorConsultas(GestTrazAqui c, Apresentacao a) {
        boolean r=true;
        int command;

        while(r) {
            a.printMenuConsultas();
            command = (int) in.lerDouble(a,"Escolha a sua opção:", 0, 2);

            switch(command) {
                case 1:
                    a.printTable("Top Utilizadores", c.getTopUsers());
                    break;
                case 2:
                    a.printTable("Top Transportadoras", c.getTopTrans());
                    break;
                case 0:
                    r = false;
                    break;
                default:
                    a.printErroComandoInvalido();
                    break;
            }
        }
    }

    /**
     * Menu das notificações
     *
     * @param c      GestTrazAqui
     * @param a      Apresentação
     * @param l      Login(nome,pass)
     * @param type   Tipo conta(User,Voluntario,...)
     */
    private void notificacoes(GestTrazAqui c, Apresentacao a, Login l, String type) {
        List<Notificacao> notificacoes;

        if(type.equals("Utilizador"))
            notificacoes = c.getUserNotificacoes(l.getCode());
        else if(type.equals("Loja"))
            notificacoes = c.getLojaNotificacoes(l.getCode());
        else
            notificacoes = c.getEstafetaNotificacoes(l.getCode());

        Collections.reverse(notificacoes);

        int size = notificacoes.size();

        if(size == 0)
            a.printEmptyNot();
        else {
            boolean r=true;
            int page = 0, command = 0;

            while(r) {
                a.notifTable(notificacoes.get(page).getNot(), notificacoes.get(page).getType(), page, size);
                command = (int) in.lerDouble(a,"Escolha a sua opção:", 0, 3);

                if(command == 1 && page<(size-1))
                    page ++;

                else if(command==2 && page>0)
                    page --;

                else if(command==3 && notificacoes.get(page).getType() == 2) {
                    double pontuacao = in.lerDouble(a,"Introduza a classificação (0/10)", 0, 10);
                    c.classificarEstafeta(pontuacao, notificacoes.get(page).getEstCode());
                    notificacoes.get(page).setType(1);
                }

                else if(command==0)
                    r=false;

                else
                    a.printErroComandoInvalido();

            }
        }

        if(type.equals("Utilizador"))
            c.limpaUserNotificacoes(l.getCode());
        else if(type.equals("Loja"))
            c.limpaLojaNotificacoes(l.getCode());
        else
            c.limpaEstafetaNotificacoes(l.getCode());
    }

    /**
     * Controlador geral do programa
     *
     * @param c                        GestTrazAqui
     * @param a                        Apresentação
     * @param l                        Login(nome,pass)
     * @throws ClassNotFoundException  Erro
     * @throws IOException             Erro
     */
    public void interpretador(GestTrazAqui c, Apresentacao a, Login l) throws ClassNotFoundException, IOException {
        int command, numN=0;
        boolean r=true;
        String type;

        Scanner s = new Scanner(System.in);
        GuardarCarregarEstado g = new GuardarCarregarEstado();

        a.welcome();
        s.nextLine();

        while(r) {
            if(l==null) {
                a.printMainMenuLogIn();
                command = (int) in.lerDouble(a,"Escolha a sua opção:", 0, 3);

                switch (command) {
                    case 1:
                        l = intL.interpretador(c, a);
                        break;
                    case 2:
                        g.guardaDados("GestTrazAqui.dat", c);
                        a.printFicheiroGuardado("GestTrazAqui.dat");
                        break;
                    case 3:
                        c = g.carregaDados("GestTrazAqui.dat");
                        a.printFicheiroCarregado("GestTrazAqui.dat");
                        break;
                    case 0:
                        r = false;
                        break;
                    default:
                        a.printErroComandoInvalido();
                        break;
                }
            }

            else {
                type = l.getTipoConta();

                if(l.getTipoConta().equals("Utilizador"))
                    numN = c.getUserNumNotificacoes(l.getCode());
                else if(l.getTipoConta().equals("Loja"))
                    numN = c.getLojaNumNotificacoes(l.getCode());
                else
                    numN = c.getEstafetaNumNotificacoes(l.getCode());

                a.printMainMenuLogOut(l.getTipoConta(),numN);
                command = (int) in.lerDouble(a, "Escolha a sua opção:", 0, 4);
                switch (command) {
                    case 1:
                        l = null;
                        a.printLogoutSucesso();
                        break;
                    case 2:
                        switch (l.getTipoConta()) {
                            case "Utilizador":
                                intU.interpretador(c, a, l);
                                break;
                            case "Voluntario":
                                intE.interpretador(c, a, l);
                                break;
                            case "Transportadora":
                                intT.interpretador(c, a, l);
                                break;
                            case "Loja":
                                intLj.interpretador(c, a, l);
                                break;
                        }
                        break;
                    case 3:
                        interpretadorConsultas(c, a);
                        break;
                    case 4:
                        notificacoes(c, a, l, type);
                        break;
                    case 0:
                        r = false;
                        break;
                    default:
                        a.printErroComandoInvalido();
                        break;
                }
            }
        }

        a.printSair();

        s.close();
    }
}