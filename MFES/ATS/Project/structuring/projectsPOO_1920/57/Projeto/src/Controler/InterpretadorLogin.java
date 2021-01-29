/**
 * Classe que controla o menu de login
 */
package Controler;

import Model.*;
import View.Apresentacao;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.TreeSet;

public class InterpretadorLogin implements Serializable {
    private final Input in;


    public InterpretadorLogin() {
        in = new Input();
    }

    /**
     *  Método que cria um login
     *
     * @param c      GestTrazAqui
     * @param a      Apresentação
     * @return       Login com nome e pass
     */
    private Login login(GestTrazAqui c, Apresentacao a) {
        Scanner s = new Scanner(System.in);
        String user, pass;

        a.printPedirUsername();
        user = s.nextLine();

        if(c.containsUser(user)) {
            a.printPedirPassword();
            pass = s.nextLine();

            if(c.containsPassword(user, pass)) {
                return c.getLogin(user);
            }
        }

        return null;
    }

    /**
     *  Método que cria um utilizador
     *
     * @param a     Apresentação
     * @param code  UserCode
     * @param nome  Nome
     * @return      novo Utilizador
     */
    private Utilizador registarUtilizador(Apresentacao a, String code, String nome){
        Scanner s = new Scanner(System.in);
        double price;

        Coordenadas cr = in.lerCoordenada(a);
        price = in.lerDouble(a,"Introduza o Preco Máximo: ",0,1000000);

        return new Utilizador(code, nome, cr, new TreeSet<>(),new TreeSet<>(),new ArrayList<>());
    }

    /**
     *  Método que cria um estafeta
     *
     * @param a     Apresentação
     * @param code  Estafeta Code
     * @param nome  Nome
     * @param type  tipo de estafeta
     * @return      Novo estafeta
     */
    private Estafeta registarEstafeta(Apresentacao a, String code, String nome, String type) {
        Scanner s = new Scanner((System.in));
        double raio, velocidade;
        String medic;
        boolean isMedic;

        Coordenadas cr = in.lerCoordenada(a);
        raio = in.lerDouble(a,"Introduza o seu raio da ação: ",0,100000);
        velocidade = in.lerDouble(a,"Introduza a sua velocidade média: ",0,100000);

        a.printPedirEncomendasMedicas();
        medic = s.nextLine();

        isMedic = medic.equals("S");

        return new Estafeta(code, nome, cr, raio, velocidade, 0, true, isMedic, 0, 0, type,false,new ArrayList<>());
    }

    /**
     *  Método que cria uma transportadora
     *
     * @param a Apresentação
     * @param e Estafeta
     * @return  Nova Transportadora
     */
    private Transportadora registarTransportadora(Apresentacao a, Estafeta e) {
        Scanner s = new Scanner(System.in);
        int nif;
        double taxaKm, taxaPeso;

        nif = (int) in.lerDouble(a,"Introduza o seu NIF: ",0,1000000);
        taxaKm = in.lerDouble(a,"Introduza a sua taxa por Km: ",0,1000000);
        taxaPeso = in.lerDouble(a,"Introduza a sua taxa por Kg: ",0,1000000);

        return new Transportadora(e.getCode(), e.getName(), e.getGps(), e.getRaio(), e.getVelocidade(), e.getNumKm(), e.isFree(), e.isMedic(), e.getClassificacao(),e.getNumCla(), nif, taxaKm, taxaPeso, 0,false, new ArrayList<>());
    }

    /**
     *  Método que cria uma loja
     *
     * @param a     Apresentação
     * @param c     Controlador
     * @param code  Código da Loja
     * @param nome  Nome da Loja
     * @return      Loja
     */
    private Loja registarLoja(Apresentacao a, GestTrazAqui c, String code, String nome) {
        Scanner s = new Scanner(System.in);
        String queue;
        boolean hasQueue;

        Coordenadas cr = in.lerCoordenada(a);

        if (in.lerSN(a, "A loja tem informação de fila de espera? (S/N)")) {
            double queueTime = in.lerDouble(a,"Qual é o tempo médio de espera em fila?: ",0,1000);
            int queueSize = (int) in.lerDouble(a, "Quantas pessoas estão na fila?", 0, 20);
            return new Loja(code, nome, cr, true, queueTime, queueSize, c.randomListaProdutos() , new ArrayList<>());
        }

        return new Loja(code, nome, cr, false, -1, 0, c.randomListaProdutos(), new ArrayList<>());
    }

    /**
     * Regista uma conta
     *
     * @param c GestTrazAqui
     * @param a Apresentação
     * @return  true se foi criada com sucesso
     */
    private boolean registar(GestTrazAqui c, Apresentacao a) {
        Scanner s = new Scanner(System.in);
        Login l = new Login();
        String code, nome, pass, tipo;

        a.printPedirNomeCompleto();
        nome = s.nextLine();

        do {
            a.printPedirTipoConta();
            tipo = s.nextLine();
        } while(!(tipo.equals("Voluntario") || tipo.equals("Transportadora") || tipo.equals("Utilizador") || tipo.equals("Loja")));

        if(!c.containsNameAndType(nome, tipo)) {
            l.setNome(nome);
            l.setTipoConta(tipo);

            a.printPedirPassword();
            pass = s.nextLine();

            l.setPassword(pass);

            do { code = c.generateCode(tipo); } while(c.containsUser(code));

            l.setCode(code);

            switch(tipo) {
                case "Voluntario":
                    c.addEstafeta(registarEstafeta(a, code, nome, "Voluntario"));
                    break;
                case "Transportadora":
                    Estafeta e = registarEstafeta(a, code, nome, "Transportadora");
                    c.addEstafeta(registarTransportadora(a, e));
                    break;
                case "Utilizador":
                    c.addUser(registarUtilizador(a, code, nome));
                    break;
                case "Loja":
                    c.addLoja(registarLoja(a, c, code, nome));
                    break;
            }

            a.printCodigoAcesso(code);

            c.addLogin(l);

            return true;
        }

        return false;
    }

    /**
     * Interpretador menu login
     *
     * @param c GestTrazAqui
     * @param a Apresentação
     * @return  Login criado
     */
    public Login interpretador(GestTrazAqui c, Apresentacao a) {
        boolean r=true;
        int command;
        Login l = null;

        while(r) {
            a.printMenuLogin();
            command = (int) in.lerDouble(a,"Escolha a sua opção:", 0, 2);

            switch(command){
                case 1:
                    if((l = login(c, a))!= null) {
                        a.printLoginSucesso();
                        r = false;
                    }
                    else {
                        a.printErroDadosInvalidos();
                    }
                    break;

                case 2:
                    if(registar(c, a))
                        a.printRegistoSucesso();
                    else
                        a.printErroDadosInvalidos();
                    break;

                case 0:
                    l = null;
                    r = false;
                    break;

                default:
                    a.printErroComandoInvalido();
                    break;
            }
        }
        return l;
    }
}
