package controller;

import interfaces.*;

import model.*;
import view.InterfaceGeral;

import java.io.IOException;
import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe que executa o programa
 */
public class Teste {

    private static ISistema s;
    private static ILogin login;

    /**
     * Construtor da classe
     * @param s sistema
     * @param login login
     */
    public Teste(ISistema s, ILogin login) {
        Teste.s = s;
        Teste.login = login.clone();
    }

    /**
     * Executa o programa
     */
    public void executer() {

        String username;
        String password;
        boolean valido, sair = false;


        while(!sair) {

            InterfaceGeral.menu();
            InterfaceGeral.message(">> ");
            int opcaoLogin = Input.lerInt();


            switch(opcaoLogin) {

                case 1:
                    InterfaceGeral.message("Insira os dados do seu login\n");
                    InterfaceGeral.message("Username: ");
                    username = Input.lerString();
                    InterfaceGeral.message("Password: ");
                    password = Input.lerString();
                    valido = login.loginUtilizador(username,password);
                    if(!valido) {
                        InterfaceGeral.message("Os dados inseridos são inválidos!\n");
                        break;
                    }

                    ControllerUser cu = new ControllerUser(s,username);

                    cu.menu();

                    break;

                case 2:
                    InterfaceGeral.message("Insira os dados do seu login\n");
                    InterfaceGeral.message("Username: ");
                    username = Input.lerString();
                    InterfaceGeral.message("Password: ");
                    password = Input.lerString();
                    valido = login.loginVoluntario(username,password);
                    if(!valido) {
                        InterfaceGeral.message("Os dados inseridos são inválidos!\n");
                        break;
                    }
                    ControllerVoluntarios cv = new ControllerVoluntarios(s,username);
                    cv.menu();
                    break;

                case 3:
                    InterfaceGeral.message("Insira os dados do seu login\n");
                    InterfaceGeral.message("Username: ");
                    username = Input.lerString();
                    InterfaceGeral.message("Password: ");
                    password = Input.lerString();
                    valido = login.loginEmpresa(username,password);
                    if(!valido) {
                        InterfaceGeral.message("Os dados inseridos são inválidos!\n");
                        break;
                    }
                    ControllerEmpresas ce = new ControllerEmpresas(s,username);

                    ce.menu();

                    break;

                case 4:
                    InterfaceGeral.message("Insira os dados do seu login\n");
                    InterfaceGeral.message("Username: ");
                    username = Input.lerString();
                    InterfaceGeral.message("Password: ");
                    password = Input.lerString();
                    valido = login.loginLoja(username,password);
                    if(!valido) {
                        InterfaceGeral.message("Os dados inseridos são inválidos!\n");
                        break;
                    }
                    ControllerLoja cl = new ControllerLoja(s,username);
                    cl.menu();
                    break;

                case 5:
                    s.addUser(registarUtilizador());
                    break;

                case 6:
                    s.addVoluntario(registarVoluntario());
                    break;

                case 7:
                    s.addEmpresa(registarEmpresa());
                    break;

                case 8:
                    s.addLoja(registarLoja());
                    break;

                case 9:
                    InterfaceGeral.showVoluntarios(s.getVoluntarios());
                    break;

                case 10:
                    InterfaceGeral.showEmpresas(s.getTransportadoras());
                    break;

                case 11:
                    InterfaceGeral.showLojas(s.getLojas());
                    break;

                case 12:
                    InterfaceGeral.showUsers(s.getUtilizadores());
                    break;

                case 13:
                    InterfaceGeral.showEmpresas(s.getTransportadoras());
                    InterfaceGeral.message("Insira a empresa transportadora desejada: ");
                    String emp = Input.lerString();
                    InterfaceGeral.date(true);
                    InterfaceGeral.message("Mês: ");
                    int inicioM = Input.lerInt();
                    InterfaceGeral.message("Dia: ");
                    int inicioD = Input.lerInt();
                    InterfaceGeral.date(false);
                    InterfaceGeral.message("Mês: ");
                    int fimM = Input.lerInt();
                    InterfaceGeral.message("Dia: ");
                    int fimD = Input.lerInt();

                    try {
                        LocalDateTime inicio = LocalDateTime.of(2020,inicioM,inicioD,0,0);
                        LocalDateTime fim = LocalDateTime.of(2020,fimM,fimD,0,0);

                        InterfaceGeral.printFat(s.getTransportadoras().get(emp).faturacao(s,inicio,fim));

                    } catch(DateTimeException e) {
                        e.getMessage();
                    }
                    break;

                case 14:
                    InterfaceGeral.top10Users(s.top10Users());
                    break;

                case 15:
                    InterfaceGeral.top10Empresas(s.top10Empresas());
                    break;

                case 16:
                    try {
                        s = s.lerDados();
                        login = login.lerDados();
                        InterfaceGeral.message("Os dados foram lidos.\n");
                    } catch (IOException | ClassNotFoundException e) {
                        e.getMessage();
                    }
                    break;

                case 17:
                    try {
                        s.gravarDados();
                        login.gravarDados();
                        InterfaceGeral.message("Os dados foram gravados.\n");
                    } catch (IOException e) {
                        e.getMessage();
                    }
                    break;

                case 0:
                    InterfaceGeral.message("Obrigado e volte sempre!\n");
                    sair = true;
            }
        }

    }

    /**
     * Regista um novo utilizador
     * @return novo utilizador
     */
    private static IUser registarUtilizador() {

        boolean valido = false;
        String username = "";

        while(!valido) {
            InterfaceGeral.message("Insira os novos dados do login\n");

            InterfaceGeral.message("Username (ex.: u00): ");
            username = Input.lerString();

            InterfaceGeral.message("Password: ");
            String password = Input.lerString();

            valido = login.addUtilizador(username, password);

            if (!valido) InterfaceGeral.message("O username não está disponível!\n");

        }

        InterfaceGeral.message("Insira o seu nome completo: ");
        String nome = Input.lerString();

        InterfaceGeral.message("Insira a latitude: ");
        double lat = Input.lerDouble();
        InterfaceGeral.message("Insira a longitude: ");
        double lon = Input.lerDouble();

        InterfaceGeral.message("Registado com sucesso!\n");

        return new User(nome,lat,lon,new HashMap<>(),username);
    }

    /**
     * Regista um novo voluntário
     * @return novo voluntário
     */
    private static IVoluntario registarVoluntario() {
        boolean valido = false;
        String username = "";

        while(!valido) {
            InterfaceGeral.message("Insira os novos dados do login");

            InterfaceGeral.message("Username (ex.: v00) ");
            username = Input.lerString();

            InterfaceGeral.message("Password: ");
            String password = Input.lerString();

            valido = login.addVoluntario(username,password);

            if(!valido) InterfaceGeral.message("O username não está disponível!\n");
        }

        InterfaceGeral.message("Insira o seu nome completo: ");
        String nome = Input.lerString();

        InterfaceGeral.message("Insira a latitude: ");
        double lat = Input.lerDouble();
        InterfaceGeral.message("Insira a longitude: ");
        double lon = Input.lerDouble();

        InterfaceGeral.message("Está apto para o transporte de medicamentos? (true ou false) ");
        boolean medi = Input.lerBoolean();

        InterfaceGeral.message("Tem disponibilidade imediata? (true ou false) ");
        boolean livre = Input.lerBoolean();

        InterfaceGeral.message("Qual o seu raio de deslocação? ");
        double raio = Input.lerDouble();

        InterfaceGeral.message("Qual a sua velocidade de deslocação? ");
        double vel = Input.lerDouble();

        InterfaceGeral.message("Registado com sucesso!\n");

        return new Voluntario(username,nome,lat,lon,raio,vel,medi,null,livre,new HashMap<>());
    }

    /**
     * Regista uma nova empresa
     * @return nova empresa
     */
    private static IEmpresa registarEmpresa() {
        boolean valido = false;
        String username = "";

        while(!valido) {
            InterfaceGeral.message("Insira os novos dados do login");

            InterfaceGeral.message("Username (ex.: t00): ");
            username = Input.lerString();

            InterfaceGeral.message("Password: ");
            String password = Input.lerString();

            valido = login.addEmpresa(username,password);

            if(!valido) InterfaceGeral.message("O username não está disponível!\n");
        }

        InterfaceGeral.message("Insira o seu nome completo: ");
        String nome = Input.lerString();

        InterfaceGeral.message("Insira o NIF da empresa: ");
        String nif = Input.lerString();

        InterfaceGeral.message("Insira a latitude: ");
        double lat = Input.lerDouble();
        InterfaceGeral.message("Insira a longitude: ");
        double lon = Input.lerDouble();

        InterfaceGeral.message("Está apto para o transporte de medicamentos? (true ou false) ");
        boolean medi = Input.lerBoolean();

        InterfaceGeral.message("Tem disponibilidade imediata? (true ou false) ");
        boolean livre = Input.lerBoolean();

        InterfaceGeral.message("Qual o seu raio de deslocação? ");
        double raio = Input.lerDouble();

        InterfaceGeral.message("Qual a sua velocidade de deslocação? ");
        double vel = Input.lerDouble();

        InterfaceGeral.message("Taxa de transporte cobrada por km: ");
        double taxa = Input.lerDouble();

        InterfaceGeral.message("Aceita o transporte de mais de uma encomenda de cada vez? (true ou false) ");
        boolean umaEnc = Input.lerBoolean();

        InterfaceGeral.message("Registado com sucesso!\n");

        return new Empresa(username,nome,nif,taxa,lat,lon,raio,vel,medi,null,livre,umaEnc,0,new HashMap<>());
    }

    /**
     * Regista uma nova loja
     * @return novo loja
     */
    private static ILoja registarLoja() {

        boolean valido = false;
        String username = "";

        while(!valido) {
            InterfaceGeral.message("Insira os novos dados do login\n");

            InterfaceGeral.message("Username (ex.: l00) ");
            username = Input.lerString();

            InterfaceGeral.message("Password: ");
            String password = Input.lerString();

            valido = login.addLoja(username, password);

            if (!valido) InterfaceGeral.message("O username não está disponível!\n");
        }

        InterfaceGeral.message("Insira o seu nome completo: ");
        String nome = Input.lerString();

        InterfaceGeral.message("Insira a latitude: ");
        double lat = Input.lerDouble();
        InterfaceGeral.message("Insira a longitude: ");
        double lon = Input.lerDouble();

        InterfaceGeral.message("Tem o sistema de fila de espera na sua loja? (true ou false) ");
        boolean fila = Input.lerBoolean();

        boolean prods = true;
        Map<String,IProduto> map = new HashMap<>();
        while(prods) {
            InterfaceGeral.message("Insira o código do produto (ex: p00): ");
            String codProduto = Input.lerString();

            InterfaceGeral.message("Insira o nome do produto: ");
            String prod = Input.lerString();

            InterfaceGeral.message("Insira o valor: ");
            double valor = Input.lerDouble();

            InterfaceGeral.message("Insira o peso: ");
            double peso = Input.lerDouble();

            boolean med = false;
            if(nome.equals("Detergente") || nome.equals("Alcool") || nome.equals("Desinfetante"))
                med = true;

            InterfaceGeral.message("Deseja adicionar mais produtos? (S ou N): ");
            String soun = Input.lerString();
            IProduto p = new Produto(codProduto, prod, 1, valor,peso,med);
            map.put(codProduto,p.clone());
            if (soun.equals("N")) {
                prods = false;
            }
        }

        InterfaceGeral.message("Registado com sucesso!\n");

        return new Loja(username,nome,fila,0,lat,lon,map);
    }

}
