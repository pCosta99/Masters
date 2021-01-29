package view;


import exceptions.*;
import model.Coordenadas;

import java.time.LocalDate;
import java.time.LocalDateTime;

import static java.lang.System.out;


/*
Classe Menu vai ser a view do MVC
 */

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.*;

public class Menu {

    private MenuIndicador menu;
    private final Stack<MenuIndicador> anterior;
    private final List<MenuIndicador> opcoes ;
    private boolean run;




    public enum MenuIndicador{
        Inicio,
        Admin,
        Registo,
        RegistoCliente,
        RegistoVoluntario,
        Login,
        //cliente
        MenuCliente,
        Encomenda,
        Carrinho,
        FinalizaComVoluntario,
        FinalizaComEmpresa,
        RetiraProdutos,
        //voluntario
        MenuVoluntario,
        AceitarPedido,
        PendingVoluntario,
        PendingEmpresa,
        RevisaoEncomenda,
        //Empresas
        EmpresasTransportadoras, // admin
        ConsultaRegistodeEncomendasEmpresas,
        ShowEmpresasTransporte,
        FacturacaoEmpresa,
        //Lojas
        Lojas,//admin
        //queries
        Top10Clientes, //admin
        Top10Empresas,//admin
        //registos
        ConsultaPedidosCliente,//clientes
        ConsultaRegistodeEncomendasVoluntario,
        ConsultaRegistodeEncomendasVoluntarioAdmin,//admin Voluntario
        Sair

    }


    public Menu() {
        this.menu = MenuIndicador.Inicio;
        this.anterior = new Stack<>();
        this.opcoes = new ArrayList<>();
        this.run = true;
        this.pickSubMenus();
    }

    public MenuIndicador getMenu(){
        return this.menu;
    }

    public boolean getRun(){return this.run;}


    //--------------------------------------Login e Registo de Users -----------------------------------------\\

    /*
    Novo login - um model da view que nao muda
    */

    public NovoLogin novoLogin(String titulo){
        Scanner scan = new Scanner(System.in);
        this.displayHeaderMenu(titulo);
        out.println("Nome de Utilizador:");
        String idUser = scan.nextLine();
        out.println("Password:");
        String password = scan.nextLine();

        return new NovoLogin(idUser,password);


    }


    /*
    Registo de User
     */

    public RegistoUser registoUser(String titulo) throws ExcecaoCoordenadasInvalidas, ExcecaoRegisto {
        this.displayHeaderMenu(titulo);

        Scanner scan = new Scanner(System.in);
        out.println("Id de Utilizador:");
        String idUser = scan.nextLine();
        out.println("Nome");
        String nome = scan.nextLine();
        out.println("Email:");
        String email = scan.nextLine();
        out.println("Password:");
        String password = scan.nextLine();
        out.println("Codigo Postal:");
        String codPostal = scan.nextLine();
        out.println("Morada:");
        String morada = scan.nextLine();


        if (this.menu.equals(MenuIndicador.RegistoCliente)) {

            out.println("Coordenadas de Morada");
            out.println("Longitude:");
            double longi = scan.nextDouble();
            out.println("Latitude:");
            double lati = scan.nextDouble();
            Coordenadas coorMorada = new Coordenadas(longi, lati);


            try {
                return new RegistoUser(idUser, nome, email, password, codPostal, morada, coorMorada);

            } catch (InputMismatchException e) { //ver o que é INputMismatchException
                throw new ExcecaoRegisto("Registo Inválido");

            }


        }else {
            out.println("Coordenadas de Posicao de Partida");
            out.println("Longitude:");
            double longi = scan.nextDouble();
            out.println("Latitude:");
            double lati = scan.nextDouble();
            Coordenadas coor = new Coordenadas(longi, lati);
            out.println("Raio de Trabalho para entrega de Encomendas");
            int raio = scan.nextInt();
            out.println("Estatuto Especial de Encomendas Médicas");
            boolean sp = scan.nextBoolean();
            out.println("Desocupado?");
            boolean st = scan.nextBoolean();

            return new RegistoUser(idUser,nome,email,password,codPostal,morada,coor,raio,st,sp);
        }
    }

    public void showClientes(String titulo,List<List<String>> valores){
        this.displayHeaderMenu(titulo);
        List<String> coluna = new ArrayList<>();
        coluna.add("Nome de CLiente");
        coluna.add("Id de Cliente");
        coluna.add("Email");
        coluna.add("Morada");
        coluna.add("Coordenadas");

        this.tableDefault(valores,coluna);

    }

    public void melhoresClientes(List<List<String>> valores){
        List<String> coluna = new ArrayList<>(0);
        coluna.add("User de Cliente");
        coluna.add("Numero de Encomendas");
        this.displayHeaderMenu("");
        this.tableDefault(valores,coluna);
        Scanner scan = new Scanner(System.in);
        scan.nextLine();
    }


    /*
    PENDING DE VOLUNTÁRIO É ONDE ELE ACEITA OU NAO A ENCOMENDA
     */

    public String pendingEscolhaPrint(String header,  List<List<String>> iT){
        Scanner scan = new Scanner(System.in);
        this.displayHeaderMenu(header);
        List<String> coluna = new ArrayList<>();
        coluna.add("Id de Encomenda");
        coluna.add("Peso");
        coluna.add("Destino");
        coluna.add("Data de Pedido");
        coluna.add("Estatuto de Encomenda");
        this.tableDefault(iT,coluna);
        out.println("\t A -> Aceitar transporte\n\t R -> Recusar transporte");

        return scan.nextLine().toLowerCase();

    }








    /*
    getPeriodoString Função que nos vai dar o periodo que temos de escolher para ver uma facturacao
     */
    public PeriodoTempo getPeriododeString(String per) throws ExcecaoPeriododeTempo {
        Scanner scan = new Scanner(System.in);
        try{
            DateTimeFormatter formatador = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            out.println("Inicio do Periodo requerido (yyyy-MM-dd HH:mm):");
            LocalDateTime inicio = LocalDateTime.parse(scan.nextLine(),formatador);

            out.println("Final do Periodo requerido (yyyy-MM-dd HH:mm):");
            LocalDateTime fim = LocalDateTime.parse(scan.nextLine(),formatador);

            return new PeriodoTempo(inicio,fim);


        }
        catch (DateTimeParseException e){
            throw new ExcecaoPeriododeTempo("Periodo não Válido");
        }
    }


    //------------------------------------------CONSULTAR REGISTOS-----------------------------------------------\\


    public void consultaRegistodeEncomendasEmpresas(PeriodoTempo p, List<List<String>> valores){
        this.displayHeaderMenu("");
        Scanner scan = new Scanner(System.in);
        List<String> colunas = new ArrayList<>();
        colunas.add("Id de Encomenda");
        colunas.add("Id de Cliente");
        colunas.add("Peso");
        colunas.add("Destino");
        colunas.add("Data de Pedido");
        colunas.add("Estatuto de Encomenda");
        colunas.add("Preço");
        colunas.add("Taxa");
        //colunas.add("Produtos");
        colunas.add("custo");

        DateTimeFormatter formato = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        out.println(p.getInicio().format(formato) + "->" + p.getFin().format(formato));
        tableDefault(valores,colunas);

        scan.nextLine();


    }


    public void consultaRegistodeEncomendasCliente(PeriodoTempo p, List<List<String>> valores){
        this.displayHeaderMenu("");
        Scanner scan = new Scanner(System.in);
        List<String> colunas = new ArrayList<>();
        //colunas.add("Periodo Proposto");
        colunas.add("Id de Encomenda");
        colunas.add("Cliente");
        colunas.add("Peso");
        colunas.add("Destino");
        colunas.add("Data do Pedido");
        colunas.add("Estatuto de Encomenda");
        colunas.add("Preço");
        //colunas.add("Produtos"); ainda nao sei com por os produtos


        DateTimeFormatter formato = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        out.println(p.getInicio().format(formato) + "->" + p.getFin().format(formato));
        tableDefault(valores,colunas);

        scan.nextLine();


    }

    /*
    Função utilizada para consultar registo de Encomendas num voluntario
     */

    public void consultarRegistodeEncomendasVoluntario(List<List<String>> valores){
        this.displayHeaderMenu("");
        List<String> colunas = new ArrayList<>();
        colunas.add("Id de Encomenda");
        colunas.add("ID Cliente");
        colunas.add("Peso");
        colunas.add("Destino");
        colunas.add("Data do Pedido");
        colunas.add("Estatuto de Encomenda");
        colunas.add("Preço");
        //colunas.add("Produtos");
        colunas.add("custo");

        tableDefault(valores,colunas);



    }

    /*
    Funções utilizada para consultar qualquer registo de encomendas de Voluntario no menu de Administrador
     */

    public void showVoluntarios(String titulo, List<List<String>> valores){
        this.displayHeaderMenu(titulo);
        List<String> coluna= new ArrayList<>();
        coluna.add("Nome de Utilizador:");
        coluna.add("Id de User");
        coluna.add("Email:");
        coluna.add("Morada");
        coluna.add("Posicao Exata no Momento");
        coluna.add("Raio de Trabalho");
        coluna.add("Status(Especial ou Não)");
        coluna.add("Tempo Medio po km");
        coluna.add("Velocidade");

        this.tableDefault(valores,coluna);


    }


    public String escolheVoluntario(String titulo){
        this.displayHeaderMenu(titulo);
        Scanner scan = new Scanner(System.in);
        out.println("Escolha o id de Voluntario que pretende consultar o registo");
        String id = scan.nextLine();
        return id;
    }

//-------------------------------------- ENCOMENDAS --------------------------------------------------------------\\
    /*
    Realiza Encomenda
     */

    public String perguntaPagar(String titulo){
        Scanner scan = new Scanner(System.in);
        this.displayHeaderMenu(titulo);
        out.println("Pretende que a Encomenda seja prioritária? (Custo aplicados ao preço da Encomenda)");
        out.println("sim-> Aceita Pagamento de taxa adicional   | nao-> Encomenda através de Voluntario");
        return scan.nextLine();
    }

    /*
    Função que pergunta o se quer uma loja com estatuto especial ou nao
     */
    public boolean perguntaEstatutoLoja(){
        Scanner scan = new Scanner(System.in);
        //this.displayHeaderMenu(titulo);
        out.println("Pretende que a loja tenha produto médicos?");
        out.println("true -> sim | false -> nao");
        return scan.nextBoolean();
    }

    public EscolheLoja escolheLojaConfirma(String titulo){
        Scanner scan = new Scanner(System.in);
        this.displayHeaderMenu(titulo);
        out.println("ID de Loja que pretende fazer a Encomenda");
        String loja = scan.nextLine();
        out.println("Escolheu:" + loja);
        out.println("sim-> Continuar   | nao-> Cancelar");
        String resposta = scan.nextLine();


        return new EscolheLoja(loja,resposta);



    }


    /*
    Funçoes para o ciclo de adicionar produtos
     */

    public String adicionaProdutos(String titulo){
        Scanner scan = new Scanner(System.in);
        this.displayHeaderMenu(titulo);
        out.println("Continuar?");
        return scan.nextLine();


    }
    public EscolheProduto adicionaProduto(){

        Scanner scan = new Scanner(System.in);
        out.println("Escreve o ID do Produto");
        String produto = scan.nextLine();
        out.println("Quantidade");
        int quantidade = scan.nextInt();

        return new EscolheProduto(produto,quantidade);

    }

    public boolean aceitaProdutos(List<List<String>> valores){
        Scanner scan = new Scanner(System.in);
        List<String> coluna = new ArrayList<>();
        coluna.add("Nome");
        coluna.add("Id do Produto");
        coluna.add("Id de Loja");
        coluna.add("Preço");
        coluna.add("Peso");
        coluna.add("Quantidade");

        tableDefault(valores,coluna);
        out.println("Aceita os Produtos Escolhidos?");
        out.println("true -> sim | false -> nao");
        boolean escolha = scan.nextBoolean();

        return escolha;


    }


    /*
    Funcao que lista os produtos a adicionar na encomenda
     */


    public String realizaEncomenda(String titulo, List<List<String>> valores){
            Scanner scan = new Scanner(System.in);
            this.displayHeaderMenu(titulo);
            List<String> coluna= new ArrayList<>();
            coluna.add("Id de Encomenda");
            coluna.add("Data de Encomenda");
            coluna.add("Custo");
            coluna.add("Peso");
            coluna.add("Taxa aplicada");
            coluna.add("Estatuto Especial");
            coluna.add("Produtos:");


            this.tableDefault(valores,coluna);
            out.println("\nAceita pagar");
            out.println("\n S -> SIM \n\t N -> NAO");

            return scan.nextLine().toUpperCase();


    }


    /*
    Voluntario
     */
    public String respostaEncomendaAoPendingPrint(String header, List<List<String>> valores){
        Scanner scan = new Scanner(System.in);
        this.displayHeaderMenu(header); //carrinho
        List<String> coluna = new ArrayList<>();
        coluna.add("Id de Encomenda");
        coluna.add("Cliente");
        coluna.add("Peso");
        coluna.add("Destino");
        coluna.add("Data do Pedido");
        coluna.add("Estatuto de Encomenda");
        coluna.add("Preço");
        coluna.add("custo");

        this.tableDefault(valores,coluna);

        out.println("Finalizar Encomenda:\n");
        out.println("\t sim -> Aceitar transporte\n\t nao -> Recusar transporte");
        String resposta = scan.nextLine();

        return resposta;
    }




    public void showEncomenda(String titulo, List<List<String>> valores) throws ExcecaoRegistoDeEncomendasNull {
        this.displayHeaderMenu(titulo);
        List<String> coluna = new ArrayList<>();
        coluna.add("Id de Encomenda");
        coluna.add("Id de Cliente");
        coluna.add("Data do Pedido");
        coluna.add("Peso");
        coluna.add("Estatuto de Encomenda ESpecial?");
        coluna.add("Destino");
        coluna.add("Taxa");
        coluna.add("Custo");
        //coluna.add("Produtos");

        this.tableDefault(valores,coluna);
    }





//----------------------------------------------CENAS MENU--------------------------------------------------------\\
    /*
    Display do Header do Menu a sua criação
     */
    public void displayHeaderMenu(String head){
        out.println("\033\143");
        out.println(this.criaHeader());
        out.println(new StringP(head).under().toString());

    }


    /*
    Cria o titulo de menu
     */

    public String criaHeader(){
       StringP header = new StringP("\t--");
        for(MenuIndicador h: this.anterior)
            header.append(h.name()).append("/");

        return header.append(this.menu.name()).append("--\n").toString();
    }


    /*
    Operações no menu
     */

    public Menu retrocesso() {
        if (this.anterior.size() > 0) {
            this.menu = this.anterior.pop(); //vai buscar o primeiro elemento da stack de menus anteriores
            this.pickSubMenus();
        } else this.run = false;

        if(this.menu.equals(MenuIndicador.Login) || this.menu.equals(MenuIndicador.Admin) || this.menu.equals(MenuIndicador.Registo) || this.menu.equals(MenuIndicador.Sair))
            this.retrocesso();

        return this;
    }

    /*
    Direciona para o menu desejado
     */
    public Menu selecionaOpcao(MenuIndicador m) {
        this.anterior.push(this.menu);
        this.menu = m;
        this.pickSubMenus();
        return this;
    }

    //---------------------------------------------------------- LOJAS -------------------------------------------------------------------------\\

    /*
        Print de Lojas
     */

    public void showLojas(String titulo, List<List<String>> valores){
        this.displayHeaderMenu(titulo);
        List<String> coluna = new ArrayList<>();
        coluna.add("Nome da Loja");
        coluna.add("Codigo de Loja");
        coluna.add("Coordenadas");
        coluna.add("Estatuto Especial");
        coluna.add("Facturacao");
        coluna.add("Em espera");
        coluna.add("Tempo médio de Atendimento");


        this.tableDefault(valores,coluna);


    }

    public EscolheLoja escolheLoja(String titulo) throws ExcecaoLojaInexistente {
        Scanner scan = new Scanner(System.in);
        this.displayHeaderMenu(titulo);
        out.println("Escreva o ID da loja que pretende ver");
        String loja = scan.nextLine();
        out.println("sim->continuar   | nao->sair");
        String ok = scan.nextLine();

        try {
            return new EscolheLoja(loja, ok);
        } catch (Exception e) {
            throw new ExcecaoLojaInexistente("Loja inexistente");
        }

    }


    /*
    Print dos Produtos
     */

    public void showProdutos(String titulo,List<List<String>> valores){
        this.displayHeaderMenu(titulo);
        List<String> coluna = new ArrayList<>();

        coluna.add("Nome");
        coluna.add("Id do Produto");
        coluna.add("Id de Loja");
        coluna.add("Preço");
        coluna.add("Peso");
        coluna.add("Quantidade");


        this.tableDefault(valores,coluna);

    }





    //------------------------------------------------------------ EMPRESAS DE TRANSPORTE ------------------------------------\\
    /*
    Empresas de Transporte
     */

    public void showEmpresasTransporte(String titulo, List<List<String>> valores){
        this.displayHeaderMenu(titulo);
        List<String> coluna = new ArrayList<>();
        coluna.add("Nome de Empresa");
        coluna.add("ID de Empresa");
        coluna.add("Facturacao Total");
        coluna.add("Taxa adicional de cada Encomenda");
        coluna.add("Capacidade Total de Encomendas");
        coluna.add("Localizaçao");
        coluna.add("Classificaçao");

        this.tableDefault(valores,coluna);
    }

    public String escolheEmpresaTransporte(String titulo){
        this.displayHeaderMenu(titulo);
        Scanner scan = new Scanner(System.in);
        out.println("Escreva o id da Empresa");
        String empresa = scan.nextLine();

        return empresa;

    }

    public void melhoresEmpresas(List<String> valores){
        List<String> coluna = new ArrayList<>();
        this.displayHeaderMenu("");
        System.out.println(valores);
        Scanner scan = new Scanner(System.in);
        scan.nextLine();
    }


    /*
    Show de String
     */

    public void showS(String string){
        Scanner scanner = new Scanner(System.in);
        out.print("\033\143");
        out.println(this.criaHeader());
        //out.println();
        out.println(string);
        scanner.nextLine();

    }


    /*
    tableDefault - print da tabela
     */


    public void tableDefault(List<List<String>> valores, List<String> coluna){
        int i;
        List<String> linhas = new ArrayList<>();
        for(i=0; i<valores.size(); i++)
            linhas.add(String.format("%d",i+1)); //ver o que isto faz

        Tabela<String> tabela = new Tabela<>(linhas,coluna,valores);
        out.println(tabela);

    }



    public  String opcoesMenus(int o) {
        switch (this.opcoes.get(o)) {

            case Inicio:
                return "TrazAqui ";

            case Admin:
                return "Administrador";

            case Registo:
                return "Novo User";

            case RegistoCliente:
                return "Registar novo Cliente";

            case RegistoVoluntario:
                return "Registar novo Voluntario";

            case Login:
                return "Login";

            case Lojas:
                return "Lojas Disponiveis";

            case EmpresasTransportadoras:
                return "Empresas Transportadoras";


            case MenuCliente:
                return "Menu de Cliente";

            case Carrinho:
                return  "Fazer Encomenda";

            case FinalizaComVoluntario:
                return "Finalizar Encomenda através de Voluntario mais perto";

            case FinalizaComEmpresa:
                return "Finalizar Encomenda através de Empresa Pretendida";

            case RetiraProdutos:
                return "Retirar Produtos";


            case MenuVoluntario:
                return "Menu de Voluntario";

            case Encomenda:
                return "Realizar Encomenda";

            case AceitarPedido:
                return "Pedido Aceite!";

            case PendingVoluntario:
                return "Por aceitar - Voluntario";

            case RevisaoEncomenda:
                return "Detalhes de Encomenda";

            case ConsultaRegistodeEncomendasVoluntarioAdmin:
                return "Registos Voluntario";
            case ConsultaRegistodeEncomendasEmpresas:
                return "Registo de Empresa";


            case Top10Clientes:
                return "Top10 de Clientes a usar TrazAqui";

            case Top10Empresas:
                return "Top10 de Empresas mais requisitadas";

            case ConsultaPedidosCliente:
                return "Historico de Pedidos de Encomendas";

            case ConsultaRegistodeEncomendasVoluntario:
                return "Historico de Encomendas Realizadas";

            case ShowEmpresasTransporte:
                return "Catálogo de Empresas associadas";

            case FacturacaoEmpresa:
                return "Facturacao de Empresa por Periodo";
            case Sair:
                return "Saiu";

            default:
                return "";
        }
    }


        private void pickSubMenus(){
            this.opcoes.clear();
            switch (this.menu){
                case Inicio:

                    this.opcoes.add(MenuIndicador.Login);//case FEITO
                    this.opcoes.add(MenuIndicador.Registo);//case FEITO
                    this.opcoes.add(MenuIndicador.Admin);
                    //this.opcoes.add(MenuIndicador.Sair);
                    break;


                case Admin:
                    this.opcoes.add(MenuIndicador.Lojas);//case feito
                    this.opcoes.add(MenuIndicador.EmpresasTransportadoras);//SUB MENU FEITO
                    this.opcoes.add(MenuIndicador.Top10Clientes); //case FEITO VER PARSER
                    this.opcoes.add(MenuIndicador.Top10Empresas); //case FEITO VER PARSER
                    this.opcoes.add(MenuIndicador.ConsultaRegistodeEncomendasVoluntarioAdmin);//CASE FEITO VER PARSER
                    this.opcoes.add(MenuIndicador.ConsultaPedidosCliente);

                    break;

                case MenuCliente:
                    this.opcoes.add(MenuIndicador.ConsultaPedidosCliente);//case feito
                    this.opcoes.add(MenuIndicador.Encomenda);

                    break;




                case Encomenda:
                    this.opcoes.add(MenuIndicador.Lojas);//case feito
                    this.opcoes.add(MenuIndicador.Carrinho);

                    break;


                case Carrinho:
                    this.opcoes.add(MenuIndicador.FinalizaComVoluntario);
                    this.opcoes.add(MenuIndicador.FinalizaComEmpresa);

                    break;

                case Registo:
                    this.opcoes.add(MenuIndicador.RegistoCliente);//case feito
                    this.opcoes.add(MenuIndicador.RegistoVoluntario);//case feito
                    break;


                case MenuVoluntario:
                    this.opcoes.add(MenuIndicador.ConsultaRegistodeEncomendasVoluntario);//case feito
                    this.opcoes.add(MenuIndicador.PendingVoluntario); //case por fazer
                    break;

                case EmpresasTransportadoras:
                    this.opcoes.add(MenuIndicador.ShowEmpresasTransporte);//case feito VER PARSER
                    this.opcoes.add(MenuIndicador.ConsultaRegistodeEncomendasEmpresas);//case feito VER PARSER
                    this.opcoes.add(MenuIndicador.FacturacaoEmpresa);
                    break;


            }
        }


        /*
        Função para andar entre menus
         */
    public Menu deFault (){
        out.println(this);
        String s = new Scanner(System.in).nextLine();
        if (s.matches("^[+-]?\\d{1,8}$")){
            int i = Integer.parseInt(s);
            if (this.opcoes.size() > i - 1 && i > 0) {
                this.anterior.push(this.menu);
                this.menu = this.opcoes.get(i - 1);
                this.pickSubMenus();
            }
        }
        switch (s) {
            case "b":
            case "..":
                this.retrocesso();
                break;
            case "e":
                this.run = false;
                break;
        }

        return this;
    }

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("\033\143");
        s.append(this.criaHeader()).append("\n\n");

        for (int i = 0; i < this.opcoes.size(); i++)
            s.append(i + 1).append("- ").append(this.opcoesMenus(i)).append("\n");
        return s.toString();
    }



}




