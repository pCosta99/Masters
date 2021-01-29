package View;
import java.io.ObjectStreamException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;
import Model.*;
import Exceptions.*;

public class Menu{
    private MenuIndice menu;
    private final Stack<MenuIndice> prev;
    private final ArrayList<MenuIndice> options;
    private boolean run;

    public enum MenuIndice {  //Temos de ir acrescentando variaveis consoante o que queremos!!!!
        MenuInicial,
        Login,
        LoginLoja,
        LoginUtilizador,
        LoginEmpresa,
        LoginVoluntario,
        Registar,
        RegistarCliente,
        RegistarVoluntario,
        RegistarTransportadora,
        RegistarLoja,
        MenuCliente,
        MenuLoja,
        MenuEmpresa,
        MenuVoluntario,
        FazerEncomenda,
        MenuPedido,
        Pedido,
        EscolhaProduto,
        Rating,
        Encomendas,
        HistoricoEncomendas,
        HistoricoEncomendasIntervalo,
        HistoricoEncomendasLoja,
        HistoricoEncomendasLojaIntervalo,
        AddProduto,
        HistoricoEncomendasEmpresa,
        HistoricoEncomendasEmpresaIntervalo,
        HistoricoEncomendasVoluntario,
        HistoricoEncomendasVoluntarioIntervalo,
        Faturacao,
        Best10Utilizadores,
        Best10Empresas,
        MudaEstadoVoluntario,
        MudaEstadoEmpresa,
        ClassificacaoVoluntario,
        ClassificaoEmpresa,
        MudaCategoriaEmpresa,
        MudaCategoriaVoluntario
    }

    public Menu() {
        this.menu = MenuIndice.MenuInicial;
        this.prev = new Stack<>();
        this.options = new ArrayList<>();
        this.run = true;
        this.pickChildMenus();
    }

    public MenuIndice getMenu() {
        return this.menu;
    }



    public void showString(String mostra) {
        Scanner scanner = new Scanner(System.in);
        System.out.print("\033\143");
        System.out.println(this.createHeader());
        System.out.println();
        System.out.println(mostra);
        scanner.nextLine();
    }

    public void showString2() {
        Scanner scanner = new Scanner(System.in);
        scanner.nextLine();
    }


    


    public NewLogin newLogin(String error) { 
        Scanner scanner = new Scanner(System.in);
        this.displayMenuHeader(error);
        System.out.println("Utilizador:");
        String user = scanner.nextLine();
        System.out.println("Password:");
        String password = scanner.nextLine();
        return new NewLogin(user, password);
    }



    public User registarCliente(String error) throws EmailInvalidoException {
        displayMenuHeader(error);
        Scanner scannerC = new Scanner(System.in);
        System.out.println("Nome de Utilizador:");
        String nome = scannerC.nextLine();
        System.out.println("Email:");
        String email = scannerC.nextLine();
        if(email.contains("@")){
            System.out.println("Password:");
            String pass = scannerC.nextLine();
            System.out.println("Latitude:");
            double latitude = scannerC.nextDouble();
            System.out.println("Longitude:");
            double longitude = scannerC.nextDouble();
            
            return new User( "u", nome, email, pass,latitude, longitude);
        }
        else throw new EmailInvalidoException();
    }


    public Voluntarios registarVoluntario(String error) throws OpcaoInvalidaException{
        displayMenuHeader(error);
        Scanner scannerV = new Scanner(System.in);
        System.out.println("Nome do Voluntario:");
        String nome = scannerV.nextLine();
        System.out.println("Password:");
        String pass = scannerV.nextLine();
        System.out.println("Latitude:");
        double latitude = scannerV.nextDouble();
        System.out.println("Longitude:");
        double longitude = scannerV.nextDouble();
        System.out.println("Raio de acao:");
        double raio = scannerV.nextDouble();
        System.out.println("Tempo medio por Km:");
        double tempoMedporKm = scannerV.nextDouble();
        System.out.println("Faz transporte de encomendas medicas (1-Sim,2-Nao):");
        int medica = scannerV.nextInt();
        boolean medicaB = false;
        switch(medica){
            case 1:
                medicaB = true;
                break;
            case 2:
                medicaB = false;
                break;
            default:
                throw new OpcaoInvalidaException();
        }

        return new Voluntarios(pass,"v",nome, latitude, longitude, raio, medicaB, tempoMedporKm);
    }

    public EmpresasTrans registarTransportadora(String error) throws OpcaoInvalidaException {
        displayMenuHeader(error);
        Scanner scannerE = new Scanner(System.in);
        System.out.println("Nome da Empresa:");
        String nome = scannerE.nextLine();
        System.out.println("Password:");
        String pass = scannerE.nextLine();
        System.out.println("Nif da Empresa:");
        String nif = scannerE.nextLine();
        System.out.println("Latitude:");
        double latitude = scannerE.nextDouble();
        System.out.println("Longitude:");
        double longitude = scannerE.nextDouble();
        System.out.println("Raio de acao:");
        double raio = scannerE.nextDouble();
        System.out.println("Preco por km percorrido:");
        double taxa = scannerE.nextDouble();
        System.out.println("Faz transporte de encomendas medicas (1-Sim,2-Nao):");
        int medica = scannerE.nextInt();
        boolean medicaB = false;
        switch(medica){
            case 1:
                medicaB = true;
                break;
            case 2:
                medicaB = false;
                break;
            default:
                throw new OpcaoInvalidaException();
        }
        System.out.println("Tempo medio por Km:");
        double tempoMedporKm = scannerE.nextDouble();
        System.out.println("Preco por unidade de peso:");
        double precoPeso = scannerE.nextDouble();
        System.out.println("Preco por unidade de tempo:");
        double precoTempo = scannerE.nextDouble();

        return new EmpresasTrans(pass,"t", nome, latitude, longitude, nif,raio,taxa,medicaB, tempoMedporKm, precoPeso, precoTempo);
    }


    public Lojas registarLoja(String error) throws OpcaoInvalidaException {
        displayMenuHeader(error);
        Scanner scannerL = new Scanner(System.in);
        System.out.println("Nome da Loja:");
        String nome = scannerL.nextLine();
        System.out.println("Password:");
        String pass = scannerL.nextLine();
        System.out.println("Latitude:");
        double latitude = scannerL.nextDouble();
        System.out.println("Longitude:");
        double longitude = scannerL.nextDouble();
        System.out.println("Tem fila de espera?  1-Sim, 2-Nao");
        int filaEspera = scannerL.nextInt();
        boolean filaEsperaB = false;
        Lojas ltem = new Lojas();
        Lojas lnaotem = new Lojas();
        switch(filaEspera){
            case 1:
                System.out.println("Tempo de atendiemnto:");
                double tempoAtendimento = scannerL.nextDouble();
                filaEsperaB = true;
                ltem = new Lojas(pass,"l", nome, latitude, longitude, tempoAtendimento, filaEsperaB);
                return ltem;
            case 2:
                filaEsperaB = false;
                lnaotem = new Lojas(pass,"l", nome, latitude, longitude, 0, filaEsperaB);
                return lnaotem;
            default:
                throw new OpcaoInvalidaException();
        }          
    } 

    public LinhaEncomenda registarProduto(String error) {
        displayMenuHeader(error);
        Scanner scannerLin = new Scanner(System.in);
        System.out.println("Codigo do Produto:");
        String cod = scannerLin.nextLine();
        System.out.println("Descricao:");
        String descricao = scannerLin.nextLine();
        System.out.println("Preco:");
        double preco = scannerLin.nextDouble();
        System.out.println("Peso por Unidade:");
        double pesoUnidade = scannerLin.nextDouble();
        return new LinhaEncomenda(cod,descricao,preco,0.0,pesoUnidade);
    }

    public Encomendas registaEncomenda(String error, String utilizador, String loja) throws OpcaoInvalidaException{
        displayMenuHeader(error);
        Scanner scannere = new Scanner(System.in);
        System.out.println("É uma encomenda médica? (1-Sim,2-Nao):");
        int medica = scannere.nextInt();
        boolean medicaB = false;
        switch(medica){
            case 1:
                medicaB = true;
                break;
            case 2:
                medicaB = false;
                break;
            default:
                throw new OpcaoInvalidaException();
            
        }
        List<LinhaEncomenda> l = new ArrayList<>();
    
        return new Encomendas("e", utilizador, loja, 0.0, medicaB, (ArrayList<LinhaEncomenda>) l);
    }


    public String getLoja(String error) {
        displayMenuHeader(error);
        Scanner scannerL = new Scanner(System.in);
        System.out.println("Insira o codigo da Loja:");
        String loja = scannerL.nextLine();
        return loja;
    }

    public String getTransportadora(String error) {
        displayMenuHeader(error);
        Scanner scannerL = new Scanner(System.in);
        System.out.println("Insira o codigo da Empresa:");
        String empresa = scannerL.nextLine();
        return empresa;
    }

    public String getVoluntario(String error) {
        displayMenuHeader(error);
        Scanner scannerL = new Scanner(System.in);
        System.out.println("Insira o codigo do voluntario:");
        String voluntario = scannerL.nextLine();
        return voluntario;
    }

    public void showFaturacao( String emp ,double fat, String error){
        displayMenuHeader(error);
        System.out.println(" O total faturado pela empresa" + emp + " e: " + fat);

    }

    public void showHistoricoEnc(List<Encomendas> enc, String error){
        displayMenuHeader(error);
        for(Encomendas e : enc){
            System.out.println(e);
        }
    }

    public String escolheLoja(){
        Scanner scannerLoj = new Scanner(System.in);
        System.out.println("Insira o codigo da loja onde pretende fazer a encomenda:");
        String codLoja = scannerLoj.nextLine();
        return codLoja;
    }

    public String escolheProduto(){
        Scanner scannerP = new Scanner(System.in);
        System.out.println("Insira o codigo do produto que pretende inserir na encomenda:");
        String codProduto = scannerP.nextLine();
        return codProduto;
    }

    public int getQuantidade(){
        Scanner scannerQ = new Scanner(System.in);
        System.out.println("Insira a quantidade de produto que quer comprar:");
        int quantidade = scannerQ.nextInt();
        return quantidade;
    }

    public int fazRate(String cod, String error) throws ClassificacaoInvalidaException{
        displayMenuHeader(error);
        Scanner scannerT = new Scanner(System.in);
        System.out.println("Insira uma classificaçao de 1 a 5 para " + cod);
        int quantidade = scannerT.nextInt();
        if(quantidade<1 || quantidade>5 ){ throw new ClassificacaoInvalidaException();}
        else{ return quantidade;}
    }

    public void showProdutos(List<LinhaEncomenda> l, String error){
        displayMenuHeader(error);
        for(LinhaEncomenda le  : l){
            System.out.println(le.getCodProdu() + " - " + le.getDescricao() + " - " + le.getPreco() + "\n");
        }
    }

    public void showLojas(List<Lojas> l, String error){
        displayMenuHeader(error);
        for(Lojas le  : l){
            System.out.println(le.getCodLoja() + " - " + le.getNome() + "\n");
        }
    }

    public void showClassificacao(String error, double classifcacao){
        displayMenuHeader(error);
        System.out.println("A sua classificacao e:" + classifcacao);
        Scanner sc = new Scanner(System.in);
        sc.nextLine();
    }




    public Menu selectOption(MenuIndice i) {
        this.prev.push(this.menu);
        this.menu = i;
        this.pickChildMenus();
        return this;
    }

    public boolean getRun() { return this.run; }


    public Menu back() {
        if (this.prev.size() > 0) {
            this.menu = this.prev.pop();
            this.pickChildMenus();
        } else {
            this.run = false; 
        }
        if (this.menu.equals(MenuIndice.Login) || this.menu.equals(MenuIndice.Registar))
            this.back();
        return this;
    }

    

    private void displayMenuHeader(String error) {
        System.out.print("\033\143");
        System.out.println("\nInstruções:");
        System.out.println("e -> Para fechar aplicação e guaradar\nb -> Para voltar atrás nos menus\n");
        System.out.println(this.createHeader());
        System.out.println(new StringBetter(error).under().toString());
    }

    private String createHeader() {
        StringBetter strHeader = new StringBetter("\t--");
        for (MenuIndice val : this.prev)
            strHeader.append(val.name()).append("/");


        return strHeader.append(this.menu.name()).append("--\n").toString();
    }

    public IntervaloDeTempo getTimeInterval(String error) throws TempoIntervaloInvalidoException{
        Scanner scanner = new Scanner(System.in);
        this.displayMenuHeader(error);
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

            System.out.println("Inicio de Intervalo (yyyy-MM-dd HH:mm):");
            LocalDateTime inicio = LocalDateTime.parse(scanner.nextLine(), formatter);

            System.out.println("Fim de Intervalo (yyyy-MM-dd HH:mm):");
            LocalDateTime fim = LocalDateTime.parse(scanner.nextLine(), formatter);

            return new IntervaloDeTempo(inicio, fim);
        }
        catch (DateTimeParseException e){
           throw new TempoIntervaloInvalidoException();
        }
    }

    

    private String menuOptionText(int i) {
        switch (this.options.get(i)) {
            case MenuInicial:
                return "Menu Inicial";
            case Registar:
                return  "Registar novo utilizador";
            case RegistarCliente:
                return "Registar novo Cliente";
            case RegistarVoluntario:
                return  "Registar novo Voluntario";
            case RegistarTransportadora:
                return "Registar nova Empresa";
            case RegistarLoja:
                return "Registar nova Loja";
            case Login:
                return  "Login";
            case LoginUtilizador:
                return  "Sou um cliente!";
            case LoginLoja:
                return  "Sou uma Loja!";
            case LoginEmpresa:
                return "Sou uma Empresa!";
            case LoginVoluntario:
                return "Sou um Voluntario!";
            case MenuCliente:
                return "Menu Cliente";
            case MenuLoja:
                return "Menu Loja";
            case MenuEmpresa:
                return "Menu Empresa";
            case MenuVoluntario:
                return "Menu Voluntario";
            case FazerEncomenda:
                return "Fazer uma encomenda";
            case MenuPedido:
                return "Fazer pedido de encomenda";
            case Pedido:
                return "Fazer pedido de encomenda";
            case EscolhaProduto:
                return "Adicionar produto ao carrinho";
            case Encomendas:
                return "Listas de Encomendas";
            case Rating:
                return "Classificar entidades de transporte";
            case HistoricoEncomendas:
                return "Meu Historico de Encomendas";
            case HistoricoEncomendasIntervalo:
                return "Meu Historico de Encomendas num Intervalo";
            case HistoricoEncomendasLoja:
                return "Historico de Encomendas de uma loja";
            case HistoricoEncomendasLojaIntervalo:
                return "Historico de Encomendas de uma loja num Intervalo";
            case AddProduto:
                return "Adicionar um produto à loja";
            case HistoricoEncomendasEmpresa:
                return  "Historico de Encomendas de uma Empresa";
            case HistoricoEncomendasEmpresaIntervalo:
                return  "Historico de Encomendas de uma Empresa num Intervalo";
            case ClassificaoEmpresa:
                return "Minha classificacao";
            case MudaEstadoEmpresa:
                return "Mudar o meu estado";
            case MudaCategoriaEmpresa:
                return "Mudar a minha Categoria (Medica/Nao Medica)";
            case HistoricoEncomendasVoluntario:
                return "Historico de Encomendas de uma Voluntario";
            case HistoricoEncomendasVoluntarioIntervalo:
                return  "Historico de Encoemndas de um Voluntario num Intervalo";
            case ClassificacaoVoluntario:
                return "Minha Classificacao";
            case MudaEstadoVoluntario:
                return "Mudar o meu estado";
            case MudaCategoriaVoluntario:
                return "Mudar a minha Categoria (Medica/Nao Medica)";
            case Faturacao:
                return "Faturacao Total de uma Empresa";
            case Best10Utilizadores:
                return "10 Melhores Utilizadores";
            case Best10Empresas:
                return "10 Melhores Empresas";
                default:
                    return "";
        }
    }

    private void pickChildMenus() {
        this.options.clear();
        switch (this.menu) {
            case MenuInicial:
                this.options.add(MenuIndice.Login);
                this.options.add(MenuIndice.Registar);
                break;
            case Login:
                this.options.add(MenuIndice.LoginUtilizador);
                this.options.add(MenuIndice.LoginLoja);
                this.options.add(MenuIndice.LoginEmpresa);
                this.options.add(MenuIndice.LoginVoluntario);
                break;
            case Registar:
                this.options.add(MenuIndice.RegistarCliente);
                this.options.add(MenuIndice.RegistarVoluntario);
                this.options.add(MenuIndice.RegistarTransportadora);
                this.options.add(MenuIndice.RegistarLoja);
                break; 
            case MenuCliente:
                this.options.add(MenuIndice.FazerEncomenda);
                this.options.add(MenuIndice.Encomendas);
                this.options.add(MenuIndice.Best10Utilizadores);
                this.options.add(MenuIndice.Best10Empresas);
                this.options.add(MenuIndice.Rating);
                break;
            case MenuLoja:
                this.options.add(MenuIndice.HistoricoEncomendasLoja);
                this.options.add(MenuIndice.HistoricoEncomendasLojaIntervalo);
                this.options.add(MenuIndice.AddProduto);
                break;
            case MenuEmpresa:
                this.options.add(MenuIndice.HistoricoEncomendasEmpresa);
                this.options.add(MenuIndice.HistoricoEncomendasEmpresaIntervalo);
                this.options.add(MenuIndice.Faturacao);
                this.options.add(MenuIndice.ClassificaoEmpresa);
                this.options.add(MenuIndice.MudaEstadoEmpresa);
                this.options.add(MenuIndice.MudaCategoriaEmpresa);   
                break;         
            case MenuVoluntario:
                this.options.add(MenuIndice.HistoricoEncomendasVoluntario);
                this.options.add(MenuIndice.HistoricoEncomendasVoluntarioIntervalo);
                this.options.add(MenuIndice.ClassificacaoVoluntario);
                this.options.add(MenuIndice.MudaEstadoVoluntario);
                this.options.add(MenuIndice.MudaCategoriaVoluntario);    
                break;
            case MenuPedido:
                this.options.add(MenuIndice.Pedido);
                this.options.add(MenuIndice.EscolhaProduto);
                break;
            case Encomendas:
                this.options.add(MenuIndice.HistoricoEncomendas);
                this.options.add(MenuIndice.HistoricoEncomendasIntervalo);
                break;

            default:
                break;
        }
    }



    public Menu parser() {
        System.out.println(this);
        String str = new Scanner(System.in).nextLine();
        if (str.matches("^[+-]?\\d{1,8}$")) {
            int i = Integer.parseInt(str);
            if (this.options.size() > i - 1 && i > 0) {
                this.prev.push(this.menu);
                this.menu = this.options.get(i - 1);
                this.pickChildMenus();
            }
        }
        switch (str) {
            case "b":
                this.back();
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
        s.append(this.createHeader()).append("\n\n");

        for (int i = 0; i < this.options.size(); i++)
            s.append(i + 1).append("- ").append(this.menuOptionText(i)).append("\n");
        return s.toString();
    }
}