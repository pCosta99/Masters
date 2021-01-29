import java.io.Console;
import java.io.Serializable;
import java.lang.System;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;



public class View implements Serializable{

    private static Integer contador = 1; /**contador usado para quando um novo Usuario
                                         da aplicacao se registar, receber este numero no seu
                                         codigo e usuario e incrementamos-lo nessa altura*/
    private Scanner in;

    /**
     * Cores usadas em diversos prints para colocar os menus mais bonitos
     */
    private static final String RESET = "\u001B[0m";
    private static final String BLACK = "\u001B[30m";
    private static final String RED = "\u001B[31m";
    private static final String GREEN = "\u001B[32m";
    private static final String YELLOW = "\u001B[33m";
    private static final String BLUE = "\u001B[34m";
    private static final String PURPLE = "\u001B[35m";
    private static final String CYAN = "\u001B[36m";
    private static final String WHITE = "\u001B[37m";
    
    public View (){
        this.in= new Scanner(System.in);
    }

    public View (Integer i){
        this.in= new Scanner(System.in);
        View.contador = i;
    }


    public void inicio(User.Meteorologia met,User.EstadoCovid estadoCovid){
        System.out.println( BLUE + "\n                          SEJA BEM-VINDO A APLICACAO TRAZ-AQUI: " + RESET );
        System.out.println( RED + "\nMeteorologia de hoje : " + RESET +  met +  RED + "\nEstado pandemia de hoje : " + RESET + estadoCovid);
    }
    public int menu(){
        System.out.println("\nEscolha a opcao pretendida:");
        System.out.println(CYAN + "\n 1 " + YELLOW + "-->" + RESET + " Registar-me");
        System.out.println(CYAN + " 2 " + YELLOW + "-->" + RESET + " Login");
        System.out.println(CYAN + " 3 " + YELLOW + "-->" + RESET + " Carregar de Ficheiro (texto)");
        System.out.println(CYAN + " 4 " + YELLOW + "-->" + RESET + " Imprimir para Ficheiro (texto)");
        System.out.println(CYAN + " 5 " + YELLOW + "-->" + RESET + " Carregar de Ficheiro (binario)");
        System.out.println(CYAN + " 6 " + YELLOW + "-->" + RESET + " Imprimir para Ficheiro (binario)");
        System.out.println(CYAN + " 7 " + YELLOW + "-->" + RESET + " Quit\n");
        return in.nextInt();
    }

    public String registoInicialnome()
    {
        System.out.println("Diga o seu nome"); 
        return this.in.next();
    }

    /**
     * metodo para ler uma password e dar return a ela mesma,
     * a password é pedida duas vezes e tem de coincidir
    */
    public String registoInicialpassword(){
        String password1 = "";
        String password2 = "";
        boolean coincide = false;
        while (!coincide) {
            Console console = System.console();
            char[] passwordArray = console.readPassword("Palavra-passe pretendida:\n");

            password1 = "";
            
            for (char i : passwordArray){
                password1 = password1 + i;
            }

            passwordArray = console.readPassword("Digite a sua palavra-passe novamente:\n");

            password2 = "";

            for (char i : passwordArray){
                password2 = password2 + i;
            }

            if (password1.equals(password2))
                coincide = true;
            
            else System.out.println("Passwords nao coincidem!\n");
        }
        return password1;

    }

    public int registoInicialOpcoes () {
        System.out.println("Registo efetuado com sucesso!\n\n");
        System.out.println("Que tipo de usuario e:?\n");
        System.out.println("1 --> Utilizador");
        System.out.println("2 --> Voluntario");
        System.out.println("3 --> Loja");
        System.out.println("4 --> Transportadoras\n");
        return in.nextInt();
    }

    /**
     * Criamos um novo Utilizador aqui, recebemos a password já antes inserida 
     * por parametro e damos return a esse Utilizador registado, 
     * mas sendo ele to tipo abstracto User para podermos adicionar ao map das contas
     * e quando precisar-mos de algum metodo especifico é que vamos dar "cast" para este tipo
     */
    public User registoUtiliazador ( String password ){

        String codUtilizador = 'u' + contador.toString();
        contador ++;
                                   
        System.out.println ( "\nNome completo: ");
        in.nextLine();
        String nomeUtilizador = in.nextLine();

        System.out.println ( "\nSua latitude: ");
        double latitudeUtilizador =  in.nextDouble();

        System.out.println ( "\nSua longitude: ");
        double longitudeUtilizador =  in.nextDouble();

        GPS gpsUtilizador = new GPS ( latitudeUtilizador , longitudeUtilizador );
        User adicionadoUtilizador = new Utilizador (codUtilizador, password, nomeUtilizador , gpsUtilizador);
        

        System.out.println ( "\nUtilizador adicionado : " + adicionadoUtilizador.toString());
        System.out.println ( "\nSeu login sera : " + adicionadoUtilizador.getId() + "\n\n");
        return adicionadoUtilizador.clone();

    }

    /**
     * Criamos um novo Voluntario aqui, recebemos a password já antes inserida 
     * por parametro e damos return a esse Voluntario registado, 
     * mas sendo ele to tipo abstracto User para podermos adicionar ao map das contas
     * e quando precisar-mos de algum metodo especifico é que vamos dar "cast" para este tipo
     */
    public User registoVoluntario ( String password ){

        String codVoluntario = 'v' + contador.toString();
        contador ++;

        System.out.println ( " Nome completo: ");
        in.nextLine();
        String nomeVoluntario = in.nextLine();

        System.out.println ( "\nSua latitude: ");
        double latitudeVoluntario =  in.nextDouble();

        System.out.println ( "\nSua longitude: ");
        double longitudeVoluntario =  in.nextDouble();

        GPS gpsVoluntario = new GPS ( latitudeVoluntario , longitudeVoluntario );

        System.out.println("\nSeu raio de acao");
        double raioVoluntario = in.nextDouble();

        System.out.println("\nTem certificado de tranporte de encomendas medicas? true / false");
        boolean tranportoMedicamentosVoluntario = in.nextBoolean();

        User adicionadoVoluntario = new Voluntarios(codVoluntario, nomeVoluntario, password,  gpsVoluntario, raioVoluntario, tranportoMedicamentosVoluntario );

        System.out.println ( "\nVoluntario adicionado : " + adicionadoVoluntario.toString());
        System.out.println ( "\nSeu login sera : " + adicionadoVoluntario.getId());
        
        return adicionadoVoluntario.clone();
    }


    /**
     * Criamos uma nova Loja ou Loja com fila aqui, recebemos a password já antes inserida 
     * por parametro e damos return a essa Loja registada, 
     * mas sendo ela to tipo abstracto User para podermos adicionar ao map das contas
     * e quando precisar-mos de algum metodo especifico é que vamos dar "cast" para este tipo
     */
    public User registoLoja ( String password ) {
        String codLoja = 'l' + contador.toString();
        contador ++;
                                

        System.out.println ( "Nome de estabelecimento: ");
        in.nextLine();
        String nomeLoja = in.nextLine();

        System.out.println ( "\nSua latitude: ");
        double latitudeLoja =  in.nextDouble();

        System.out.println ( "\nSua longitude: ");
        double longitudeLoja =  in.nextDouble();

        GPS gpsLoja = new GPS ( latitudeLoja, longitudeLoja );

        System.out.println("\nSua loja e com fila? (true/false)");
        boolean b = in.nextBoolean();
        User adicionadoLoja;
        
        if (b){
            adicionadoLoja = new LojaComFila (codLoja, nomeLoja, password, gpsLoja);
        }
        else {
            adicionadoLoja = new Lojas (codLoja, nomeLoja, password, gpsLoja);
        }

        System.out.println ( "\nLoja adicionado : " + adicionadoLoja.toString());
        System.out.println ( "\nSeu login sera : " + adicionadoLoja.getId());
        return adicionadoLoja.clone();

    }

    /**
     * Criamos uma nova Transportadora aqui, recebemos a password já antes inserida 
     * por parametro e damos return a essa Transportadora registada, 
     * mas sendo ela to tipo abstracto User para podermos adicionar ao map das contas
     * e quando precisar-mos de algum metodo especifico é que vamos dar "cast" para este tipo
     */
    public User registoTransportadora ( String password){

        String codEmpresa = 't' + contador.toString();
        contador ++;

        System.out.println ( " Nome empresa: ");
        in.nextLine();
        String nomeEmpresa = in.nextLine();

        System.out.println ( "\nSua latitude: ");
        double latitudeTransportadora =  in.nextDouble();

        System.out.println ( "\nSua longitude: ");
        double longitudeTransportadora  =  in.nextDouble();

        GPS gpsTransportadora  = new GPS ( latitudeTransportadora  , longitudeTransportadora  );

        System.out.println("Seu nif");
        String nifTransportadora = in.next();

        System.out.println("\nSeu raio de acao");
        double raioTransportadora  = in.nextDouble();

        System.out.println("\nSeu preco por km");
        double precoPorKmTransportadora  = in.nextDouble();

        System.out.println("\nVelocidade media do seu transporte");
        int velocidadeMedia = in.nextInt();
        

        System.out.println("\nSua capacidade de encomendas");
        int capacidadeTransportadora = in.nextInt();
        

        System.out.println("\nTem certificado de tranporte de encomendas medicas? true / false");
        boolean tranportoMedicamentosTransportadora = in.nextBoolean();


        User adicionadoTransportadora = new Transportadoras(codEmpresa, nomeEmpresa, password, gpsTransportadora , 
        nifTransportadora, raioTransportadora , precoPorKmTransportadora,velocidadeMedia,capacidadeTransportadora,tranportoMedicamentosTransportadora);


        System.out.println ( "\nTransportadora adicionada : " + adicionadoTransportadora.toString());
        System.out.println ( "\nSeu login sera : " + adicionadoTransportadora.getId());

        return adicionadoTransportadora.clone();
    }

    /**
     * Scan ao nome de ficheiro para ler
     */
    public String nomeFicheiro(){
        
        System.out.print("Introduza o nome do ficheiro\n");
        return in.next();
    }

    /**
     * Um print para quando quisermos imprimir algo pelo controller chamamos view.print("..")
     */
    public void print(String mensagem){
        System.out.println(mensagem);
    }

    /**
     * Print para erros, a mensagem fica a vermelho
     */
    public void printErro(String mensagem){
        System.out.println(RED + mensagem + RESET);
    }
    
    public void close()
    {
        this.in.close();
    }

    public String scan(){
        return this.in.next();
    }

    public int scanInt(){
        return this.in.nextInt();
    }

    /**
     * Funcao para criar uma nova encomenda por parte de um Utilizador
     * Consiste em uma série de perguntas direcionadas ao utilizador, no final
     * damos return a essa Encomenda
     */
    public Encomenda novaEncomenda(String nUtilizador, String nLoja,Integer contadorEnc)
            throws ValorInvalidoException
    {
        int peso = 0;
        boolean done = false;
        List<LinhaEncomenda> linhas= new ArrayList<>();
        while(!done){
            System.out.println("Digite o codigo de produto");
            String codProduto=in.next();
            System.out.println("Digite a descricao do produto");
            String descricao=in.next();
            System.out.println("Indique a quantidade");
            double quantidade=in.nextInt();
            while (quantidade <= 0){
                System.out.println("Quantidade nao pode ser inferior a 1");
                quantidade=in.nextInt();
            }
            System.out.println("Indique o valor unitario");
            double valorUnitario=in.nextInt();
            while (valorUnitario < 0){
                System.out.println("Valor nao pode ser inferior a 0");
                valorUnitario=in.nextInt();
            }
            LinhaEncomenda linha= new   LinhaEncomenda(codProduto,descricao,quantidade,valorUnitario);
            linhas.add(linha);
            peso+=(int) quantidade;

            System.out.println ("Chegou ao fim? (true / false )");
            done = in.nextBoolean();
        }
        
        System.out.println("A encomenda e medica? (true / false )");
        boolean eMedica = in.nextBoolean();
        String codEncomendaNova = 'e' + contadorEnc.toString();
        Encomenda encomendaNova = new Encomenda(codEncomendaNova, nUtilizador , nLoja, peso, LocalDate.now(), linhas , eMedica);
        System.out.print (YELLOW + "\n\nDados da encomenda :" + RESET + encomendaNova.toString() + "\nValor total: " + encomendaNova.valorTotalEncomenda() + "\n\n");
        return encomendaNova.clone();
    }

    /**
     * Perguntar algo ao utilizador e responder com um booleano
     */
    public Boolean pergunta(String mensagem)
    {
        System.out.println(mensagem);
        return this.in.nextBoolean();
    }

    /**
     * Perguntar algo ao utilizador e responder com um int
     */
    public int perguntaInt (String mensagem)
    {
        System.out.println(mensagem);
        return this.in.nextInt();
    }

    /**
     * Perguntar algo ao utilizador e responder com uma String
     */
    public String perguntaString (String mensagem)
    {
        System.out.println(mensagem);
        return this.in.next();
    }

    /**
     *  Par imprimir uma lista de encomendas por entregar de forma mais bonita
     */
    public void apresentaEncomendasParaEntregar (List <Encomenda> l){

        int conta = 1;

        StringBuilder sb = new StringBuilder();

        for ( Encomenda e : l){

            sb.append( RED + "\nEncomenda que necessita entrega nº" + RESET + conta + " :\n").append(e.toString()).append("\n\n");
            conta++;

        }

        System.out.println(sb.toString());

    }

    /**
     * Scan de uma data inicial constituida por dia, mes, ano e irá dar return 
     * a uma LocalDate, metodo usado para fazer pesquisa de algum registo por datas 
     */
    public LocalDate scanDataInicial (){
        System.out.println("Qual a data inicial?\n Escreva da forma dia - mes - ano com um enter de espaço em cada campo\n");
        int diaI = in.nextInt();
        int mesI = in.nextInt();
        int anoI = in.nextInt();
        LocalDate dataI = LocalDate.of(anoI, mesI, diaI);
        return dataI;
    }


    /**
     * Scan de uma data final constituida por dia, mes e ano e irá dar return 
     * a uma LocalDate, metodo usado para fazer pesquisa de algum registo por datas 
     */
    public LocalDate scanDataFinal (){
        System.out.println("Qual a data final?\n Escreva da forma dia - mes - ano com um enter de espaço em cada campo\n");
        int diaF = in.nextInt();
        int mesF = in.nextInt();
        int anoF = in.nextInt();                                 
        LocalDate dataF = LocalDate.of(anoF, mesF, diaF);
        return dataF;
    }


    /**
     * Diversos menus usados pela aplicacao
     */

    public int menuUtilizador () {
        System.out.println("Selecione a opcao desejada:\n");
        System.out.println("1 - Criar encomenda");
        System.out.println("2 - Pedir entrega de uma encomenda"); //
        System.out.println("3 - Aceitar Transporte");
        System.out.println("4 - Classificar Voluntario / Empresa de Transporte"); //
        System.out.println("5 - Consultas avancadas");
        System.out.println("6 - Voltar ao menu inicial\n");
        return in.nextInt();
    }

    public int consultarDadosUtilizador (){
        System.out.println("Selecione a opcao desejada:\n");
        System.out.println("1 - Ver o meu perfil");
        System.out.println("2 - Ver registos de encomendas pedidas");
        System.out.println("3 - Ver registos de encomendas");
        System.out.println("4 - Ver registos de encomendas entre certas datas");
        System.out.println("5 - Ver top 'x'");  //top x voluntarios / users ordenados por diversas cenas
        System.out.println("6 - voltar atraz\n");
        return in.nextInt();
    }

    public int menuLoja (){
        System.out.println("Selecione a opcao desejada:\n");
        System.out.println("1 - Sinalizar que certa encomenda está disponivel para entrega");
        System.out.println("2 - consultas avançadas");
        System.out.println("3 - Voltar ao menu inicial\n");
        return in.nextInt();
    }

    public int consultarDadosLoja (){
        System.out.println("Selecione a opcao desejada:\n");
        System.out.println("1 - Ver o meu perfil");
        System.out.println("2 - Ver total que a minha loja faturou nesta app");
        System.out.println("3 - Ver total de pessoas na fila");
        System.out.println("4 - Ver os meus registos de encomendas preparadas num periodo de tempo");
        System.out.println("5 - Ver registos de encomendas preparadas");
        System.out.println("6 - Ver encomendas em preparacao");
        System.out.println("7 - Ver top 'x'");  //top x voluntarios / users ordenados por diversas cenas
        System.out.println("8 - Voltar atraz\n");
        return in.nextInt();

    }

    public int menuTransportadora (){
        System.out.println("Selecione a opcao desejada:\n");
        System.out.println("1 - Sinalizar disponibilidade de entrega");
        System.out.println("2 - Aceitar entrega de certa encomenda no seu raio");
        System.out.println("3 - Ir buscar certa encomenda a uma loja");
        System.out.println("4 - Transportar encomenda "); //Transportar certa encomenda e registar sua duração
        System.out.println("5 - Consultas avancadas");
        System.out.println("6 - Voltar ao menu inicial\n");
        return in.nextInt();
    }

    public int consultarDadosTransportadora (){
        System.out.println("Selecione a opcao desejada:\n");
        System.out.println("1 - Ver o meu perfil");
        System.out.println("2 - Ver total faturado num periudo de tempo");
        System.out.println("3 - Ver os meus registos de encomendas entregues");
        System.out.println("4 - Ver os meus registos de encomendas entregues num periodo de tempo");
        System.out.println("5 - Ver as minhas classificacoes");
        System.out.println("6 - Ver top 'x'");  //top x voluntarios / users ordenados por diversas cenas
        System.out.println("7 - Voltar Atraz\n");
        return in.nextInt();

    }


    public int menuVoluntario () {
        System.out.println("Selecione a opcao desejada:\n");
        System.out.println("1 - Sinalizar disponibilidade de entrega");
        System.out.println("2 - Ir buscar certa encomenda a uma loja dentro do seu raio");
        System.out.println("3 - Transportar encomenda ");  //Transportar certa encomenda e registar sua duração 
        System.out.println("4 - Consultas avancadas");
        System.out.println("5 - Voltar ao menu inicial\n");
        return in.nextInt();
    }


    public int consultarDadosVoluntario (){
        System.out.println("Selecione a opcao desejada:\n");
        System.out.println("1 - Ver o meu perfil");
        System.out.println("2 - Ver as minhas classificacoes");
        System.out.println("3 - Ver registos de encomendas entregues");
        System.out.println("4 - Ver os meus registos de encomendas entregues num periodo de tempo");
        System.out.println("5 - Ver top 'x'");  //top x voluntarios / users ordenados por diversas cenas
        System.out.println("6 - Voltar Atraz\n");
        return in.nextInt();

    }


}
