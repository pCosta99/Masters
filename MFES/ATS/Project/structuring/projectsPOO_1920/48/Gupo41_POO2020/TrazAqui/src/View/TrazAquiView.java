package View;

import Controller.ITrazAquiController;
import Model.Exceptions.*;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.*;

public class TrazAquiView implements ITrazAquiView{

    private ITrazAquiController controller;

    /**
     * Construtor parametrizado de TrazAquiView
     * @param controller
     */
    public TrazAquiView(ITrazAquiController controller){
        this.controller = controller;
    }

    /**
     * Lê uma opção introduzida pelo utilizar e devolve-a
     * @return
     */
    private int lerOpcao(){
        int op;
        Scanner is = new Scanner(System.in);

        try{
            op = is.nextInt();
        }
        catch(InputMismatchException e){
            op = -1;
        }
        if(op < 0 ){
            System.out.println("Opção inválida!");
            op = -1;
        }
        return op;
    }

    /**
     * Imprime o menu inicial da aplicação
     */
    public void printMenuInicial(){
        System.out.println("----------------------------------------------------------");
        System.out.print("\t\t\t\t\t\t\t\tTRAZ AQUI\t\t\t\t\t\t\t\t\n");
        System.out.println("----------------------------------------------------------");
        System.out.println("0 - Sair");
        System.out.println("1 - Registar");
        System.out.println("2 - Login");
        System.out.println("3 - Ver faturação de uma empresa num determinado intervalo de tempo");
        System.out.println("4 - Ver os 10 utilizadores que mais utilizaram o sistema");
        System.out.println("5 - Ver as 10 empresas que mais kms percorreram");
        System.out.println("6 - Gravar Estado");
        System.out.println("7 - Carregar Estado");
        System.out.println("8 - Ver fatores de aleatoriedade");
        System.out.println("----------------------------------------------------------");
    }


    /**
     * Método que inicia o menu
     */
    public void run(){

        Scanner sc = new Scanner(System.in);

        String ficheiro;
        int scan = -1;
        boolean exit = false;

        while(!exit) {
            printMenuInicial();

            scan = lerOpcao();

            switch (scan) {
                case 0:
                    exit = true;
                    break;
                case 1:
                    menuRegistar();
                    break;
                case 2:
                    menuLogin();
                    break;
                case 3:
                    System.out.println("Indique a empresa de transporte que pretende ver.");
                    String codEmp = sc.next();
                    System.out.println("Qual o dia, mes e ano da data inicial (separados por \" - \" )?");
                    String dataIS = sc.next();
                    System.out.println("Qual o dia, mes e ano da data final (separados por \" - \" )?");
                    String dataFS = sc.next();
                    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");

                    try {
                        LocalDate dataI = LocalDate.parse(dataIS, formatter);
                        LocalDate dataF = LocalDate.parse(dataFS, formatter);
                        System.out.println("Faturação igual a " + this.controller.processaFaturacaoEmp(codEmp, dataI, dataF));
                    }
                    catch (DateTimeParseException e){
                        System.out.println("Data inserida inválida");
                    }
                    catch (EmpresaInexistenteException e){
                        System.out.println("Não existe a empresa " + e.getMessage());
                    }

                    esperaAteContinuar();
                    break;
                case 4 :
                    System.out.println(this.controller.processaUtilizadoresMaisAtivos());

                    esperaAteContinuar();
                    break;
                case 5 :
                    System.out.println(this.controller.processaEmpresaMaisAtivas());

                    esperaAteContinuar();
                    break;
                case 6 :
                    //Gravar estado
                    System.out.println("Qual o ficheiro desejado?");
                    ficheiro = sc.next();
                    boolean sucessoGravar = false;

                    try{
                        sucessoGravar = this.controller.processaGuardaEstado(ficheiro);
                        if(sucessoGravar) System.out.println("Estado gravado com sucesso");
                    }
                    catch(FileNotFoundException e){
                        System.out.println("Não foi possível abrir o ficheiro!");
                    }
                    catch(IOException e){
                        System.out.println("Não foi possível gravar o estado da aplicação!");
                    }
                    break;
                case 7:
                    //Carregar estado
                    System.out.println("Qual o ficheiro desejado?");
                    ficheiro = sc.next();
                    boolean sucessoCarregar = false;

                    try{
                        sucessoCarregar = this.controller.processaCarregarEstado(ficheiro);
                    }
                    catch(FileNotFoundException e){
                        System.out.println("Não foi possível abrir o ficheiro!");
                    }
                    catch(IOException e){
                        System.out.println("Não foi possível ler em modo binário!");
                    }
                    catch(ClassNotFoundException e){
                        System.out.println("Não foi possivel encontrar a classe correta no ficheiro!");
                    }
                    break;
                case 8:
                    String metereologia = "";
                    switch (this.controller.processaGetMetereologia()){
                        case 1:
                            metereologia = "Sol";
                            break;
                        case 2:
                            metereologia = "Nublado";
                            break;
                        case 3:
                            metereologia = "Chuva";
                            break;
                        case 4:
                            metereologia = "Neve";
                            break;
                    }
                    String transito = "";
                    switch (this.controller.processaGetTransito()){
                        case 1:
                            transito = "Leve";
                            break;
                        case 2:
                            transito = "Moderado";
                            break;
                        case 3:
                            transito = "Elevado";
                            break;
                        case 4:
                            transito = "Acidente";
                            break;
                    }

                    System.out.println("Metereologia: " + metereologia + "\nTrânsito: " + transito + "\n");

                    break;
                default:
                    System.out.println("Insira uma opção válida!!!");
                    break;
            }
        }
    }

    /**
     * Método que apresenta o menu registar
     */
    public void menuRegistar(){

        Scanner sc = new Scanner(System.in);

        String scan = "";

        while(!scan.equals("0")) {
            System.out.println("---------------- Menu Registar ---------------------");
            System.out.println("0 - Sair");
            System.out.println("1 - Utilizador");
            System.out.println("2 - Loja");
            System.out.println("3 - Voluntário");
            System.out.println("4 - Empresa de Transporte");

            scan = sc.next();

            switch (scan) {
                case "0":
                    break;
                case "1":
                    registarUtilizador();
                    break;
                case "2":
                    registarLoja();
                    break;
                case "3":
                    registarVoluntario();
                    break;
                case "4":
                    registarEmpresa();
                    break;
                default:
                    System.out.println("Insira uma opção válida!!!");
                    break;
            }
        }
    }

    /**
     * Método que guarda os dados de um
     * registo e devolve na forma de uma lista
     * @param tipo
     * @return
     */
    public List<String> registar(String tipo) {
        Scanner sc = new Scanner(System.in).useDelimiter("\n");
        List<String> params = new ArrayList<>();

        System.out.println("Inserir Nome");
        params.add(sc.next());
        System.out.println("Inserir Email");
        params.add(sc.next());
        System.out.println("Inserir Password");
        params.add(sc.next());
        System.out.println("Inserir Nif");
        params.add(sc.next());
        System.out.println("Inserir Latitude");
        boolean lido = false;
        while (!lido){
            try {
                double lat = Double.parseDouble(sc.next());
                params.add(String.valueOf(lat));
                lido = true;
            } catch (NumberFormatException e) {
                System.out.println("Não foi possível ler latitude, volta a inserir!");
            }
        }
        System.out.println("Inserir Longitude");

        /*lido = false;
        while(!lido) {
            try {
                double lon = sc.nextDouble();
                params.add(String.valueOf(lon));
                lido = true;
            } catch (InputMismatchException e) {
                System.out.println("Não foi possível ler longitude, volte a inserir!");
            }
        }*/

        lido = false;
        while(!lido) {
            try {
                double lon = Double.parseDouble(sc.next());
                params.add(String.valueOf(lon));
                lido = true;
            } catch (NumberFormatException e) {
                System.out.println("Não foi possível ler longitude, volte a inserir!");
            }
        }

        if (tipo.equals("V")){
            System.out.println("Inserir raio");
            params.add(sc.next());
            System.out.println("Qual a velocidade média");
            params.add(sc.next());
        }
        if (tipo.equals("E")){
            System.out.println("Inserir raio");
            params.add(sc.next());
            System.out.println("Inserir precoKm");
            params.add(sc.next());
            System.out.println("Qual a velocidade média");
            params.add(sc.next());
            System.out.println("Qual o preço por Kg");
            params.add(sc.next());
        }

        return params;
    }

    /**
     * Método que regista um utilizador
     */
    public void registarUtilizador(){
        List<String> params = registar("U");
        try {
            this.controller.processaRegistaUtilizador(params.get(0), params.get(1), params.get(2), params.get(3), params.get(4),  params.get(5));
        }
        catch (EmailExistenteException e){
            System.out.println("Email inserido já existe");
        }
    }

    /**
     * Método que regista uma loja
     */
    public void registarLoja(){
        List<String> params = registar("L");
        try {
            this.controller.processaRegistaLoja(params.get(0), params.get(1), params.get(2), params.get(3), params.get(4), params.get(5));
        }
        catch (EmailExistenteException e){
            System.out.println("Email inserido já existe");
        }
    }

    /**
     * Método que regista um voluntário
     */
    public void registarVoluntario(){
        List<String> params = registar("V");
        try {
            this.controller.processaRegistaVoluntario(params.get(0), params.get(1), params.get(2), params.get(3),
                    params.get(4), params.get(5), params.get(6), params.get(7));
        }
        catch (EmailExistenteException e){
            System.out.println("Email inserido já existe");
        }
    }

    /**
     * Método que regista uma empresa
     */
    public void registarEmpresa(){
        List<String> params = registar("E");
        Scanner sc = new Scanner(System.in);
        System.out.println("É uma empresa preparado para transportar encomendas médicas? (sim ou nao)");
        String medicaS = sc.next();
        boolean medica = false;
        if(medicaS.equals("sim"))
            medica = true;

        try {
            this.controller.processaRegistaEmpresa(params.get(0), params.get(1), params.get(2), params.get(3),
                    params.get(4), params.get(5), params.get(6), params.get(7), medica, params.get(8), params.get(9));
        }
        catch (EmailExistenteException e){
            System.out.println("Email inserido já existe");
        }
    }

    /**
     * Método responsável pelo menu login
     */
    public void menuLogin(){

        Scanner sc = new Scanner(System.in);
        List<String> params;
        String cod = "";

        int scan = -1;

        while(scan != 0) {

            System.out.println("---------------- Menu Login ---------------------");
            System.out.println("0 - Sair");
            System.out.println("1 - Utilizador");
            System.out.println("2 - Loja");
            System.out.println("3 - Voluntário");
            System.out.println("4 - Empresa de Transporte");

            scan = lerOpcao();

            switch (scan) {
                case 0:
                    break;
                case 1:
                    params = login();
                    try {
                        cod = this.controller.processaLogin("U", params.get(0), params.get(1));
                        menuUtilizador(cod);
                    }
                    catch (EmailNaoValidoException e){
                        System.out.println("Email inválido");
                    }
                    catch (PasswordNaoValidoException e){
                        System.out.println("Password inválida");
                    }
                    break;
                case 2:
                    params = login();
                    try {
                        cod = this.controller.processaLogin("L", params.get(0), params.get(1));
                        menuLoja(cod);
                    }
                    catch (EmailNaoValidoException e){
                        System.out.println("Email inválido");
                    }
                    catch (PasswordNaoValidoException e){
                        System.out.println("Password inválida");
                    }
                    break;
                case 3:
                    params = login();
                    try {
                        cod = this.controller.processaLogin("V", params.get(0), params.get(1));
                        menuVoluntario(cod);
                    }
                    catch (EmailNaoValidoException e){
                        System.out.println("Email inválido");
                    }
                    catch (PasswordNaoValidoException e){
                        System.out.println("Password inválida");
                    }
                    break;
                case 4:
                    params = login();
                    try {
                        cod = this.controller.processaLogin("E", params.get(0), params.get(1));
                        menuEmpresa(cod);
                    }
                    catch (EmailNaoValidoException e){
                        System.out.println("Email inválido");
                    }
                    catch (PasswordNaoValidoException e){
                        System.out.println("Password inválida");
                    }
                    break;
                default:
                    System.out.println("Insira uma opção válida!!!");
                    break;
            }
        }
    }

    /**
     * Método que guarda os dados do login e
     * devolve na forma de uma lista
     * @return
     */
    public List<String> login(){
        Scanner sc = new Scanner(System.in);
        List<String> params = new ArrayList<>();

        System.out.println("Inserir Email");
        params.add(sc.next());
        System.out.println("Inserir Password");
        params.add(sc.next());

        return params;
    }

    /**
     * Método responsável pelo menu do Utilizador
     * @param cod
     */
    public void menuUtilizador(String cod){

        Scanner sc = new Scanner(System.in);

        int scan = -1;

        while(scan != 0) {
            System.out.println("---------------- Menu Utilizador ---------------------");
            System.out.println("---------------------Notificações-------------------------");
            String aguardar = this.controller.processaEncomendasAguardandoUtilizador(cod);
            if(aguardar.equals("")) System.out.println("Não existem transporte de encomendas por validar");
            else{
                System.out.println("->Transporte de encomendas por aceitar:\n");
                System.out.println(aguardar);
            }
            System.out.println("----------------------------------------------------------");

            System.out.println("0 - Sair");
            System.out.println("1 - Pedir Encomenda");
            System.out.println("2 - Classificar entrega");
            System.out.println("3 - Ver histórico de entregas");
            System.out.println("4 - Ver produtos");
            System.out.println("5 - Aceitar transporte de encomenda");
            System.out.println("6 - Recusar transporte de encomenda");
            System.out.println("7 - Ver histórico de entregas de um voluntário/empresa num dado período de tempo");

            scan = lerOpcao();

            switch (scan) {
                case 0:
                    System.out.println("Adeus " + this.controller.processaGetNomeUt(cod));
                    break;
                case 1:
                    boolean medica = false;
                    boolean medicaLida = false;
                    System.out.println("É uma encomenda médica (sim ou nao)?");
                    while(!medicaLida) {
                        String medicaS = sc.next();
                        if (medicaS.equals("sim")){
                            medica = true;
                            medicaLida = true;
                        }
                        else if(medicaS.equals("nao")){
                            medica = true;
                            medicaLida = true;
                        }
                        if(!medicaLida) System.out.println("Opção inválida, volte a inserir!");
                    }


                    List<List<String>> encs = new ArrayList<>();
                    System.out.println("Insira o código da Loja");
                    String codLoja = sc.next();
                    menuEncomenda(false, encs);
                    System.out.println("Nº de encomendas: " + encs.size());
                    try {
                        this.controller.processaPedidoEncomenda(cod, codLoja, encs, medica);
                        System.out.println("Encomenda registada com sucesso");
                    }
                    catch (ProdutosNaoExisteException e) {
                        System.out.println("Não existe o produto: " + e.getMessage());
                    }
                    catch (LojaInexistenteException e) {
                        System.out.println("Não existe a loja: " + e.getMessage());
                    }
                    break;
                case 2:
                    System.out.println("Insira o código da encomenda");
                    String codEnc = sc.next();
                    System.out.println("Insira uma classificação para a encomenda");
                    int classificacao = Integer.parseInt(sc.next());
                    try {
                        this.controller.processaClassificarEncomenda(codEnc, classificacao);
                    }
                    catch (EncomendaInexistenteException e){
                        System.out.println("Não existe a encomenda " + e.getMessage());
                    }
                    break;
                case 3:
                    System.out.println(this.controller.processaHistoricoEntregasUt(cod));
                    break;
                case 4:
                    System.out.println(this.controller.processaVerProdutos());
                    break;
                case 5:
                    System.out.println("Qual a encomenda que deseja aceitar?");
                    String enc = sc.next();
                    System.out.println("Qual a empresa que deseja aceitar?");
                    String codTrans = sc.next();
                    try{
                        this.controller.processaAceitaEncomendaUtilizador(enc, codTrans);
                    }
                    catch (EncomendaInexistenteException e) {
                        System.out.println("Não existe a encomenda: " + e.getMessage());
                    }
                    catch (EmpresaInexistenteException e) {
                        System.out.println("Não existe a empresa: " + e.getMessage());
                    }
                    break;
                case 6:
                    System.out.println("Qual a encomenda que deseja rejeitar?");
                    String enc1 = sc.next();
                    System.out.println("Qual a empresa que deseja rejeitar?");
                    String codTransp = sc.next();
                    try{
                        this.controller.processaRecusaEncomendaUtilizador(enc1, codTransp);
                    }
                    catch (EncomendaInexistenteException e){
                        System.out.println("Não existe a encomenda: " + e.getMessage());
                    }
                    catch (EmpresaInexistenteException e){
                        System.out.println("Não existe a empresa: " + e.getMessage());
                    }
                    break;

                case 7:
                    System.out.println("Qual o voluntário ou a empresa?");
                    String vol = sc.next();

                    System.out.println("Qual o dia, mes e ano da data inicial (separados por \" - \" )?");
                    String dataIS = sc.next();
                    System.out.println("Qual o dia, mes e ano da data final (separados por \" - \" )?");
                    String dataFS = sc.next();
                    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");

                    try {
                        LocalDate dataI = LocalDate.parse(dataIS, formatter);
                        LocalDate dataF = LocalDate.parse(dataFS, formatter);

                        System.out.println(this.controller.processaHistoricoEncomendasTransportadoraData(vol, dataI, dataF));
                    }
                    catch (DateTimeParseException e){
                        System.out.println("Data inserida inválida");
                    }
                    catch (EmpresaInexistenteException e){
                        System.out.println("Empresa " + e.getMessage() + " inexistente!");
                    }
                    catch(VoluntarioInexistenteException e){
                        System.out.println("Voluntário " + e.getMessage() + " inexistente!");
                    }


                    esperaAteContinuar();
                    break;

                default:
                    System.out.println("Insira uma opção válida!!!");
                    break;
            }
        }
    }

    /**
     * Método responsável pelo menu da encomenda
     * @param flag
     * @param encs
     */
    public void menuEncomenda(boolean flag, List<List<String>> encs){
        Scanner sc = new Scanner(System.in);

        if(!flag){
            encs.add(menuPedirEncomenda());
        }

        System.out.println("0 - Sair");
        System.out.println("1 - Inserir mais um produto");

        switch (sc.next()){
            case "0":
                return;
            case "1":
                flag = true;
                encs.add(menuPedirEncomenda());
                menuEncomenda(flag, encs);
                break;
            default:
                System.out.println("Insira uma opção válida!!!");
                break;
        }
    }

    /**
     * Método que guarda os dados de uma encomenda e
     * devolve na forma de uma lista
     * @return
     */
    public List<String> menuPedirEncomenda(){
        Scanner sc = new Scanner(System.in);
        List<String> params = new ArrayList<>();

        System.out.println("Insira o código do produto");
        params.add(sc.next());
        System.out.println("Insira a quantidade da encomenda");
        params.add(sc.next());

        return params;
    }

    /**
     * Método responsável pelo menu de uma loja
     * @param cod
     */
    public void menuLoja(String cod){

        Scanner sc = new Scanner(System.in).useDelimiter("\n");

        String scan = "";

        while(!scan.equals("0")) {

            System.out.println("---------------- Menu Loja ---------------------");
            System.out.println("0 - Sair");
            System.out.println("1 - Ver encomendas pedidas");
            System.out.println("2 - Sinalizar uma encomenda como pronta para ser entregue");
            System.out.println("3 - Adicionar produto");
            System.out.println("4 - Ver histórico de encomendas");
            System.out.println("5 - Ver faturação num determinado intervalo de tempo");
            System.out.println("6 - Atualizar o tempo médio de espera de cada encomenda");
            System.out.println("7 - Atualizar a fila de espera para levantar encomendas");

            scan = sc.next();

            switch (scan) {
                case "0":
                    break;

                case "1":
                    System.out.println(this.controller.processaEncomendasPedidas(cod));
                    break;

                case "2":
                    Scanner sc1 = new Scanner(System.in);
                    System.out.println("Indique o código da encomenda: ");
                    String codEnc = sc1.next();
                    try {
                        this.controller.processaEncomendaPronta(cod, codEnc);
                    }
                    catch (EncomendaInexistenteException e) {
                        System.out.println("Não existe a encomenda: " + e.getMessage());
                    }
                    catch (LojaInexistenteException e) {
                        System.out.println("Não existe a loja: " + e.getMessage());
                    }
                    break;

                case "3":
                    System.out.println("Insira a descrição do produto");
                    String descricao = "";
                    descricao += sc.next();
                    System.out.println("Insira o peso do produto");
                    String peso = sc.next();
                    System.out.println("Insira o preco do produto");
                    String preco = sc.next();
                    this.controller.processaRegistaProduto(cod, descricao, peso, preco);
                    break;

                case "4":
                    System.out.println(this.controller.processaHistoricoEntregasLoja(cod));
                    break;

                case "5":

                    System.out.println("Qual o dia, mes e ano da data inicial (separados por \" - \" )?");
                    String dataIS = sc.next();
                    System.out.println("Qual o dia, mes e ano da data final (separados por \" - \" )?");
                    String dataFS = sc.next();
                    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");

                    try {
                        LocalDate dataI = LocalDate.parse(dataIS, formatter);
                        LocalDate dataF = LocalDate.parse(dataFS, formatter);

                        System.out.println("Faturação igual a " + this.controller.processaFaturacaoLoja(cod, dataI, dataF));
                    }
                    catch (DateTimeParseException e){
                        System.out.println("Data inserida inválida");
                    }
                    catch (LojaInexistenteException e){
                        System.out.println("Não existe a loja " + e.getMessage());
                    }

                    esperaAteContinuar();
                    break;

                case "6" :
                    System.out.println("Qual o novo tempo de espera médio? (dias- horas-minutos)");
                    String tempo = sc.next();
                    String[] valores = tempo.split("-");

                    try {
                        Duration tempoEspera = Duration.ofDays(0);
                        tempoEspera = tempoEspera.plusDays(Long.parseLong(valores[0]));
                        tempoEspera = tempoEspera.plusHours(Long.parseLong(valores[1]));
                        tempoEspera = tempoEspera.plusMinutes(Long.parseLong(valores[2]));
                        this.controller.processaAtualizaTempoEspera(cod, tempoEspera);
                    }
                    catch (DateTimeException e){
                        System.out.println("Hora inserida inválida");
                    }
                    catch (ArithmeticException e){
                        System.out.println("Hora inserida inválida");
                    }
                    catch (NumberFormatException e){
                        System.out.println("Hora inserida inválida");
                    }

                    break;

                case "7" :
                    System.out.println("Qual o tamanho da fila de espera neste momento? " +
                            "(digite -1 caso não pretende alimentar mais esta informação");
                    int tam = sc.nextInt();
                    try {
                        this.controller.processaTamanhoFila(cod, tam);
                    }
                    catch (FilaDeEsperaException e){
                        System.out.println("Tamanho " + e.getMessage() + "não suportado!");
                    }
                    break;

                default:
                    System.out.println("Insira uma opção válida!!!");
                    break;
            }
        }
    }

    /**
     * Método responsável pelo menu de um voluntário
     * @param cod
     */
    public void menuVoluntario(String cod){

        Scanner sc = new Scanner(System.in);

        String scan = "";

        while(!scan.equals("0")) {

            System.out.println("---------------- Menu Voluntário ---------------------");
            System.out.println("=========== Encomendas em Transporte ==============");
            String encs = this.controller.processaEncomendaParaTransporte(cod);
            //System.out.println((encs != null) ? encs : "");
            if(!encs.isEmpty()) System.out.println(encs);
            else System.out.println("Não se encontra a transportar nenhuma encomenda neste momento");
            System.out.println("0 - Sair");
            System.out.println("1 - Ver encomendas para entrega");
            System.out.println("2 - Transportar uma encomenda");
            System.out.println("3 - Sinalizar que uma encomenda foi entregue");
            System.out.println("4 - Ver histórico das encomendas");
            System.out.println("5 - Atualizar a velocidade média");

            scan = sc.next();

            switch (scan) {
                case "0":
                    break;
                case "1":
                    try {
                        System.out.println(this.controller.processaEncomendasParaEntrega(cod));
                    }
                    catch (EmpresaInexistenteException e) {
                        System.out.println("Não existe o voluntário: " + e.getMessage());
                    }
                    break;
                case "2":
                    System.out.println("Insira o código da encomenda que pretende transportar");
                    String codEnc = sc.next();
                    try {
                        this.controller.processaTransportarEncomendaVoluntario(cod, codEnc);
                    }
                    catch (EncomendaInexistenteException e){
                        System.out.println("Encomenda " + e.getMessage() + " não existe");
                    }
                    catch (TransportadoraNaoMedicaException e){
                        System.out.println("Encomenda " + e.getMessage() + " é uma encomenda médica, esta voluntário" +
                                " não está habilitada");
                    }
                    break;
                case "3":
                    System.out.println("Insira o código da encomenda que acabou de entregar");
                    codEnc = sc.next();
                    System.out.println("Qual a duração da entrega? (horas-minutos)");
                    String tempoE = sc.next();
                    String[] spl = tempoE.split("-");

                    try{
                        int horas = Integer.valueOf(spl[0]);
                        int minutos = Integer.valueOf(spl[1]);
                        Duration tempoEntrega = Duration.ZERO;
                        tempoEntrega = tempoEntrega.plusHours(horas);
                        tempoEntrega = tempoEntrega.plusMinutes(minutos);
                        this.controller.processaEncomendaEntregue(cod, codEnc, tempoEntrega);
                    }
                    catch(EncomendaNaoTransportadaException e){
                        System.out.println("Encomenda " + e.getMessage() + " não estava a ser transportada " +
                                "por este voluntário");
                    }
                    catch (ArithmeticException e){
                        System.out.println("Duração inserida inválida");
                    }
                    catch (NumberFormatException e){
                        System.out.println("Duração inserida inválida");
                    }

                    break;
                case "4":
                    System.out.println(this.controller.processaHistoricoEntregasTransportadora(cod));
                    break;

                case "5":
                    System.out.println("Qual a nova velocidade média?");
                    double novaVelocidade = Double.parseDouble(sc.next());

                    this.controller.processaAtualizaVelocidadeVol(cod, novaVelocidade);


                    break;
                default:
                    System.out.println("Insira uma opção válida!!!");
                    break;
            }
        }
    }

    /**
     * Método responsável pelo menu de uma empresa
     * @param cod
     */
    public void menuEmpresa(String cod){

        Scanner sc = new Scanner(System.in);

        String scan = "";

        while(!scan.equals("0")) {

            System.out.println("---------------- Menu Empresa ---------------------");
            System.out.println("---------------- Notificações ---------------------");
            System.out.println("============= Encomendas Aceites ==================");
            String encs = this.controller.processaEncomendasAceitesUtilizador(cod);
            //System.out.println((encs != null) ? encs : "");
            if(!encs.isEmpty()) System.out.println(encs);
            else System.out.println("Não se encontra disponível nenhuma encomenda para transporte");
            System.out.println("=========== Encomendas em Transporte ==============");
            encs = this.controller.processaEncomendaParaTransporte(cod);
            //System.out.println((encs != null) ? encs : "");
            if(!encs.isEmpty()) System.out.println(encs);
            else System.out.println("Não se encontra a transportar nenhuma encomenda neste momento");
            System.out.println("---------------------------------------------------");
            System.out.println("0 - Sair");
            System.out.println("1 - Ver encomendas para entrega");
            System.out.println("2 - Pedir para transportar uma encomenda");
            System.out.println("3 - Transportar uma encomenda");
            System.out.println("4 - Sinalizar que uma encomenda foi entregue");
            System.out.println("5 - Ver histórico das encomendas");
            System.out.println("6 - Ver faturação num determinado intervalo de tempo");
            System.out.println("7 - Atualizar a velocidade média");
            System.out.println("8 - Atualizar o preço por Km");
            System.out.println("9 - Atualizar o preço por Kg");
            System.out.println("10 - Transportar várias encomendas");

            scan = sc.next();

            switch (scan) {
                case "0":
                    break;
                case "1":
                    try {
                        System.out.println(this.controller.processaEncomendasParaEntrega(cod));
                    }
                    catch (EmpresaInexistenteException e) {
                        System.out.println("Não existe a empresa: " + e.getMessage());
                    }
                    break;
                case "2":
                    System.out.println("Insira o código que encomenda que pretende transportar");
                    String codEnc = sc.next();
                    try {
                        this.controller.processaPedidoTransporteEncomenda(cod, codEnc);
                    }
                    catch (EncomendaInexistenteException e){
                        System.out.println("Encomenda " + e.getMessage() + " não existe");
                    }
                    break;

                case "3":
                    System.out.println("Insira o código da encomenda que pretende transportar");
                    String codEnc1 = sc.next();
                    try {
                        this.controller.processaTransportarEncomendaEmpresa(cod, codEnc1);
                    }
                    catch (EncomendaInexistenteException e){
                        System.out.println("Encomenda " + e.getMessage() + " não existe");
                    }
                    catch (TransportadoraNaoMedicaException e){
                        System.out.println("Encomenda " + e.getMessage() + " é uma encomenda médica, esta empresa" +
                                " não está habilitada");
                    }
                    catch(EncomendaNaoAceiteUtilizadorException e){
                        System.out.println("Transporte da encomenda " + e.getMessage() + "não aceite pelo " +
                                "utilizador para este empresa");
                    }
                    break;

                case "4" :
                    System.out.println("Insira o código da encomenda que acabou de entregar");
                    codEnc = sc.next();
                    System.out.println("Qual a duração da entrega? (horas-minutos)");
                    String tempoE = sc.next();
                    String[] spl = tempoE.split("-");

                    try{
                        int horas = Integer.valueOf(spl[0]);
                        int minutos = Integer.valueOf(spl[1]);
                        Duration tempoEntrega = Duration.ZERO;
                        tempoEntrega = tempoEntrega.plusHours(horas);
                        tempoEntrega = tempoEntrega.plusMinutes(minutos);
                        this.controller.processaEncomendaEntregue(cod, codEnc, tempoEntrega);
                    }
                    catch(EncomendaNaoTransportadaException e){
                        System.out.println("Encomenda " + e.getMessage() + "não estava a ser transportada " +
                                "por esta empresa");
                    }
                    catch (ArithmeticException e){
                        System.out.println("Duração inserida inválida");
                    }
                    catch (NumberFormatException e){
                        System.out.println("Duração inserida inválida");
                    }


                    break;
                case "5":
                    System.out.println(this.controller.processaHistoricoEntregasTransportadora(cod));
                    break;

                case "6":
                    System.out.println("Qual o dia, mes e ano da data inicial (no formato DD-MM-AAAA)?");
                    String dataIS = sc.next();
                    System.out.println("Qual o dia, mes e ano da data final (no formato DD-MM-AAAA)?");
                    String dataFS = sc.next();
                    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");

                    try {
                        LocalDate dataI = LocalDate.parse(dataIS, formatter);
                        LocalDate dataF = LocalDate.parse(dataFS, formatter);
                        System.out.println("Faturação igual a " + this.controller.processaFaturacaoEmp(cod, dataI, dataF));
                    }
                    catch (DateTimeParseException e){
                        System.out.println("Insira uma data válida");
                    }
                    catch (EmpresaInexistenteException e){
                        System.out.println("Não existe a empresa " + e.getMessage());
                    }


                    esperaAteContinuar();

                    break;

                case "7":
                    System.out.println("Qual a nova velocidade média?");

                    double novaVelocidade = Double.parseDouble(sc.next());

                    this.controller.processaAtualizaVelocidadeEmp(cod, novaVelocidade);

                    break;

                case "8" :
                    System.out.println("Qual o novo preço por Km?");
                    double precoKm = Double.parseDouble(sc.next());

                    this.controller.processaAtualizaPrecoKm(cod, precoKm);

                    break;

                case "9" :
                    System.out.println("Qual o novo preço por Kg?");
                    double precoKg = Double.parseDouble(sc.next());

                    this.controller.processaAtualizaPrecoKg(cod, precoKg);

                    break;

                case "10" :
                    List<String> encomendasVarias = new ArrayList<>();
                    String lido = "";
                    while(!lido.equals("s")) {
                        System.out.println("Qual a próxima encomenda, por ordem de entrega, que pretende transportar ? " +
                                "(Pressione \"s\" caso pretenda terminar)");
                        lido = sc.next();
                        encomendasVarias.add(lido);
                    }

                    try {
                        this.controller.processapedidoTransporteEncomendaGrupo(cod, encomendasVarias);
                    }
                    catch(EncomendaInexistenteException e){
                        System.out.println("Encomenda "  + e.getMessage() + " indisponível ");
                    }

                    break;
                default:
                    System.out.println("Insira uma opção válida!!!");
                    break;
            }
        }
    }

    /**
     * Até ao utilizar digirar 'C' obriga o programa a aguardar
     */
    public static void esperaAteContinuar(){
        System.out.println("Pressione C para continuar");
        Scanner sc = new Scanner(System.in);

        while(!sc.next().equals("C"));
    }

}
