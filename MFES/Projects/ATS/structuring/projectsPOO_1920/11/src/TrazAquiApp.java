
import java.io.*;
import java.time.LocalDateTime;
import java.util.*;

public class TrazAquiApp{

    private TrazAqui logNegocio;

    private Menu init, difUsers, menuUtil, menuLoja, menuTransportadora, menuEncomenda,
            menuEncomendaPorEntregar, menuHistoricoEncomendas, menuEstatisticas,
            menuPropostaTransporte, menuDetalhesEncomenda, menuFaturado;

    public static void main(String[] args){
        new TrazAquiApp().run();
    }

    private TrazAquiApp(){
        String[] opcoesInit = {"Log In",
                                "Sign Up"};
        String[] opcoesDifUsers = {"Utilizador Singular",
                                    "Empresa Comercial",
                                    "Empresa Transportadora"};
        String[] opcoesMenuUtil ={"Fazer Encomenda",
                                    "Transportar encomendas",
                                    "Encomendas por entregar",
                                    "Historico de encomendas",
                                    "Estatisticas"};
        String[] opcoesMenuTransportador ={"Encomendas disponiveis",
                                            "Confirmar entrega de encomenda",
                                            "Historico de encomendas",
                                            "Procurar utilizadores",
                                            "Total dinheiro faturado"};
        String[] opcoesMenuLoja ={"Aceitar encomendas",
                                    "Indicar encomenda pronta",
                                    "Encomendas por entregar",
                                    "Encomendas em fila de espera",
                                    "Historico de encomendas",
                                    "Procurar utilizadores"};
        String[] opcoesMenuEncomenda = {"Fazer encomenda a loja",
                                        "Ordenar lojas por mais perto primeiro",
                                        "Ordenar lojas por melhor classificadas",
                                        "Ordenar lojas por ordem alfabética de nome",
                                        "Filtrar lojas disponiveis",
                                        "Filtrar lojas com fila",
                                        "Filtrar lojas sem fila"};
        String[] opcoesEncomendasPorEntregar = {"Propostas de transporte de encomenda",
                                                "Ordenar por tempo de pedido",
                                                "Filtrar encomenda médica"};
        String[] opcoesMenuHistorico = {"Ver detalhes de encomenda encomenda",
                                        "Ordenar encomendas por data de entrega",
                                        "Ordenar encomendas por peso",
                                        "Ordenar encomendas por preço",
                                        "Filtrar encomendas por user",
                                        "Filtrar encomendas medicinais",
                                        "Encomendas feitas hoje",
                                        "Encomendas feitas esta semana",
                                        "Encomendas feitas este mês"};
        String[] opcoesEstatisticas = {"Top 10 utilizadores da App(mais encomendas feitas)",
                                        "Top 10 voluntarios com mais kms",
                                        "Top 10 empresas com mais kms"};
        String[] opcoesPropostasTransporte = {"Aceitar proposta de tranporte",
                                                "Ordenar por preço proposto"};
        String[] opcoesDetalhesEncomenda = {"Classificar transportador",
                                            "Classificar loja"};
        String[] opcoesDinheiroFacturado = {"Hoje","Esta semana","Este mes","Sempre"};
        this.init = new Menu(opcoesInit);
        this.difUsers = new Menu(opcoesDifUsers);
        this.menuUtil = new Menu(opcoesMenuUtil);
        this.menuTransportadora = new Menu(opcoesMenuTransportador);
        this.menuLoja = new Menu(opcoesMenuLoja);
        this.menuEncomenda = new Menu(opcoesMenuEncomenda);
        this.menuEncomendaPorEntregar = new Menu(opcoesEncomendasPorEntregar);
        this.menuHistoricoEncomendas = new Menu(opcoesMenuHistorico);
        this.menuEstatisticas = new Menu(opcoesEstatisticas);
        this.menuPropostaTransporte = new Menu(opcoesPropostasTransporte);
        this.menuDetalhesEncomenda = new Menu(opcoesDetalhesEncomenda);
        this.menuFaturado = new Menu(opcoesDinheiroFacturado);
        try {
            this.logNegocio = TrazAqui.carregaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
        }
        catch (FileNotFoundException e) {
            System.out.println("Parece que é a primeira utilização, vamos importar informação do ficheiro de logs.");
            try {
                System.out.println("Carregamento de logs bem sucedido");
                this.logNegocio = TrazAqui.importaCSV("TrazAquiApp","/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/LogsGerados.csv");
            } catch (IOException ioException) {
                System.out.println("Erro ao importar os Logs");
            }
        }
        catch (IOException e) {
            System.out.println("Ops! Erro de leitura, vamos importar informação do ficheiro de logs.");
            try {
                System.out.println("Carregamento de logs bem sucedido");
                this.logNegocio = TrazAqui.importaCSV("TrazAquiApp","/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/LogsGerados.csv");
            } catch (IOException ioException) {
                System.out.println("Erro ao importar os Logs");
            }
        }
        catch (ClassNotFoundException e) {
            System.out.println("Ops! Formato de ficheiro de dados errado, vamos importar informação do ficheiro de logs.");
            try {
                System.out.println("Carregamento de logs bem sucedido");
                this.logNegocio = TrazAqui.importaCSV("TrazAquiApp","/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/LogsGerados.csv");
            } catch (IOException ioException) {
                System.out.println("Erro ao importar os Logs");
            }
        }
    }

    private void run(){
        this.logNegocio.verificaAceites();
        System.out.println(this.logNegocio.toString());
        do {
            init.executa();
            switch (init.getOpcao()) {
                case 1: logInData();
                    break;
                case 2: escolheUserType();
                    break;
            }
        } while (init.getOpcao()!=0);
    }

    public void logInData() {
        Scanner scin = new Scanner(System.in);

        System.out.println("Username: ");
        String username = scin.nextLine();
        System.out.println("Password: ");
        String password = scin.nextLine();
        try {
            this.logNegocio.existeUser(username,password);
            if(username.charAt(0) == 'u') {
                menuDeUtil(username);
            }
            if(username.charAt(0) == 'v' || username.charAt(0) == 't') {
                menuDeTransportador(username);
            }
            if(username.charAt(0) == 'l') {
                menuDeLoja(username);
            }
        }
        catch (UserInexistenteException | IOException | EncomendaInexistenteException e) {
            System.out.println(e.getMessage());
        }
    }

    public void escolheUserType(){
        do {
            difUsers.executa();
            switch (difUsers.getOpcao()) {
                case 1:
                    signUpDeUtil();
                    break;
                case 2:
                    signUpDeLoja();
                    break;
                case 3:
                    signUpDeEmpresa();
                    break;
            }
        } while (difUsers.getOpcao()!=0);
    }

    public void signUpDeUtil(){
        Scanner scin = new Scanner(System.in);

        System.out.println("Username: ");
        String username = scin.nextLine();
        System.out.println("Nome: ");
        String nome = scin.nextLine();
        System.out.println("Password: ");
        String password = scin.nextLine();
        System.out.println("Idade: ");
        int idade = Integer.parseInt(scin.nextLine());
        System.out.println("Sexo: ");
        String sexo = scin.nextLine();
        try {
            this.logNegocio.existeUsername(username);
            this.logNegocio.adicionaUser(TrazAqui.criaUtilizador(username,nome,password,TrazAqui.criaLocalizacao(0,0),idade,sexo));
            this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
            menuDeUtil(username);
        }
        catch (UserInexistenteException e) {
            System.out.println(e.getMessage());
        } catch (FileNotFoundException e) {
            System.out.println("Ficheiro objeto não encontrado");
        } catch (IOException e) {
            System.out.println("Erro a guardar no ficheiro objeto");
        } catch (EncomendaInexistenteException e) {
            e.printStackTrace();
        }
    }

    public void signUpDeEmpresa(){
        Scanner scin = new Scanner(System.in);

        System.out.println("Username: ");
        String username = scin.nextLine();
        System.out.println("Nome: ");
        String nome = scin.nextLine();
        System.out.println("Password: ");
        String password = scin.nextLine();
        System.out.println("NIF: ");
        String nif = scin.nextLine();
        System.out.println("Preço por km: ");
        double preco_km = Double.parseDouble(scin.nextLine());
        System.out.println("Preço por kg: ");
        double preco_kg = Double.parseDouble(scin.nextLine());
        System.out.println("Raio de ação: ");
        double raio = Double.parseDouble(scin.nextLine());
        System.out.println("Latitude: ");
        double latitude = Double.parseDouble(scin.nextLine());
        System.out.println("Longitude: ");
        double longitude = Double.parseDouble(scin.nextLine());
        System.out.println("Encontra-se disponivel?[s/n]: ");
        boolean disponivel = s_ou_n(scin.nextLine());
        System.out.println("Faz o transporte de material medicial?[s/n]: ");
        boolean medica = s_ou_n(scin.nextLine());

        try {
            this.logNegocio.existeUsername(username);
            this.logNegocio.adicionaUser(TrazAqui.criaEmpresa(username,nome,password,TrazAqui.criaLocalizacao(latitude,longitude),raio,disponivel,medica,
                                                                new HashMap<>(),new HashMap<>(),preco_km,preco_kg,nif));
            this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
            menuDeTransportador(username);
        }
        catch (UserInexistenteException e) {
            System.out.println(e.getMessage());
        } catch (FileNotFoundException e) {
            System.out.println("Ficheiro objeto não encontrado");
        } catch (IOException e) {
            System.out.println("Erro a guardar no ficheiro objeto");
        } catch (EncomendaInexistenteException e) {
            e.printStackTrace();
        }
    }

    public boolean s_ou_n(String b){
        if(b.equals("s")){
            return true;
        }
        if(b.equals("n")){
            return false;
        }
        return false;
    }

    public void signUpDeLoja(){
        Scanner scin = new Scanner(System.in);

        System.out.println("Username: ");
        String username = scin.nextLine();
        System.out.println("Nome: ");
        String nome = scin.nextLine();
        System.out.println("Password: ");
        String password = scin.nextLine();
        System.out.println("Latitude: ");
        double latitude = Double.parseDouble(scin.nextLine());
        System.out.println("Longitude: ");
        double longitude = Double.parseDouble(scin.nextLine());
        System.out.println("Tem fila de espera?[s/n]: ");
        boolean fila = s_ou_n(scin.nextLine());
        int tam = 0;
        if(fila){
            System.out.println("De que tamanho?: ");
            tam = Integer.parseInt(scin.nextLine());
        }

        try {
            this.logNegocio.existeUsername(username);
            this.logNegocio.adicionaUser(TrazAqui.criaLoja(username,nome,password,TrazAqui.criaLocalizacao(latitude,longitude),new HashMap<>(),new HashMap<>(),fila,tam));
            this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
            menuDeLoja(username);
        }
        catch (UserInexistenteException | EncomendaInexistenteException e) {
            System.out.println(e.getMessage());
        } catch (FileNotFoundException e) {
            System.out.println("Ficheiro objeto não encontrado");
        } catch (IOException e) {
            System.out.println("Erro a guardar no ficheiro objeto");
        }

    }

    public void menuDeUtil(String username) throws IOException, EncomendaInexistenteException, UserInexistenteException {
        Scanner scin = new Scanner(System.in);
        do {
            menuUtil.executa();
            switch (menuUtil.getOpcao()) {
                case 1:
                    System.out.println("Insira as coordenadas para entrega: ");
                    System.out.println("Latitude: ");
                    double latitude = Double.parseDouble(scin.nextLine());
                    System.out.println("Longitude: ");
                    double longitude = Double.parseDouble(scin.nextLine());
                    fazerEncomenda(latitude,longitude, username);
                    break;
                case 2:
                    System.out.println("Quer fazer transporte médico?[true/false]: ");
                    Boolean medico =Boolean.parseBoolean(scin.nextLine());
                    System.out.println("Latitude: ");
                    double latitude1 = Double.parseDouble(scin.nextLine());
                    System.out.println("Longitude: ");
                    double longitude1 = Double.parseDouble(scin.nextLine());
                    System.out.println("Raio de ação: ");
                    double raio = Double.parseDouble(scin.nextLine());
                    String vusername = this.logNegocio.user2Vol(username, medico, raio,latitude1,longitude1);
                    this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
                    menuDeTransportador(vusername);
                    break;
                case 3:
                    //Encomendas por entregar(ver pedidos de transporte)
                    menuDeEncomendasPorEntregar(username);
                    break;
                case 4:
                    //historico de encomendas
                    historicoEncomendas(username);
                    break;
                case 5:
                    //estatisticas
                    estatisticas();
                    break;
            }
        } while (menuUtil.getOpcao()!=0);
    }

    public void fazerEncomenda(double latitude, double longitude, String username) throws IOException, UserInexistenteException, EncomendaInexistenteException {
        Scanner scin = new Scanner(System.in);
        StringBuilder lojas = new StringBuilder("*** Lojas registadas ***\n");
        for(String ln: this.logNegocio.listOfLojasInfo(this.logNegocio.listOfLojas())){
            lojas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
        }
        do {
            System.out.println(lojas);
            menuEncomenda.executa();
            switch (menuEncomenda.getOpcao()) {
                case 1:
                    System.out.println("Username da loja: ");
                    String nloja = scin.nextLine();
                    if(this.logNegocio.contemLoja(nloja)){
                        String[][] matrix = new String[50][4];
                        String ex = "s";
                        int ind = 0;
                        while(ex.equals("s")) {
                            System.out.println("Referencia: ");
                            String ref = scin.nextLine();
                            matrix[ind][0] = ref;
                            System.out.println("Descrição do produto: ");
                            String desc = scin.nextLine();
                            matrix[ind][1] = desc;
                            System.out.println("Preço: ");
                            String preco = scin.nextLine();
                            matrix[ind][2] = preco;
                            System.out.println("Quantidade: ");
                            String quant = scin.nextLine();
                            matrix[ind][3] = quant;
                            System.out.println("Deseja juntar outro produto[s/n]");
                            ex = scin.nextLine();
                            ind += 1;
                        }
                        System.out.println("É transporte médico?[s/n]: ");
                        String medica = scin.nextLine();
                        Random r = new Random();
                        if(medica.equals("s")) {
                            this.logNegocio.adicionaEncomenda(this.logNegocio.criaEncomenda(this.logNegocio.geraCodEncomenda(), username, nloja, "", LocalDateTime.now(), r.nextDouble(), matrix, ind, true, false));
                        }
                        else{
                            if(medica.equals("n")) {
                                this.logNegocio.adicionaEncomenda(this.logNegocio.criaEncomenda(this.logNegocio.geraCodEncomenda(), username, nloja, "", LocalDateTime.now(), r.nextDouble(), matrix, ind, false, false));
                            }
                            else{
                                System.out.println("A sua escolha é invalida");
                            }
                        }
                        this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
                        System.out.println("A sua encomenda foi realizada, muito obrigado");
                    }
                    else{
                        System.out.println("A loja não pode ser escolhida");
                    }
                    break;
                case 2:
                    lojas = new StringBuilder("\n*** Lojas registadas ***\n");
                    for(String ln: this.logNegocio.listOfLojasInfo(this.logNegocio.ordenaLojasDistancia(latitude,longitude))){
                        lojas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 3:
                    lojas = new StringBuilder("\n*** Lojas registadas ***\n");
                    for(String ln: this.logNegocio.listOfLojasInfo(this.logNegocio.ordenaLojasClass())){
                        lojas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 4:
                    lojas = new StringBuilder("\n*** Lojas registadas ***\n");
                    for(String ln: this.logNegocio.listOfLojasInfo(this.logNegocio.ordenaLojasNome())) {
                        lojas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 5:
                    break;
                case 6:
                    lojas = new StringBuilder("\n*** Lojas registadas ***\n");
                    for(String ln: this.logNegocio.listOfLojasInfo(this.logNegocio.listOfLojasComFila())){
                        lojas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 7:
                    lojas = new StringBuilder("\n*** Lojas registadas ***\n");
                    for(String ln: this.logNegocio.listOfLojasInfo(this.logNegocio.listOfLojasSemFila())){
                        lojas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
            }
            //System.out.println(this.logNegocio.toString());
        } while (menuEncomenda.getOpcao()!=0);
    }

    public void menuDeTransportador(String username) throws EncomendaInexistenteException, IOException, UserInexistenteException {
        Scanner scin = new Scanner(System.in);
        do {
            //print com as encomendas que transporta no momento
            StringBuilder encomendas = new StringBuilder("*** Encomendas a transportar ***\n");
            for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.listOfEncomendasTransportador(username))){
                encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
            }
            System.out.println(encomendas);
            menuTransportadora.executa();
            switch (menuTransportadora.getOpcao()) {
                case 1:
                    encomendasDisponiveis(username);
                    break;
                case 2:
                    System.out.println("Insira o código da encomenda: ");
                    String cod = scin.nextLine();
                    if(this.logNegocio.contemEncomenda(this.logNegocio.listOfEncomendasTransportador(username),username,cod)) {
                        System.out.println(this.logNegocio.getEncomendaString(cod));
                        System.out.println("Deseja confirmar a entrega?[true/false]: ");
                        boolean b = Boolean.parseBoolean(scin.nextLine());
                        if (b) {
                            this.logNegocio.confirmarEntEncomenda(cod, username);
                        }
                    }
                    else{
                        System.out.println("Opção errada");
                    }
                    this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
                    break;
                case 3:
                    //historico de encomendas
                    historicoEncomendas(username);
                    break;
                case 4:
                    //procurar utilizadores
                    estatisticas();
                    break;
                case 5:
                    //total faturado pela empresa
                    if(this.logNegocio.eEmpresa(username)){
                       dinheiroFat(username);
                    }
                    else{
                        System.out.println("Não tem acesso a este menu");
                    }
                    break;
            }
        } while (menuTransportadora.getOpcao()!=0);
    }

    public void dinheiroFat(String username){
        do{
            menuFaturado.executa();
            switch(menuFaturado.getOpcao()){
                case 1:
                    System.out.println("Hoje faturou: " + this.logNegocio.saldoHoje(username));
                    break;
                case 2:
                    System.out.println("Esta semana faturou: " + this.logNegocio.saldoSemana(username));
                    break;
                case 3:
                    System.out.println("Este mes faturou: " + this.logNegocio.saldoMes(username));
                    break;
                case 4:
                    System.out.println("Desde o inicio, faturou: " + this.logNegocio.saldoSempre(username));
                    break;
            }
        }while(menuFaturado.getOpcao()!=0);
    }

    public void encomendasDisponiveis(String username) throws IOException {
        Scanner scin = new Scanner(System.in);
        do {
            StringBuilder encomendas = new StringBuilder("*** Encomendas disponiveis ***\n");
            for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.listOfEncomendas(username))){
                encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
            }
            System.out.println(encomendas);
            menuEncomendaPorEntregar.executa();
            switch (menuEncomendaPorEntregar.getOpcao()) {
                case 1:
                    if(username.charAt(0) == 'v'){
                        System.out.println("Insira o código da encomenda: ");
                        String cod = scin.nextLine();
                        if(this.logNegocio.contemEncomenda(this.logNegocio.listOfEncomendas(username),username,cod)) {
                            this.logNegocio.realizaEncomenda(cod, username);
                        }
                        else{
                            System.out.println("Codigo invalido");
                        }
                    }
                    if(username.charAt(0) == 't'){
                        System.out.println("Insira o código da encomenda para fazer proposta: ");
                        String cod = scin.nextLine();
                        //System.out.println(cod);
                        if(this.logNegocio.contemEncomenda(this.logNegocio.listOfEncomendas(username),username,cod)) {
                            this.logNegocio.realizaProposta(cod, username);
                        }
                        else{
                            System.out.println("Codigo invalido");
                        }
                    }
                    this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
                    break;
                case 2:
                    // ordenar por tempo do pedido
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.ordenarEncomendaDataIT(username))){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 3:
                    //filtrar encomendas medicas
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.filtrarEncomendaMedicaT(username))){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
            }
        } while (menuEncomendaPorEntregar.getOpcao()!=0);
        //System.out.println(this.logNegocio.toString());
    }

    public void menuDeEncomendasPorEntregar(String username) throws IOException {
        Scanner scin = new Scanner(System.in);
        StringBuilder encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
        for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.listOfEncomendasPorEntregar(username))){
            encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
        }
        do {
            System.out.println(encomendas);
            menuEncomendaPorEntregar.executa();
            switch (menuEncomendaPorEntregar.getOpcao()) {
                case 1:
                    System.out.println("Insira o código da encomenda que deseja selecionar: ");
                    String cod = scin.nextLine();
                    if(this.logNegocio.contemEncomenda(this.logNegocio.listOfEncomendasPorEntregar(username),username,cod)) {
                        menuDePropostasTransporte(cod, username);
                    }
                    else{
                        System.out.println("Codigo invalido");
                    }
                    break;
                case 2:
                    //ordenar por tempo de pedido
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.ordenarEncomendaDataI(username))){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 3:
                    //Filtrar encomendas medicas
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.filtrarEncomendaMedica(username))){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
            }
        } while (menuEncomendaPorEntregar.getOpcao()!=0);
    }

    public void menuDePropostasTransporte(String cod, String username) throws IOException {
        Scanner scin = new Scanner(System.in);
        StringBuilder encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
        for(String ln: this.logNegocio.listOfTransportadoresInfo(cod,this.logNegocio.listOfPropostas(cod))){
            encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
        }
        do {
            System.out.println(encomendas);
            menuPropostaTransporte.executa();
            switch (menuPropostaTransporte.getOpcao()) {
                case 1:
                    System.out.println("Insira o username da empresa: ");
                    String nome = scin.nextLine();
                    if(this.logNegocio.contemProposta(this.logNegocio.listOfEncomendasPorEntregar(username),username,cod,nome)) {
                        this.logNegocio.realizaEncomenda(cod, nome);
                    }
                    else{
                        System.out.println("Codigo invalido");
                    }
                    this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
                    break;
                case 2:
                    //ordenar por preço proposto
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfTransportadoresInfo(cod,this.logNegocio.ordenarTransportadoraPreco(cod))){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
            }
        } while (menuPropostaTransporte.getOpcao()!=0);
    }

    public void menuDeLoja(String username) throws UserInexistenteException, EncomendaInexistenteException, IOException {
        Scanner scin = new Scanner(System.in);
        do {
            StringBuilder encomendas = new StringBuilder("*** Encomendas por Aceitar ***\n");
            for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.listOfEncomendasPorAceitar(username))){
                encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
            }
            System.out.println(encomendas);
            menuLoja.executa();
            switch (menuLoja.getOpcao()) {
                case 1:
                    //aceitar encomendas
                    System.out.println("Insira o código da encomenda que deseja aceitar: ");
                    String cod = scin.nextLine();
                    if(this.logNegocio.contemEncomenda(this.logNegocio.listOfEncomendasPorAceitar(username),username,cod)) {
                        if (this.logNegocio.temFila(username)) {
                            this.logNegocio.adicionaEncFila(username, cod);
                        } else {
                            this.logNegocio.adicionaEncLoja(username, cod);
                        }
                        this.logNegocio.adicionaAceites(cod);
                    }
                    else{
                        System.out.println("Codigo invalido");
                    }
                    this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
                    break;
                case 2:
                    //indicar que uma encomenda da fila encontra-se pronta
                    if(this.logNegocio.temFila(username)) {
                        System.out.println("A encomenda em primeiro lugar na fila está pronta?: ");
                        boolean cd = Boolean.parseBoolean(scin.nextLine());
                        if (cd) {
                            String cod2 = this.logNegocio.retEncFilaCod(username);
                            this.logNegocio.adicionaEncLoja(username, cod2);
                            this.logNegocio.retiraEncFila(username, cod2);
                        }
                    }
                    else{
                        System.out.println("Esta loja não pode aceder a esta função pois não tem fila");
                    }
                    this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
                    break;
                case 3:
                    //encomendas por entregar
                    StringBuilder encomendas2 = new StringBuilder("*** Encomendas prontas ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.listOfEncomendasProntas(username))){
                        encomendas2.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    System.out.println(encomendas2);
                    break;
                case 4:
                    //encomendas em fila de espera
                    if(this.logNegocio.temFila(username)){
                        StringBuilder encomendas3 = new StringBuilder("*** Encomendas em fila ***\n");
                        for (String ln : this.logNegocio.listOfEncomendasInfo(this.logNegocio.listOfEncomendasFila(username))) {
                            encomendas3.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                        }
                        System.out.println(encomendas3);
                    }
                    else{
                        System.out.println("A sua loja não tem fila");
                    }
                    break;
                case 5:
                    //historico de encomendas
                    historicoEncomendas(username);
                    break;
                case 6:
                    //Procurar Utilizador
                    estatisticas();
                    break;
            }
            //System.out.println(this.logNegocio.toString());
        } while (menuLoja.getOpcao()!=0);
    }

    public void historicoEncomendas(String username) throws EncomendaInexistenteException, UserInexistenteException, IOException {
        Scanner scin = new Scanner(System.in);
        StringBuilder encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
        for(String ln: this.logNegocio.listOfEncomendasInfo(this.logNegocio.listOfEncomendasHistorico(username))){
            encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
        }
        do {
            System.out.println(encomendas);
            menuHistoricoEncomendas.executa();
            switch (menuHistoricoEncomendas.getOpcao()) {
                case 1:
                    //detalhes
                    System.out.println("Insira o código da encomenda: ");
                    String cod = scin.nextLine();
                    if(this.logNegocio.contemEncomenda(this.logNegocio.listOfEncomendasHistorico(username),username,cod)) {
                        detalhesEncomenda(username, cod);
                    }
                    else{
                        System.out.println("Codigo errado");
                    }
                    break;
                case 2:
                    //ordenar por data
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasHistoricoInfo(username,this.logNegocio.ordenarEncomendaData())){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 3:
                    //ordenar por peso
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasHistoricoInfo(username,this.logNegocio.ordenarEncomendaPeso())){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 4:
                    //ordenar por preço
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasHistoricoInfo(username,this.logNegocio.ordenarEncomendaPreco())){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 5:
                    //filtrar por user
                    System.out.println("Insira o user que deseja filtrar");
                    String username2 = scin.nextLine();
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasHistoricoInfo(username,this.logNegocio.filtrarEncomendaUser(username2))){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 6:
                    //fitrar encomendas medicas
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasHistoricoInfo(username,this.logNegocio.filtrarEncomendaMedica())){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 7:
                    //encomendas entregue hoje
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasHistoricoInfo(username,this.logNegocio.filtrarEncomendaDia())){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 8:
                    //encomendas entregues esta semana
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasHistoricoInfo(username,this.logNegocio.filtrarEncomendaSemana())){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
                case 9:
                    //encomendas entregues este mes
                    encomendas = new StringBuilder("*** Encomendas por Entregar ***\n");
                    for(String ln: this.logNegocio.listOfEncomendasHistoricoInfo(username,this.logNegocio.filtrarEncomendaMes())){
                        encomendas.append("\n").append(ln).append("\n").append("*** ***").append("\n");
                    }
                    break;
            }
            //System.out.println(this.logNegocio.toString());
        } while (menuHistoricoEncomendas.getOpcao()!=0);
    }

    public void detalhesEncomenda(String username, String codigo) throws EncomendaInexistenteException, UserInexistenteException, IOException {
        Scanner scin = new Scanner(System.in);
        do {
            System.out.println(this.logNegocio.getEncomendaString(codigo));
            menuDetalhesEncomenda.executa();
            switch (menuDetalhesEncomenda.getOpcao()) {
                case 1:
                    //classificar transportador
                    System.out.println("Insira a classificação[1-5]: ");
                    int classit = Integer.parseInt(scin.nextLine());
                    if (classit < 6 && classit > 0) {
                        this.logNegocio.classifTransportador(username, codigo, classit);
                    } else {
                        System.out.println("Numero invalido");
                    }
                    this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
                    break;
                case 2:
                    //classificar loja
                    System.out.println("Insira o código da encomenda[1-5]: ");
                    int classil = Integer.parseInt(scin.nextLine());
                    if (classil < 6 && classil > 0) {
                    this.logNegocio.classifLoja(username, codigo, classil);
                    }
                    else{
                    System.out.println("Numero invalido");
                    }
                    this.logNegocio.guardaEstado("/home/simao/Desktop/Universidade/POO/ProjetoPOO1920/ProjetoPOO1920/estado.obj");
                    break;
            }
            //System.out.println(this.logNegocio.toString());
        } while (menuDetalhesEncomenda.getOpcao()!=0);
    }

    public void estatisticas(){
        do {
            menuEstatisticas.executa();
            switch (menuEstatisticas.getOpcao()){
                case 1:
                    System.out.println("Top 10 utilizadores por encomendas: ");
                    System.out.println(this.logNegocio.top10Utilizadores().toString());
                    break;
                case 2:
                    //top10 voluntarios
                    System.out.println("Top 10 Voluntarios com mais kms: ");
                    System.out.println(this.logNegocio.top10Voluntarios().toString());
                    break;
                case 3:
                    //top10 empresas
                    System.out.println("Top 10 empresas com mais kms: ");
                    System.out.println(this.logNegocio.top10Empresas().toString());
                    break;
            }
            //System.out.println(this.logNegocio.toString());
        } while (menuEstatisticas.getOpcao()!=0);
    }

}