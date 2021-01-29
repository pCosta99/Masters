import javax.annotation.processing.SupportedSourceVersion;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

public class TrazAquiApp implements Serializable {
    private GestaoAplicacao logApp;
    private Menu menuPrincipal, menuLogin, menuUser, menuCliente, menuLoja, menuEmpresa, menuEstafeta;

    public static void main(String[] args) {
        new TrazAquiApp().run();
    }

    private TrazAquiApp() {
        try {
            this.logApp = GestaoAplicacao.lerObj("estado.obj");
            System.out.println("Consegui Ler");
        } catch (FileNotFoundException e) {
            System.out.println("A preparar a aplicação para o primeiro uso!");
            this.logApp = new GestaoAplicacao();
        } catch (IOException e) {
            System.out.println("Oops! Erro de leitura!");
            this.logApp = new GestaoAplicacao();
        } catch (ClassNotFoundException e) {
            System.out.println("Oops! Fromato de ficheiro de dados errado!");
            this.logApp = new GestaoAplicacao();
        }
        Parser p  = new Parser();
        p.parse(logApp);
    }

    private void run() {
        List<String> opcoes = Arrays.asList("Login", "Registo");

        this.menuPrincipal = new Menu(opcoes);
        do {
            menuPrincipal.executa();
            switch (menuPrincipal.getOpcao()) {
                case 1:
                    System.out.println("Escolheu Login");
                    login();
                    //Something something Login
                    break;
                case 2:
                    System.out.println("Escolheu Registo");
                    registarUser();
                    break;
            }
        } while (menuPrincipal.getOpcao() != 0);

        try {
            this.logApp.gravarObj("estado.obj");
            System.out.println("Consegui gravar!");
        } catch (IOException e) {
            System.out.println("oops! Não consegui gravar os dados!");
        }
        System.out.println("Até breve!...");
    }

    private void login() {
            Scanner sc = new Scanner(System.in);
            //System.out.println("Escolheu Login");
            System.out.print("Username: ");
            String userName = sc.nextLine();
            System.out.print("Password: ");
            String password = sc.nextLine();
            if(userName.equals("ADMIN") && password.equals("ADMIN"))
                adminChoices();
            else {

                try {
                    if (this.logApp.login(userName, password)) {
                        System.out.println("Entrou com sucesso!");
                        userChoices(userName);
                    }

                } catch (UserInexistenteException e) {
                    System.out.println("Oops! Parece que o User não existe!");
                }
            }
        }

        private void adminChoices(){
            System.out.println("A aplicação tem " + logApp.getNumeroEntidade("User") + " usuários!");
            System.out.println("A aplicação tem " + logApp.getNumeroEntidade("Cliente") + " clientes!");
            System.out.println("A aplicação tem " + logApp.getNumeroEntidade("Estafeta") + " estafetas!");
            System.out.println("A aplicação tem " + logApp.getNumeroEntidade("Loja") + " lojas!");
            System.out.println("A aplicação tem " + logApp.getNumeroEntidade("Transportadora") + " transportadoras!");
            System.out.println("A aplicação tem " + logApp.getNumeroEntidade("Encomenda") + " encomendas!");
            System.out.println("5 Empresas que mais lucraram na TrazAqui: " + logApp.getXDistribuidorasProfit(5));
        }


    private void userChoices(String username){
        Scanner sc = new Scanner(System.in);
        menuUser = new Menu(Arrays.asList("Opções Cliente", "Opções Estafeta", "Opções Estabelecimento/s", "Opções Transportador"));
        do {
            menuUser.executa();
            switch (menuUser.getOpcao()) {

                case 0:
                    break;
                case 1:
                    System.out.println("Entrar em opções de Cliente");
                    List<String> listaC = logApp.getEntidade(username, "Cliente");
                    if (listaC != null) {
                        String cliente = listaC.get(0);
                        opcoesCliente(cliente);
                    }
                    else{
                        System.out.println("Parece que não tem qualquer Cliente associada a esta conta!\n Deseja criar um perfil de cliente?");
                        System.out.println("1\t-\tSIM \n0\t-\tSAIR");
                        int opcao = sc.nextInt();
                        if(opcao == 1) {
                            criaEntidade(username, "Cliente");
                        }
                    }
                    break;
                case 2:
                    System.out.println("Entrar em opções de Estafeta");
                    List<String> listaE = logApp.getEntidade(username, "Estafeta");
                    if(listaE != null) {
                        String estafeta = listaE.get(0);
                        opcoesEstafeta(estafeta);
                    }
                    else{
                        System.out.println("Parece que não tem qualquer Estafeta associada a esta conta!\n Deseja criar um perfil de estafeta?");
                        System.out.println("1\t-\tSIM \n0\t-\tSAIR");
                        int opcao = sc.nextInt();
                        if(opcao == 1)
                            criaEntidade(username, "Estafeta");
                    }
                    break;
                case 3:
                    System.out.println("Entrar em opções de Loja");
                    List<String> lojas = logApp.getEntidade(username, "Loja");
                    if(lojas != null) {
                        opcoesLoja(lojas);
                    }
                    else{
                        System.out.println("Parece que não tem qualquer Loja associada a esta conta!\n Deseja criar um perfil de loja?");
                        System.out.println("1\t-\tSIM \n0\t-\tSAIR");
                        int opcao = sc.nextInt();
                        if(opcao == 1)
                            criaEntidade(username,"Loja");;
                    }
                    break;
                case 4:
                    System.out.println("Entrar em opções de Transportador");
                    List<String> transportadoras = logApp.getEntidade(username, "Transportadora");
                    if(transportadoras != null) {
                        opcoesEmpresa(transportadoras);
                    }
                    else{
                        System.out.println("Parece que não tem qualquer Transportadora associada a esta conta!\n Deseja criar um perfil de Transportadora?");
                        System.out.println("1\t-\tSIM \n0\t-\tSAIR");
                        int opcao = sc.nextInt();
                        if(opcao == 1)
                            criaEntidade(username, "Transportadora");
                    }
                default:
                    userChoices(username);
                    break;
            }
        }while (menuUser.getOpcao() != 0);

    }

    private void registarUser(){
        Scanner sc = new Scanner(System.in);

        System.out.print("Username: ");
        String username = sc.nextLine();
        System.out.print("Email: ");
        String email = sc.nextLine();
        System.out.print("Password: ");
        String password = sc.nextLine();

        this.logApp.registarUtilizador(username, email, password);
        System.out.println("User: " + username + " | " +
                            "Email: " + email + " | " +
                            "Password: " + password + ';');
    }

    private void opcoesCliente(String codCliente) {
        Scanner sc = new Scanner(System.in);
        menuCliente = new Menu(Arrays.asList("Consultar Saldo", "Encomendar", "Histórico de Encomendas"));
        do {
            menuCliente.executa();
            switch (menuCliente.getOpcao()) {
                case 0:
                    break;
                case 1:
                    System.out.println("Consultar Saldo!");
                    System.out.println("Neste momento não é possível realizar a operação!");
                    //this.logApp.getSaldoUtilizador();
                    break;
                case 2: //LISTAGEM DE LOJAS PARA CONSULTA DE PRODUTOS E REALIZAÇÃO DE ENCOMENDAS
                    List<String> codigosLoja = this.logApp.lojasAbertas(); //codigos de lojas
                    List<String> opcoesLoja = this.logApp.nomesLojas(codigosLoja); //nomes de lojas
                    Menu umMenu = new Menu(opcoesLoja);
                    if (opcoesLoja.size() > 0) {
                        umMenu.executa();
                        int opcao = umMenu.getOpcao();
                        if (opcao > 0) {
                            List<String> produtos = verLoja(codigosLoja.get(opcao - 1), codCliente); //chamar função com certas descrições da loja reparar que loja pode estar fechada
                            if(produtos.size() > 0){
                            Menu menuProdutos = new Menu(produtos);
                            Map<String, Integer> produtosEnc = new HashMap<>();
                            do {
                                menuProdutos.executa();
                                int prodNum = menuProdutos.getOpcao() -1 ;
                                String produto = produtos.get(prodNum); //BUG tem aqui uma lista de Descricoes de produtos e nao codigos
                                int quantidade = 1; //para já é 1 (mudar mais tarde)

                                if(!produtosEnc.containsKey(produto))
                                    produtosEnc.put(produto, quantidade);

                            } while (menuProdutos.getOpcao() != 0);
                                if(produtosEnc.size()>0)
                                    logApp.criaEncomenda(codCliente, (codigosLoja.get(opcao - 1)), produtosEnc);
                            }
                        }
                    }else {
                            System.out.println("Não existem lojas por perto");
                        }
                        break;
                        case 3:
                            System.out.println("Histórico de Encomendas!");
                            Menu tamanhoEncomendas = new Menu(Arrays.asList("Todas as Encomendas", "Apartir de um determinado periodo de tempo"));
                            tamanhoEncomendas.executa();
                            if (tamanhoEncomendas.getOpcao() == 1) {
                                List<String> opcoesEncomendas = this.logApp.getHistorico(codCliente, "Cliente");
                                Menu menuEncomendas = new Menu(opcoesEncomendas);
                                if (opcoesEncomendas.size() > 0)
                                    menuEncomendas.executa();
                                else
                                    System.out.println("Não existem encomendas!");
                            } else {
                                System.out.println("Insira o ano: ");
                                int ano = sc.nextInt(); //pedir ano ao user
                                System.out.println("Insira o mês:");
                                int mes = sc.nextInt(); //pedir mes ao user
                                List<String> encomendas = this.logApp.historicoEncomendasPeriodo(codCliente, ano, mes);
                                Menu menuEncomendas = new Menu(encomendas);
                                if (encomendas.size() > 0)
                                    menuEncomendas.executa();
                                else
                                    System.out.println("Não existem encomendas neste periodo de tempo!");
                            }
                            break;
                    }

        } while (menuCliente.getOpcao() != 0);
    }

    private void opcoesEstafeta(String codEstafeta) {
        menuEstafeta = new Menu(Arrays.asList("Procurar Pedidos de Encomendas", "Histórico de encomendas", "Consultar Classificacao"));
        menuEstafeta.executa();
        switch (menuEstafeta.getOpcao()){
            case 0:
                break;
            case 1:
                System.out.println("Pedidos de Encomendas por perto!");
                List<String> encomendas = this.logApp.getPertoEstafeta(codEstafeta, "Encomenda");
                if(encomendas != null) {
                    Menu menuEncomendas = new Menu(encomendas);
                    do {
                        menuEncomendas.executa();
                        if(menuEncomendas.getOpcao() != 0){
                            String encomenda = encomendas.get(menuEncomendas.getOpcao() - 1);
                            Menu menuOpcoesEncomenda = new Menu(Arrays.asList("Informações da Encomenda", "Aceitar Encomenda"));
                            do{
                                menuOpcoesEncomenda.executa();
                                switch (menuOpcoesEncomenda.getOpcao()){
                                    case 1:
                                        String info = logApp.getInfo(encomenda, "Encomenda");
                                        System.out.println(info);
                                        break;
                                    case 2:
                                        System.out.println(logApp.alterarEstadoEncomenda(codEstafeta, encomenda, "Estafeta"));
                                        break;
                                    default:
                                        break;
                                }
                            }while (menuOpcoesEncomenda.getOpcao() != 0);
                        }

                    }
                    while(menuEncomendas.getOpcao() != 0);
                }
                //procurar encomendas dentro do raio do estafeta
                break;
            case 2:
                System.out.println("Histórico de Encomendas feitas");
                //imprimir o histórico de encomendas feitas
                break;
            case 3:
                System.out.println("Classificação!");
                //getClassificacao() ou atualizaClassificacao();
                break;
            default:
                opcoesEstafeta(codEstafeta);
                break;
            }
        }

    private void opcoesLoja(List<String> codLojas){
        Menu menuLojas = new Menu(codLojas);
        do{
            menuLojas.executa();
            if(menuLojas.getOpcao() != 0){
                String loja = codLojas.get(menuLojas.getOpcao() - 1);

                menuLoja = new Menu(Arrays.asList("Informações da Loja", "Gerir Pedidos", "Gerir Inventario", "Histórico de Encomendas"));
                do {
                    menuLoja.executa();
                    switch (menuLoja.getOpcao()){
                        case 0:
                            break;
                        case 1:
                            System.out.println("Informações da Loja!");
                            System.out.println(logApp.getInfo(loja, "Loja"));
                            break;
                        case 2:
                            System.out.println("Gestão de Pedidos!");
                            List<String> pedidos = logApp.getEncomendas(loja, "Loja", 1); //não esquecer que nesta altura o estado já é 1!
                            Menu menuPedidos = new Menu(pedidos);
                            do {
                                menuPedidos.executa();
                                if(menuPedidos.getOpcao() != 0){
                                    String pedido = pedidos.get(menuPedidos.getOpcao() - 1);
                                    Menu menuAuxiliar = new Menu(Arrays.asList("Informações do Pedido!","Despachar Pedido!"));
                                    do{
                                        menuAuxiliar.executa();
                                        switch (menuAuxiliar.getOpcao()) {
                                            case 0:
                                                break;
                                            case 1:
                                                System.out.println(logApp.getInfo(pedido, "Encomenda"));
                                                break;
                                            case 2:
                                                System.out.println(logApp.alterarEstadoEncomenda(loja, pedido, "Loja"));
                                                break;
                                            default:
                                                break;
                                        }
                                    }while(menuAuxiliar.getOpcao() != 0);
                                }
                            }while(menuPedidos.getOpcao() != 0);
                            break;
                        case 3:
                            System.out.println("Hitórico de Encomendas!");
                            List<String> historico = logApp.getHistorico(loja, "Loja");
                            int i = 0;
                            for(String s : historico){
                                System.out.println(i + "\t-\t" + s);
                                i++;
                            }
                            break;
                        default:
                            opcoesLoja(codLojas);
                            break;
                    }
                }while(menuLoja.getOpcao() != 0);

            }
        }while(menuLojas.getOpcao() != 0);

    }

    private void opcoesEmpresa(List<String> codTransportadoras) {
        Menu menuTransportadoras = new Menu(codTransportadoras);
        do {
            menuTransportadoras.executa();
            if(menuTransportadoras.getOpcao() != 0){
                String transportadora = codTransportadoras.get(menuTransportadoras.getOpcao() - 1);

                Menu menuTransportadora = new Menu(Arrays.asList("Informações da Transportadora!", "Gerir Funcionários!", "Histórico de Encomendas!"));
                do{
                    menuTransportadora.executa();
                    switch (menuTransportadora.getOpcao()){
                        case 0:
                            break;
                        case 1:
                            System.out.println("Informações da Transportadora!");
                            String info = logApp.getInfo(transportadora, "Transportadora");
                            break;
                        case 2:
                            Menu opcoesFuncionarios = new Menu(Arrays.asList("Listagem de Funcionários!", "Funcionários capazes de transportar encomendas médicas!"));
                            Menu menuFuncionarios = new Menu(logApp.getFuncionarios(transportadora));
                            do{
                                menuFuncionarios.executa();
                                switch (menuFuncionarios.getOpcao()){
                                    case 0:
                                        break;
                                    case 1:
                                        System.out.println("Escolha o tipo de listagem!");
                                        Menu menuListagens = new Menu(Arrays.asList("Listagem Alfabética",
                                                                                    "Listagem por Reputação",
                                                                                    "Listagem por Distância",
                                                                                    "Listagem por Tempo",
                                                                                    "Listagem Normal (do trabalhador mais antigo para o mais recente)"));
                                        int i; //acumulador para ciclos
                                        do{
                                            menuListagens.executa();
                                            switch (menuListagens.getOpcao()){
                                                case 0:
                                                    break;
                                                case 1:
                                                    List <String> alfa = logApp.getFuncionarios(transportadora, "Por nome");
                                                    i = 0;
                                                    for(String s : alfa){
                                                        System.out.println(i + "\t-\t" + s);
                                                        i++;
                                                    }
                                                    break;
                                                case 2:
                                                    List <String> rep = logApp.getFuncionarios(transportadora, "Por Reputacao");
                                                    i = 0;
                                                    for(String s : rep){
                                                        System.out.println(i + "\t-\t" + s);
                                                        i++;
                                                    }
                                                    break;
                                                case 3:
                                                    List <String> dist = logApp.getFuncionarios(transportadora, "Por Distância");
                                                    i = 0;
                                                    for(String s : dist){
                                                        System.out.println(i + "\t-\t" + s);
                                                        i++;
                                                    }
                                                case 4:
                                                    List <String> temp = logApp.getFuncionarios(transportadora, "Por Tempo");
                                                    i = 0;
                                                    for(String s : temp){
                                                        System.out.println(i + "\t-\t" + s);
                                                        i++;
                                                    }
                                                    break;
                                                case 5:
                                                    List <String> norm = logApp.getFuncionarios(transportadora, "Por mais antigo");
                                                    i = 0;
                                                    for(String s : norm){
                                                        System.out.println(i + "\t-\t" + s);
                                                        i++;
                                                    }
                                                    break;
                                                default:
                                                    break;
                                            }
                                        }while(menuListagens.getOpcao() != 0);
                                        break;
                                    case 2:
                                        List<String> meds = logApp.getFuncionariosMed(transportadora); //1 -> encomenda médica
                                        i = 0;
                                        for(String s : meds){
                                            System.out.println(i + "\t-\t" + s);
                                            i++;
                                        }
                                }
                            }while (menuFuncionarios.getOpcao() != 0);
                    }
                }while(menuTransportadora.getOpcao() != 0);
            }
        }while (menuTransportadoras.getOpcao() != 0);
    }

    private List<String> verLoja(String codigoLoja, String codigoCliente){
        return logApp.getProdutos(codigoLoja);
    }

    private List<String> getFuncionarios(List<String> codigoFuncionarios){
        List<String> nomes = new ArrayList<>();
        for(String cod : codigoFuncionarios){
            List<String> aux= logApp.getEntidade(cod, "Estafeta");
            if(aux.size() > 0)
                nomes.add(aux.get(0));
        }
        return nomes;
    }

    private void criaEntidade(String username, String entidade){
        Scanner sc = new Scanner(System.in);
        switch (entidade){
            case "Cliente": // não esquecer de controlar o input
                System.out.println("Criação de Perfil de Clinete\nNome: ");
                String nomeC = sc.nextLine();
                System.out.println("Localização (2 inteiros): ");
                double latC = sc.nextDouble();
                double lonC = sc.nextDouble();
                logApp.criaCliente(username, nomeC, latC, lonC);
                break;
            case "Estafeta":
                System.out.println("Criação de Perfil de Estafeta\nNome: ");
                String nomeE = sc.nextLine();
                System.out.println("Localização (2 inteiros): ");
                double latE = sc.nextDouble();
                double lonE = sc.nextDouble();
                System.out.println("Raio de ação para encomendas:");
                double raioE = sc.nextDouble();
                int cert = -1;
                while(cert != 0 && cert != 1){ //enquanto opção não for Sim ou Não
                    System.out.println("Tem certificado para transportar encomendas médicas?\n Prima 1 para SIM, 0 para Nao");
                    cert = sc.nextInt();
                }
                logApp.criaEstafeta(username, nomeE, latE, lonE, raioE, cert == 1);
                break;
            case "Loja":
                System.out.println("Criação de Perfil de Estafeta\nNome: ");
                String nomeL = sc.nextLine();
                System.out.println("Localização (2 inteiros): ");
                double latL = sc.nextDouble();
                double lonL = sc.nextDouble();
                logApp.criaLoja(username, nomeL, latL, lonL);
                break;
            case "Transportadora":
                System.out.println("Criação de Perfil de Estafeta\nNome: ");
                String nomeT = sc.nextLine();
                System.out.println("Nif da Entidade: ");
                String nifT = sc.nextLine();
                System.out.println("Localização (2 inteiros): ");
                double latT = sc.nextDouble();
                double lonT = sc.nextDouble();
                System.out.println("Raio de ação para encomendas:");
                double raioT = sc.nextDouble();
                System.out.println("Taxa por quilómetro (€): ");
                float taxaT = sc.nextFloat();
                logApp.criaTransportadora(username, nomeT, latT, lonT, nifT, raioT, taxaT);
                break;
        }

    }
}

