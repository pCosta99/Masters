    import java.io.FileInputStream;
    import java.io.FileNotFoundException;
    import java.io.FileOutputStream;
    import java.io.IOException;
    import java.io.ObjectInputStream;
    import java.io.ObjectOutputStream;
    import java.io.Serializable;
    import java.time.LocalDate;
    import java.time.format.DateTimeFormatter;
    import java.util.ArrayList;
    import java.util.List;
    import java.util.Random;
    import java.util.regex.Matcher;
    import java.util.regex.Pattern;
    import java.util.Map;
    import java.util.HashMap;
    import java.util.Set;
    
    
    
    public class TrazAqui implements Serializable {
    
    private Dados          dados;
    private Utilizador     user;
    private Calculos       calculos;
    private LinhaEncomenda linha;
    private Viagem         viagem;
    private Voluntario     voluntarios;
    private Transportadora trans;
    private Loja           loja;
    private Cliente        cliente;
    private Admin          admin;
    private Menu           menu  = new Menu();
    private Input          input = new Input();
    private Parser         parser = new Parser();
    
    
    public TrazAqui() throws UserException, IOException, ClassNotFoundException,Exception {
    janela_inicial();
    }
    
    public static void main(String[] args) throws UserException, IOException, ClassNotFoundException,Exception {
    new TrazAqui();
    }
    
    public void janela_inicial() throws UserException, IOException, ClassNotFoundException,Exception {
    
        boolean flag = false;
        lerFicheiro();
        while (!flag) {
            menu.janela_inicial();
            int in = input.lerInt();
            switch (in) {
                case 1:
                    logIn();
                    break;
                case 2:
                    RegistarUser();
                    break;
                case 3:
                    menu.sair();
                    flag = true;
                    break;
            }
        }
        guardarFicheiro();
    }
    
    
    /* Se existir um ficheiro com utilizadores, abre-o */
    public void lerFicheiro() throws IOException,Exception {
        dados = new Dados();
        Utilizador user = new Admin();
        try {
            ObjectInputStream ois = new ObjectInputStream(new FileInputStream("saveData.ser"));
            try {
                dados = (Dados) ois.readObject();
                ois.close();
            } catch (ClassNotFoundException  |ClassCastException  |IOException e ) {
                dados=parser.parse();
                System.out.println(e.getMessage());
                dados.registarUser(user);
                guardarFicheiro();
            }
        } catch (IOException e) {
            System.out.println("erro ler ficheiro");
            dados=parser.parse();
            dados.registarUser(user);
            guardarFicheiro();
         
        }
        
         
    
    }
    
    public void logIn() throws IOException, ClassNotFoundException {
        
        String email = "";
        String password = "";
        
        System.out.print("\n\tEmail: ");
        email = input.lerString();
        while (!dados.containsUser(email)) {
            if (email.equals("0")) {
                return;
            }
            System.out.println("User nao registado, por favor faca o registo primeiro!");
            System.out.println("\n\tEmail: ");
            email = input.lerString();
        }
        Utilizador us = dados.getUser(email);
        System.out.print("\n\tPassword: ");
        password = input.lerString();
        
        while (!password.equals(us.getPassword())) {
            if (password.equals("0")) {
                return;
            }
            System.out.println("password errada, tente novamente!!");
            System.out.println("\tPassword: ");
            password = input.lerString();
        }
        
        if (us instanceof Voluntario) {
            voluntarios = (Voluntario) dados.getUser(email);
            menu.janelaPrincipalVoluntario();
            menuVoluntario();
        }
        if (us instanceof Loja) {
            loja = (Loja) dados.getUser(email);
            menu.janelaPrincipalLoja();
            menuLoja();
        }
        if (us instanceof Transportadora) {
            trans = (Transportadora) dados.getUser(email);
            menu.janelaPrincipalTransportadora();
            menuTransportadora();
        }
        if (us instanceof Cliente) {
            cliente = (Cliente) dados.getUser(email);
            menu.janelaPrincipalCliente();
            menuCliente();
        }
        if (us instanceof Admin){
            menu.janelaPrincipalAdmin();
            menuAdmin();
            
        }
    }
    
    
    public void menuVoluntario() {
    
        int opcao_inicial = -1;
        String codVoluntario = voluntarios.getCodVoluntario();
        opcao_inicial = input.lerInt();
        switch (opcao_inicial) {
            case 1:
                alterarDisponibilidadeV(codVoluntario);
                break;
            case 2:
                escolheEntregaV(codVoluntario);
                break;
            case 3:
                System.out.println();
                break;
            case 4:
                menu.sair();
                break;
        }
    }
    
    
    public void menuAdmin() {
    
        int opcao_inicial = -1;
        opcao_inicial = input.lerInt();
        switch (opcao_inicial) {
            case 1: // top 10 clientes por encomendas
                top10Cliente();
                break;
            case 2: top10Transportadoras();
                break;
            case 3: 
                menu.sair();
                break;
        }
    }
    
    
    public void menuLoja() {
    
        loja = dados.getLoja(loja.getCodLoja()).clone();
        int opcao_inicial = -1;
        
        opcao_inicial = input.lerInt();
        switch (opcao_inicial) {
            case 1:
                listagemArtigos();
                break;
            case 2:
                filadeEspera();
                break;
            case 3:
                validarEncomenda(loja);
                break;
            case 4:
                menu.sair();
                break;
        }
    }
    
    
    public void menuTransportadora() { // TODO: rever opcoes do menu a ser printed
    
        int opcao_inicial = -1;
        String codEmpresa = trans.getCodEmpresa();
        opcao_inicial = input.lerInt();
        switch (opcao_inicial) {
            case 1:
                alterarDisponibilidadeT(codEmpresa);
                break;
            case 2: // escolheEntrega();
                escolheEntregaT(codEmpresa);
                break;
            case 3: // total faturado num periodo de tempo
                faturadoEntreDatas(codEmpresa);
                break;
            case 4: 
                 verAvaliacao(codEmpresa);
                 break;
            case 5:
                menu.sair();
                break;
        }
    }
    
    
    public void menuCliente() {
        int opcao_inicial = -1;
        String codCliente = cliente.getCodCliente();
        opcao_inicial = input.lerInt();
        switch (opcao_inicial) {
            case 1:
                encomendar(codCliente);
                break;
            case 2:
                aceitaTransporte(codCliente);
                break;
            case 3: // consultar informacao de entrega entre datas
                consultaInfoEntreDatas(codCliente);
                break;
            case 4: 
                classificar(codCliente);
                break;
            case 5: // notificacoes
                notificacoes(codCliente);
                break;
            case 6:
                menu.sair();
                break;
        }
    }
    
    public void validarEncomenda(Loja loja) {
        try{
                Encomenda enc = dados.validarEncomendaLoja(loja.getCodLoja());
                if(enc != null){
                    dados.adicionaEncomendaCliente(enc);
                    dados.adicionarNotificacaoCliente(enc);
                }
            }
            catch(Exception e){
                System.out.println(e.getMessage());
            }
            menu.pressionaUmaTecla();
            menu.janelaPrincipalLoja();
            menuLoja();
    }
    
    
    public void notificacoes(String codCliente) {
        List<String> nots = dados.getCliente(codCliente).getNotificacoes();
        for (String s : nots) {
            System.out.println(s);
        }
        menu.limparNotificacoes();
        switch (input.lerString()) {
            case "s":
                dados.removerTodasNotificacoesCliente(codCliente);
                break;
            default:
                break;
        }
        menu.pressionaUmaTecla();
        menu.cls();
        menu.janelaPrincipalCliente();
        menuCliente();
    }
    
    
    
    public void guardarFicheiro() throws IOException {
    
        try {
            ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("saveData.ser"));
            oos.writeObject(dados);
            oos.flush();
            oos.close();
            System.out.println("\n\tFicheiro guardado com successo\n");
    
        } catch (IOException e) {
            System.out.println("\t**Erro ao guardar o ficheiro!**\n" + e);
        }
    }
    
    
    public int RegistarUser() {
    
        String nome, email = "", password = "";
        String codVoluntario = "", codEmpresa = "", codLoja = "", codUtilizador = "";
        double pos_x = 0, pos_y = 0, precoKm = 0;
        double precoKilo = 0, taxa = 0;
        int raio = 0, nif = 0;
        int funcao = -1;
    
        menu.janela_registo();
        System.out.println("\tNome: ");
        nome = input.lerString();
        System.out.println("\tPosicao X: ");
        pos_x = input.lerDouble();
        System.out.println("\tPosicao Y: ");
        pos_y = input.lerDouble();
        
        menu.escolhaFuncao();
        funcao = input.lerInt();
        System.out.println("Funcao: " + funcao);
    
        if (funcao == 1) { // Voluntario
            System.out.print("\tcodVoluntario (vX/ vXX): ");
            codVoluntario = input.lerString();
            while (!(codVoluntario.matches("[v]+[0-9]{1,2}")) || dados.containsUser(codVoluntario)) {
                System.out.println("\tInsira um codigo: ");
                codVoluntario = input.lerString();
            }
            email = password = codVoluntario;
            System.out.println("\tRaio: ");
            raio = input.lerInt();
    
            Voluntario novo_voluntario = new Voluntario(codVoluntario, nome, email, password, pos_x, pos_y, raio,0,0,  new ArrayList<>());
            dados.registarVoluntario(novo_voluntario);
            dados.registarUser(novo_voluntario);
            menu.registoComSucesso(novo_voluntario.getNome());
        }
    
        if (funcao == 2) { // Transportadora
            System.out.print("\tcodEmpresa(tX / tXX): ");
            codEmpresa = input.lerString();
            while (!(codEmpresa.matches("[t]+[0-9]{1,2}")) || dados.containsUser(codEmpresa)) {
                System.out.println("Insira um codigo ");
                codEmpresa = input.lerString();
            }
            email = password = codEmpresa;
            System.out.println("\tNIF: ");
            nif = input.lerInt();
            System.out.println("\tRaio: ");
            raio = input.lerInt();
            System.out.println("\tPreco por Km: ");
            precoKm = input.lerDouble();
            System.out.println("\tPreco por kg:");
            precoKilo = input.lerDouble();
            System.out.println("\tTaxa de transporte (0-100):");
            int taxaIn = input.lerInt();
            while(taxaIn > 100 || taxaIn < 0){
                System.out.println("Taxa invalida");
                taxaIn = input.lerInt();
            }
            taxa = taxaIn/100;
            System.out.println("\tDisponibilidade (sim/nao) : ");
            String x = " ";
            boolean b = false;
            do {
                x = input.lerString();
                Pattern valor = Pattern.compile("Sim|Nao", Pattern.CASE_INSENSITIVE);
                Matcher m = valor.matcher(x);
                b = m.matches();
            } while (b == false);
            Pattern valor = Pattern.compile("Sim", Pattern.CASE_INSENSITIVE);
            Matcher m = valor.matcher(x);
            boolean disp = m.matches();
              
    
            Transportadora nova_transportadora = new Transportadora(codEmpresa, nome, email, password, pos_x, pos_y, nif, raio,
                                                                    precoKm, precoKilo, taxa, disp, 0,0,new ArrayList<>());
            dados.registarTransportadora(nova_transportadora);
            dados.registarUser(nova_transportadora);
            menu.registoComSucesso(nova_transportadora.getNome());
        }
    
        if (funcao == 3) { // Loja
            System.out.println("\tCodLoja: (lX / lXX)");
            codLoja = input.lerString();
            while (!(codLoja.matches("[l]+[0-9]{1,2}")) || dados.containsUser(codLoja)) {
                System.out.println("Insira um codigo valido: ");
                codLoja = input.lerString();
            }
            email = password = codLoja;
    
            Loja nova_loja = new Loja(codLoja, nome, email, password, pos_x, pos_y, new HashMap<String,LinhaEncomenda>(), new ArrayList<Encomenda>(), new ArrayList<Encomenda>());
            dados.registarLoja(nova_loja);
            dados.registarUser(nova_loja);
            
            menu.registoComSucesso(nova_loja.getNome());
        }
        if (funcao == 4) { // Cliente
            System.out.print("\tcodUtilizador  (uX / uXX): ");
            codUtilizador = input.lerString();
            while (!(codUtilizador.matches("[u]+[0-9]{1,2}")) || dados.containsUser(codUtilizador)) {
                System.out.println("\tInsira um codigo ");
                codUtilizador = input.lerString();
            }
            email = password = codUtilizador;
           
            Cliente novo_utilizador = new Cliente(codUtilizador, nome, email, password, pos_x, pos_y, new ArrayList<>(), new ArrayList<>());
            dados.registarUtilizador(novo_utilizador);
            dados.registarUser(novo_utilizador);
            
            menu.registoComSucesso(novo_utilizador.getNome());
        }
        return 0;
    }
    
    public void top10Transportadoras(){
        List<Transportadora> transportadoras = dados.top10Transportadoras();
        menu.top10Transportadoras(transportadoras);
        menu.janelaPrincipalAdmin();
        menuAdmin();
    }
    
    public void top10Cliente(){
        List<ClienteViagem> cv = dados.top10Cliente();
        menu.top10Clientes(cv);
        menu.janelaPrincipalAdmin();
        menuAdmin();
    }
    
    public void listagemArtigos(){
        menu.listaArtigos(loja.getProdutos());
        menu.editarArtigos();
        int opcao_inicial = -1;
        opcao_inicial = input.lerInt();
        switch (opcao_inicial) {
            case 1:
                acrescentarArtigo();
                break;
            case 2:
                menu.janelaPrincipalLoja();
                menuLoja();
                break;
            default:
                break;
        }
        
    }
    
    public int acrescentarArtigo() {
        String codProduto, descricao = new String();
        int quantidade = 0;
        double valorUnitario = 0;
        System.out.println("\tCodigo Produto(pXX / pX) : ");
        codProduto = input.lerString();
        while (!(codProduto.matches("[p]+[0-9]{1,2}"))) {
            System.out.println("\tInsira um codigo valido: ");
            codProduto = input.lerString();
        }
    
        if (loja.getProdutos().containsKey(codProduto)) {
            LinhaEncomenda p = loja.getProduto(codProduto);
            System.out.println("\tDescricao do Produto: ");
            descricao = input.lerString();
            p.setDescricao(descricao);
            System.out.println("\tNeste momento tem " + p.getQuantidade() + " kg");
            System.out.println("\tQual a quantidade(kg) a acrescentar?");
            quantidade = input.lerInt();
            p.setQuantidade(quantidade + p.getQuantidade());
            System.out.println("\tValor por kg: ");
            valorUnitario = input.lerDouble();
            p.setValorUnitario(valorUnitario);
            loja.registarProduto(p);
            dados.atualizarLoja(loja);
        } else {
            System.out.println("\tDescricao do Produto: ");
            descricao = input.lerString();
            System.out.println("\tQuantidade(kg): ");
            quantidade = input.lerInt();
            System.out.println("\tValor por kg: ");
            valorUnitario = input.lerDouble();
    
            LinhaEncomenda p = new LinhaEncomenda(codProduto, descricao, quantidade, valorUnitario);
            
            loja.registarProduto(p);
            dados.atualizarLoja(loja);
        }
        menu.cls();
        menu.janelaPrincipalLoja();
        menuLoja();
        return 0;
    }
    
    
    public void filadeEspera() {
    
        int q = loja.quantidadeListaEspera();
        System.out.print("Pessoas em espera : " + q);
        menu.pressionaUmaTecla();
        menu.cls();
        menu.janelaPrincipalLoja();
        menuLoja();
    
    }
    
    
    public void alterarDisponibilidadeV(String codVoluntario) {
    
        String disp = "";
        Voluntario v = dados.getVoluntario(codVoluntario);
        int opcao = -1;
    
        while (opcao != 3) {
            System.out.print(v);
            if (v.getDisponivel() == false)
                disp = "Indisponivel";
            else
                disp = "Disponivel";
            System.out.println("\t\tDisponibilidade\n");
            System.out.println("\tAtual: " + disp + "\n");
            menu.disponibilidade();
    
            opcao = input.lerInt();
            switch (opcao) {
                case 1:
                    v.setDisponivel(true);
                    menu.cls();
                    break;
                case 2:
                    v.setDisponivel(false);
                    menu.cls();
                    break;
                case 3:
                    menu.janelaPrincipalVoluntario();
                    menuVoluntario();
                    break;
            }
        }
    
    }
    
    
    public void alterarDisponibilidadeT(String codEmpresa) {
    
        Transportadora e = dados.getTrans(codEmpresa);
        int opcao = -1;
    
        while (opcao != 3) {
    
            String disp;
            if (e.getDisponivel() == false)
                disp = "Indisponivel";
            else
                disp = "Disponivel";
            System.out.println("\t\tDisponibilidade\n");
            System.out.println("\tAtual: " + disp + "\n");
            menu.disponibilidade();
    
            opcao = input.lerInt();
            switch (opcao) {
                case 1:
                    e.setDisponivel(true);
                    menu.cls();
                    break;
                case 2:
                    e.setDisponivel(false);
                    menu.cls();
                    break;
                case 3:
                    menu.janelaPrincipalTransportadora();
                    menuTransportadora();
                    break;
            }
        }
    
    }
    
    
    public void encomendar(String codUtilizador) {
        try{
            double peso = 0;
            double pesoFinal = 0;
            String finalencomenda = "", codproduto = "", codUser = codUtilizador;
            ArrayList<LinhaEncomenda> encomendas = new ArrayList<>();
    
            Random num = new Random();
            int n = num.nextInt(999);
            String cod = "e" + n;
            
            menu.listaLojas(dados.getLojas());
            System.out.println("\tQual o codigo da loja onde quer comprar: ");
    
            String codLoja = input.lerString();
            
            while (!dados.containsLoja(codLoja)) {
                if (codLoja.equals("0")) {
                    return;
                }
                System.out.println("Loja inexistente, tente de novo");
                codLoja = input.lerString();
            }
            Loja loja = dados.getLoja(codLoja);
            menu.listaArtigos(loja.getProdutos());
            System.out.println("\t\t\tPODE FAZER UMA ENCOMENDA\n");
            while (!(finalencomenda.equals("2"))) {
                System.out.println("\tQual o codigo do produto? ");
                codproduto = input.lerString();
                
                while (!loja.containsProduto(codproduto)) {
                    if (codLoja.equals("0")) {
                        return;
                    }
                    System.out.println("Produto com o codigo errado\nTente novamente!");
                    codproduto = input.lerString();
                }
    
                System.out.print("\tQuantos kilos quer? ");
                peso = input.lerDouble();
                
                LinhaEncomenda p = loja.getProduto(codproduto);
                
                while (!(p.getQuantidade() >= peso)) {
                    System.out.println("Quantidade sem stock!\nO maximo que pode encomendar sao "
                                       + (p.getQuantidade()) + " kg");
                    System.out.println("\nQual a quantidade que quer encomendar:");
                    peso = input.lerDouble();
                }
                pesoFinal += peso;
                LinhaEncomenda le = new LinhaEncomenda(codproduto, p.getDescricao(), peso, p.getValorUnitario());
                encomendas.add(le);
    
                System.out.println("Deseja continuar a fazer encomendas?");
                System.out.println(Constantes.SIM);
                System.out.println(Constantes.NAO);
                finalencomenda = input.lerString();
            }
    
            Encomenda e = new Encomenda(cod, codUser, codLoja, pesoFinal, encomendas);
    
            System.out.println("encomenda :" + e.toString());
            
            dados.registarEncomenda(e);
    
            System.out.println("\n\t\t **Encomenda registada com sucesso**");
            menu.pressionaUmaTecla();
            menu.janelaPrincipalCliente();
            menuCliente();
        }
        catch(Exception e){
            System.out.println("Nao foi possivel realizar a sua encomenda:");
            System.out.println(e.getMessage());
            menu.pressionaUmaTecla();
            menu.janelaPrincipalCliente();
            menuCliente();
        }
    }
    
    
    
    
    
    
    public void escolheEntregaV(String codVoluntario){
        try{
            System.out.println("\n\t\tTem estas entregas diponiveis:");
            menu.listaCodEncomendas(dados.getEncomendasAceites());
            System.out.print("\nQual deseja efetuar: ");
            String opcao = input.lerString();
            while (!(dados.getEncomendasAceites().containsKey(opcao))) {
                System.out.println("Codigo invalido. Tente novamente");
                System.out.println("Codigo: ");
                opcao = input.lerString();
            }
            
            Voluntario v = dados.getVoluntario(codVoluntario);
            dados.escolheEncomenda(opcao, v);
        }
        catch(Exception e){
            System.out.println("Nao e possivel aceitar esta entrega");
            System.out.println(e.getMessage());
        }
        menu.pressionaUmaTecla();
        menu.janelaPrincipalVoluntario();
        menuVoluntario();
    }
    
    public void escolheEntregaT(String cod){
        try{
            Map<String, Encomenda> aceites = dados.getEncomendasAceites();
            if(aceites.isEmpty()){
                System.out.println("Não existem ecomendas disponiveis");
            }
            else{
                System.out.println("\n\t\tTem estas entregas diponiveis:");
                menu.listaCodEncomendas(aceites);
                System.out.println("\nQual deseja efetuar: ");
                String opcao = input.lerString();
                while (!(aceites.containsKey(opcao))) {
                    System.out.println("Codigo invalido. Tente novamente");
                    System.out.println("Codigo: ");
                    opcao = input.lerString();
                }
                
                Transportadora t = dados.getTrans(cod);
                dados.escolheEncomenda(opcao, t);
            }
        }
        catch(Exception e){
            System.out.println("Nao e possivel aceitar esta entrega");
            System.out.println(e.getMessage());
        }
        menu.pressionaUmaTecla();
        menu.janelaPrincipalTransportadora();
        menuTransportadora();
    }
    
    public void aceitaTransporte(String codCliente) {
        try{
            Set<String> codEncomendas = dados.getCodEncomendasProntasCliente(codCliente);
            Map<String, List<Viagem>> encomendasProntas = dados.getViagensEncomendas();
            menu.listaCodEncomendas(codEncomendas);
            if(!codEncomendas.isEmpty()){
                String codEncomenda = input.lerString();
                while (!encomendasProntas.containsKey(codEncomenda)) {
                    System.out.println("Codigo invalido. Tente novamente");
                    System.out.println("Codigo: ");
                    codEncomenda = input.lerString();
                }
                
                List<Viagem> viagensAceites = dados.getViagensEncomenda(codEncomenda);
                menu.escolhaViagens(viagensAceites);
                int opcao = input.lerInt();
                while (opcao <= 0 && opcao > viagensAceites.size()) {
                    System.out.println("Numero invalido. Tente novamente");
                    System.out.println("Codigo: ");
                    opcao = input.lerInt();
                }
                
                dados.realizaTransporte(opcao, codCliente, codEncomenda);
                System.out.println("O seu transporte foi realizado");
            }
            else {
                System.out.println("Não existem encomendas para aceitar");
            }
        }
        catch(Exception e){
            System.out.println("Nao foi possivel realizar o transporte");
            System.out.println(e.getMessage());
        }
        menu.pressionaUmaTecla();
        menu.janelaPrincipalCliente();
        menuCliente();
    }
    
    public void classificar(String codCliente) {
        int classificacao;
        List<Viagem> viagens = dados.getViagensCliente(codCliente);
        if(viagens.isEmpty()){
            System.out.println("Nao existem viagens");
        }
        else{
            try{
                menu.aceitatransporte(viagens);
                Map<String, Transportadora> t = dados.getTransportadoras();
                Map<String, Voluntario> v = dados.getVoluntarios();
                String codTransportadora = input.lerString();
                System.out.println(v.containsKey(codTransportadora));
                System.out.println(t.containsKey(codTransportadora));
                while(!(v.containsKey(codTransportadora) || t.containsKey(codTransportadora))){
                    System.out.println("Codigo errado!!");
                    codTransportadora = input.lerString();
                }
                System.out.println("Indique qual a classificacao (0-10)");
                classificacao = input.lerInt();
                while(classificacao<0 || classificacao>10){
                    System.out.println("Classificacao errada!!");
                    classificacao = input.lerInt();
                }
                dados.classificar(codTransportadora, classificacao);
                System.out.println("Obrigado por ter atribuido uma classificacao!! ");
            }
            catch( Exception e){
                System.out.println("Nao foi possivel classificar");
                System.out.println(e.getMessage());
            }
        }
    
        menu.pressionaUmaTecla();
        menu.janelaPrincipalCliente();
        menuCliente();
    }
            
    public void faturadoEntreDatas(String codTransportadora){
        
        try{
            System.out.println("Data Inicial" );
            LocalDate inicio = input.lerData();
            System.out.println("Data Final" );
            LocalDate fim = input.lerData();
            while(fim.compareTo(inicio)<=0){
                System.out.println("Intervalo de datas invalido");
                System.out.println("Data Inicial" );
                inicio = input.lerData();
                System.out.println("Data Final" );
                fim = input.lerData();
            }
            double total = dados.faturadoEntreDatas( inicio,  fim,codTransportadora);
           
            System.out.println("Total faturado: " + total);
        }
        catch(Exception e){
            System.out.println("Nao existe faturado entre datas");
            System.out.println(e.getMessage());
        }
        menu.pressionaUmaTecla();
        menu.janelaPrincipalTransportadora();
        menuTransportadora();
    }
    
    public void consultaInfoEntreDatas(String codCliente){
        
        try{
            System.out.println("Data Inicial" );
            LocalDate inicio = input.lerData();
            System.out.println("Data Final" );
            LocalDate fim = input.lerData();
            while(fim.compareTo(inicio)<=0){
                System.out.println("Intervalo de datas invalido");
                System.out.println("Data Inicial" );
                inicio = input.lerData();
                System.out.println("Data Final" );
                fim = input.lerData();
            }
            List<Viagem> viagem = dados.informacaoViagem(inicio, fim, codCliente);
            if(viagem.isEmpty()){
                System.out.println(Constantes.CLIENTE_NAO_TEM_VIAGENS);
            }
            else {
                menu.listaViagens(viagem);
            }
        }
        catch(Exception e){
            System.out.println("Nao existe informacao entre Datas");
            System.out.println(e.getMessage());
        }
        menu.pressionaUmaTecla();
        menu.janelaPrincipalCliente();
        menuCliente();
    }
    
    public void verAvaliacao(String codEmpresa){
        double avaliacao = dados.getAvaliacaoT(codEmpresa);
        menu.mostrarAvaliacao(avaliacao);
        menu.pressionaUmaTecla();
        menu.janelaPrincipalTransportadora();
        menuTransportadora();
    }
    
    
}
