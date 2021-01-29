package View;

import  Modelo.*;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class TrazAquiView {

    public void opcao(){
        System.out.println();
        System.out.println("Opção inválida\n");
    }

    public void inicio(){
        System.out.println("Utlima atualização do programa carregada.");
        System.out.println();
        System.out.println("Deseja fazer reset? [S/N]?");

    }

    public void menuInicial(){
        System.out.println("------------------------ T R A Z   A Q U I   !!! ------------------------");
        System.out.println("1 - Iniciar sessao");
        System.out.println("2 - Criar registo");
        System.out.println("3 - Registos de encomendas");
        System.out.println("4 - Sair");

    }

    /* ------------------------------------ I N F O R M A C O E S  ------------------------------------*/

    public void registoMenu(){
        System.out.println();
        System.out.println("------------------------ I N F O R M A C O E S ------------------------");
        System.out.println("1 - 10 utilizadores que mais utilizam o sistema");
        System.out.println("2 - 10 empresas  que mais utilizam o sistema");
        System.out.println("3 - Sair");
        System.out.print("Escolha a opcao: ");
        System.out.println();
    }

    public void utilizadores(List<Utilizador> util){
        int i = 1;
        for(Utilizador s: util){
            System.out.println(i + ": " + s.getCodUti() + " -> " + s.getnEncomendas());
            i++;
        }
    }

    public void transporta(List<Transportadora> tran){
        int i = 1;
        for(Transportadora t: tran){
            System.out.println(i + ": " + t.getCodEmpr()  + " -> "  + t.getnKmsfeitos());
            i++;
        }
    }

    /* ------------------------------------ I N I C I A R    S E S S Ã O ------------------------------------*/

    public void inicioSessao(){
        System.out.println();
        System.out.println("-------------- I N I C I O   D E   S E S S A O -------------- ");
        System.out.println("U - Utilizador");
        System.out.println("V - Voluntario");
        System.out.println("T - Transportadora");
        System.out.println("L - Loja");
        System.out.println("S - Sair");
        System.out.print("Escolha a opcao: ");
    }

    public String sessaoUti(){
        Scanner sc = new Scanner(System.in);
        System.out.print("Codigo: ");
        String codigoU = sc.next();
        System.out.println();
        return codigoU;
    }

    public String sessaoTrans(){
        Scanner sc = new Scanner(System.in);
        System.out.print("Codigo: ");
        String codigoT = sc.next();
        System.out.println();
        return codigoT;
    }

    public String sessaoLoja(){
        Scanner sc = new Scanner(System.in);
        System.out.print("Codigo: ");
        String codigoL = sc.next();
        System.out.println();
        return codigoL;
    }

    public String sessaoVol(){
        Scanner sc = new Scanner(System.in);
        System.out.print("Codigo: ");
        String codigoV = sc.next();
        System.out.println();
        return codigoV;
    }

    public void errado(){
        System.out.println("Codigo errado ou inexistente. \nPor favor crie uma conta ou tente novamente!");
        System.out.println();
    }

    public void bemvindo(){
        System.out.println();
        System.out.println("            B E M   V I N D O  ");
        System.out.println();
    }

    /* ------------------------------------ C R I A R   S E S S Ã O ------------------------------------*/

    public void criarSessao(){
        System.out.println("--------------  C R I A R   S E S S A O -------------- ");
        System.out.println("U - Utilizador");
        System.out.println("V - Voluntario");
        System.out.println("T - Transportadora");
        System.out.println("L - Loja");
        System.out.println("S - Sair");
        System.out.print("Escolha a opcao: ");
    }

    public void criadaSucesso(){
        System.out.println("Sessao criada com  sucesso");
        System.out.println();
    }

    public Utilizador criarUtili(TrazAquiModel p){
        Scanner sc = new Scanner(System.in);
        System.out.print("Codigo: ");
        String cod = sc.next();
        if (p.getUtilizador().containsKey(cod)) {
            System.out.println("Utilizador já existente.\nPor favor, inicie sessao!");
            return null;
        }
        else {
            System.out.print("Nome: ");
            String primeiro = sc.next();
            System.out.print("Apelido: ");
            String apelido = sc.next();
            String nome = primeiro + apelido;
            System.out.print("Coordenada x: ");
            double x = sc.nextDouble();
            System.out.print("Coordenada y: ");
            double y = sc.nextDouble();
            Coordenadas c = new Coordenadas(x, y);
            List<Encomenda> enc = new ArrayList<>();
            List<String> tran  = new ArrayList<>();
            return new Utilizador(cod, nome, c,0,enc,tran);
        }
    }

    public Voluntario criarVoluntario(TrazAquiModel p){
        Scanner sc = new Scanner(System.in);
        System.out.print("Codigo: ");
        String codV = sc.next();
        if(p.getVoluntario().containsKey(codV)){
            System.out.println("Voluntario já existente.\nPor favor, inicie sessao!");
            return null;
        }
        else {
            System.out.print("Nome: ");
            String primeiro = sc.next();
            System.out.println("Apelido: ");
            String apelido = sc.next();
            String nome = primeiro + apelido;
            System.out.print("Coordenada x: ");
            double x = sc.nextDouble();
            sc.nextLine();
            System.out.print("Coordenada y: ");
            double y = sc.nextDouble();
            sc.nextLine();
            Coordenadas c = new Coordenadas(x, y);
            System.out.print("Raio: ");
            double raio = sc.nextDouble();
            System.out.println("Tem certificado de transporte de medicamentos?[S/N]");
            char medsLetra = sc.next().charAt(0);
            boolean meds = false;
            if(medsLetra == 'S' || medsLetra == 's')
                meds =  true;
            List<Integer> cla = new ArrayList<>();
            Map<Encomenda,LocalDateTime> en = new HashMap<>();
            return new Voluntario(codV, nome, c, raio, true, meds, cla, en);
        }
    }

    public Transportadora criaTransportadora(TrazAquiModel p){
        Scanner sc = new Scanner(System.in);
        System.out.print("Codigo: ");
        String codT = sc.next();
        if (p.getTransportador().containsKey(codT)) {
            System.out.println("Transportadora já existente.\nPor favor, inicie sessao!");
            return null;
        } else {
            System.out.print("Nome: ");
            String nome = sc.next();
            System.out.print("NIF: ");
            int nif = sc.nextInt();
            System.out.print("Coordenada x: ");
            double x = sc.nextDouble();
            System.out.print("Coordenada y: ");
            double y = sc.nextDouble();
            Coordenadas c = new Coordenadas(x, y);
            System.out.print("Raio: ");
            double raio = sc.nextDouble();
            System.out.print("Preco por km: ");
            double preco_km = sc.nextDouble();
            System.out.print("Preco por peso: ");
            double preco_peso = sc.nextDouble();
            System.out.print("Velocidade media: ");
            double velocidade = sc.nextDouble();
            Map<Encomenda, LocalDateTime> enc = new HashMap<>();
            System.out.print("Faz multiplas encomendas?[S/N]");
            char escolha = sc.next().charAt(0);
            System.out.println("Tem certificado de transporte de medicamentos?[S/N]");
            char medsLetra = sc.next().charAt(0);
            boolean meds = false;
            if(medsLetra == 'S' || medsLetra == 's')
                meds = true;
            List<Integer> clas = new ArrayList<>();
            if (escolha == 'S' || escolha == 's') {
                System.out.print("Capacidade de transporte:");
                int nMulti = sc.nextInt();
                List<Encomenda> pen = new ArrayList<>();
                return new TransportadoraMulti(codT, nome, nif, true, meds, c, raio, preco_km, preco_peso, velocidade,0,clas, enc, nMulti,pen);
            } else {
                return new TransportadoraNormal(codT, nome, nif, true, meds, c, raio, preco_km, preco_peso, velocidade,0,clas, enc);
            }
        }
    }

    public Loja criaLoja(TrazAquiModel p){
        Scanner sc = new Scanner(System.in);
        System.out.print("Codigo: ");
        String codL = sc.next();
        if (p.getLoja().containsKey(codL)) {
            System.out.println("Loja já existente.\nPor favor, inicie sessao!");
            return null;
        }
        else {
            System.out.print("Nome: ");
            String nome = sc.next();
            System.out.print("Tempo medio de atendimento: ");
            double tempoAtend = sc.nextDouble();
            System.out.print("Coordenada x: ");
            double x = sc.nextDouble();
            System.out.print("Coordenada y: ");
            double y = sc.nextDouble();
            Coordenadas c = new Coordenadas(x, y);
            List<Encomenda> enc = new ArrayList<>();
            System.out.print("Tem informacao de fila?[S/N]");
            char escolha = sc.next().charAt(0);
            Map<String, LinhaEncomenda> catalogo = new HashMap<>();
            List<Encomenda> pend = new ArrayList<>();
            if (escolha == 'S' || escolha == 's') {
                System.out.print("Tempo medio da fila: ");
                double tempoFila = sc.nextDouble();
                return new Loja(codL, nome, tempoAtend, c, enc, tempoFila, true, pend, catalogo);
            } else {
                double tempoFila = -1;
                return new Loja(codL, nome, tempoAtend, c, enc, tempoFila, false, pend, catalogo);
            }
        }
    }

    /* ------------------------------------ U T I L I Z A D O R ------------------------------------*/

    public void menuGerarEncomenda(){
        System.out.println("------- C A R R I N H O   D E   C O M P R A S -------");
        System.out.println("1 - Gerar encomenda");
        System.out.println("2 - Finalizar encomendas pendentes");
        System.out.println("3 - Lista de produtos");
        System.out.println("4 - Area de cliente");
        System.out.println("5 - Sair");
        System.out.print("Escolha a opcao:");
        System.out.println();
    }

    public void areaCliente(){
        System.out.println("------- A R E A   D E   C L I E N T E -------");
        System.out.println("1 - Perfil;");
        System.out.println("2 - Editar Perfil");
        System.out.println("3 - Meus Pedidos;");
        System.out.println("4 - Sair;");
    }

    public void perfil(Utilizador u){
        System.out.println("Codigo: " +u.getCodUti());
        System.out.println("Nome: " + u.getNome());
        System.out.println("Localizacao: (" + u.getGps().getLatitude() + ", " + u.getGps().getLongitude() + ").");
    }

    public void editarPerfil(Utilizador u){
        System.out.println("Pretende alterar o que?");
        System.out.println("1 - Codigo\n2 - Nome\n3 - Localizacao");
        Scanner s = new Scanner(System.in);
        int escolha = s.nextInt();
        if(escolha == 1){
            System.out.println("Inserir novo codigo:");
            String cod = s.next();
            u.setCodUti(cod);
        }
        if(escolha == 2){
            System.out.println("Inserir primeiro nome:");
            String nom = s.next();
            System.out.println("Inserir apelido:");
            String seg = s.next();
            String nome = nom + seg;
            u.setNome(nome);
        }
        if(escolha == 3){
            System.out.println("Inserir coordenada X:");
            double x = s.nextDouble();
            System.out.println("Inserir coordenada Y:");
            double y = s.nextDouble();
            Coordenadas c = new Coordenadas(x,y);
            u.setGps(c);
        }
    }

    public boolean isLevantar(){
        System.out.println("Levantar na loja?[S/N]");
        Scanner sc = new Scanner(System.in);
        char letra = sc.next().charAt(0);
        return letra == 's' || letra == 'S';
    }

    public void pedidos(){
        System.out.println("------- T R A N S P O R T E -------");
        System.out.println("1 -  Transportadora");
        System.out.println("2 -  Voluntario");
    }

    public void encomendasPendentes(Utilizador u){
        int num = u.getPendentes().size();
        System.out.println();
        System.out.println("Tem " + num  + " encomendas prontas.");
        System.out.println();
    }

    public void encomendaMedica(){
        System.out.println();
        System.out.println("1 - Encomenda Medica;");
        System.out.println("2 - Encomenda Normal.");
        System.out.println();
    }

    public void encomendaVaziaTrans(){
        System.out.println();
        System.out.println("Ainda não realizou encomendas com transportadoras");
        System.out.println();
    }

    public void encomendaVaziaVol(){
        System.out.println();
        System.out.println("Ainda não realizou encomendas com voluntarios");
        System.out.println();
    }

    public void listaEncomendasTransp(Transportadora t, List<Encomenda> enc){
        System.out.println("Registo de encomendas da transportadora " + t.getCodEmpr()  + "*");
        for(Encomenda e: enc){
            System.out.println("Encomenda: " + e.getCodEncomenda());
            System.out.print("\t\t\t Loja: ");
            System.out.println(e.getCodLoja());
            System.out.print("\t\t\t Peso: ");
            System.out.println(e.getPeso());
            System.out.println("\t\t\t Descricao:");
            for(LinhaEncomenda l: e.getE()) {
                System.out.print("\t\t\t\t\t\t Produto: ");
                System.out.println(l.getCodProduto());
                System.out.print("\t\t\t\t\t\t Nome: ");
                System.out.println(l.getDescricao());
                System.out.print("\t\t\t\t\t\t Peso [Kg/U]: ");
                System.out.println(l.getPesoU());
                System.out.print("\t\t\t\t\t\t Preco [€/U]: ");
                System.out.println(l.getPrecoU());
            }
        }
        System.out.println("*Lista ordenada da mais recente para a mais antiga.");
    }

    public void listaEncomendasVol(Voluntario v, List<Encomenda> enc){
        System.out.println("Registo de encomendas do voluntario " + v.getCodVolu()  + "*");
        for(Encomenda e: enc){
            System.out.println("Encomenda: " + e.getCodEncomenda());
            System.out.print("\t\t\t Loja: ");
            System.out.println(e.getCodLoja());
            System.out.print("\t\t\t Peso: ");
            System.out.println(e.getPeso());
            System.out.println("\t\t\t Descricao:");
            for(LinhaEncomenda l: e.getE()) {
                System.out.print("\t\t\t\t\t\t Produto: ");
                System.out.println(l.getCodProduto());
                System.out.print("\t\t\t\t\t\t Nome: ");
                System.out.println(l.getDescricao());
                System.out.print("\t\t\t\t\t\t Peso [Kg/U]: ");
                System.out.println(l.getPesoU());
                System.out.print("\t\t\t\t\t\t Preco [€/U]: ");
                System.out.println(l.getPrecoU());
            }
        }
        System.out.println("*Lista ordenada da mais recente para a mais antiga.");
    }

    public void encomendaProcesso(){
        System.out.println();
        System.out.println("Encomenda gerada com sucesso.\nComecamos ja a preparar a sua encomenda.");
    }
    public void encomendaSucesso(){
        System.out.println();
        System.out.println("Encomenda efetuada com sucesso.");
        System.out.println();
    }

    public void encomendaCancelada(){
        System.out.println();
        System.out.println("Encomenda cancelada.");
        System.out.println("\n");
    }

    public void encomendasEntregues(Utilizador u){
        if(u.getTransportadoraEntregue().size() > 0){
            int i = 1;
            System.out.println("Tem " + u.getTransportadoraEntregue().size() + " encomendas entregues.");
            for(String e: u.getTransportadoraEntregue()){
                System.out.println(i + " -> " + e);
                i++;
            }
        }
    }

    public void encomendaCaminho(Voluntario v){
        System.out.println();
        System.out.println("Encomenda a caminho.\nTransportada pelo voluntario " + v.getCodVolu());
        System.out.println();
    }

    public void encomendaPendente(){
        System.out.println();
        System.out.println("Encomenda inexistente.");
        System.out.println();
    }

    public String imprimePendentes(Utilizador u){
        List<Encomenda> pend = u.getPendentes();
        if(pend.size() > 0) {
            for (Encomenda e : pend) {
                System.out.println("Encomenda: " + e.getCodEncomenda());
                System.out.print("\t\t\t  Loja: ");
                System.out.println(e.getCodLoja());
                System.out.print("\t\t\t Peso: ");
                System.out.println(e.getPeso());
                System.out.println("\t\t\t Descricao:");
                for (LinhaEncomenda l : e.getE()) {
                    System.out.print("\t\t\t\t\t\t Produto: ");
                    System.out.println(l.getCodProduto());
                    System.out.print("\t\t\t\t\t\t Nome: ");
                    System.out.println(l.getDescricao());
                    System.out.print("\t\t\t\t\t\t Peso [Kg/U]: ");
                    System.out.println(l.getPesoU());
                    System.out.print("\t\t\t\t\t\t Preco [€/U]: ");
                    System.out.println(l.getPrecoU());
                }
            }
            System.out.print("Insira o codigo da encomenda: ");
            Scanner sc = new Scanner(System.in);
            String codEncomenda = sc.next();
            System.out.println();
            return codEncomenda;
        }
        else {
            System.out.println("Não tem encomendas pendentes.");
            return null;
        }
    }


    /* ------------------------------------ L O J A  ------------------------------------*/

    public void produtosLoja(Map<String,Loja> loja) {
        System.out.println("---------- L O J A S ----------");
        for (Loja l : loja.values()) {
            System.out.println(l.getCodLoja() + " - " + l.getNomeLoja());
        }
        System.out.print("Insira o codigo da loja: ");
        System.out.println();
    }

    public void encomendasPendentes(Loja l){
        int num = l.getPendentes().size();
        System.out.println("Tem " + num  + " encomendas pendentes.");
        System.out.println();
    }

    public void catalogoProdutos(Map<String,LinhaEncomenda> loja) {
        if (loja.size() == 0){
            System.out.println();
            System.out.println("Loja sem catalogo disponivel.");
        }
        else {
            for (LinhaEncomenda aux : loja.values()) {
                System.out.println(aux.getCodProduto() + " - " + aux.getDescricao());
            }
        }
        System.out.println();
        System.out.println();
    }

    public List<LinhaEncomenda> listaProdutosEscolhida(Map<String, LinhaEncomenda> cat){
        List<LinhaEncomenda> carrinhoCompras = new ArrayList<>();
        Scanner sc = new Scanner(System.in);
        char escolha;
        do{
            System.out.println("[P] - Selecionar produto; [S] - Sair;");
            String var = sc.next().toUpperCase();
            escolha = var.charAt(0);
            if (escolha == 'P') {
                Scanner s = new Scanner(System.in);
                System.out.println("Insira o codigo do produto");
                String codP = s.next();
                if (cat.containsKey(codP)) {
                    LinhaEncomenda linha = cat.get(codP);
                    carrinhoCompras.add(linha);
                }
            }
        }while(escolha!='S');
        return carrinhoCompras;
    }

    public void listaVazia(){
        System.out.println("Loja sem catalogo!\nSelecione outra loja");
        System.out.println();
    }

    public void inserirCatalogo(Loja atual, TrazAquiModel p){
        Scanner sc = new Scanner(System.in);
        int escolha;
        do{
            System.out.println("------ A R E A   C L I E N T E ------ ");
            System.out.println("1 - Inserir produto;");
            System.out.println("2 - Catalogo de produtos");
            System.out.println("3 - Finalizar encomenda");
            System.out.println("4 - Registo de encomenda");
            System.out.println("5 - Sair.");
            try {
                escolha = sc.nextInt();
                if (escolha == 1) {
                    Scanner s = new Scanner(System.in);
                    System.out.print("Insira a referencia: ");
                    String codProduto = s.next();
                    System.out.print("Insira a descricao: ");
                    String descricao = s.next();
                    System.out.print("Peso por unidade [kg/U]: ");
                    double pesoU = s.nextDouble();
                    System.out.print("Preco por unidade [€/U]: ");
                    double precoU = s.nextDouble();
                    LinhaEncomenda nova = new LinhaEncomenda(codProduto, descricao, pesoU, precoU);
                    atual.adicionaMapCatalogo(nova);
                } else if (escolha == 2) {
                    catalogoProdutos(atual.getCatalogo());
                } else if (escolha == 3) {
                    List<Encomenda> pen = atual.getPendentes();
                    if (pen.size() == 0) {
                        System.out.println();
                        System.out.println("Nao tem pendentes.\n");
                        System.out.println();
                    } else {
                        for (Encomenda e : pen) {
                            System.out.println(" Encomenda: " + e.getCodEncomenda());
                            System.out.println("\t\t\t Descricao:");
                            for (LinhaEncomenda l : e.getE()) {
                                System.out.print("\t\t\t\t\t\t Produto: ");
                                System.out.println(l.getCodProduto());
                                System.out.print("\t\t\t\t\t\t Nome: ");
                                System.out.println(l.getDescricao());
                                System.out.print("\t\t\t\t\t\t Peso [Kg/U]: ");
                                System.out.println(l.getPesoU());
                                System.out.print("\t\t\t\t\t\t Preco [€/U]: ");
                                System.out.println(l.getPrecoU());
                            }
                        }
                        System.out.print("Escolha a encomenda: ");
                        Scanner sca = new Scanner(System.in);
                        String codEncomenda = sca.next();
                        Encomenda e = atual.devolvePendente(codEncomenda);
                        if (pen.contains(e)) {
                            Utilizador u = p.getUtilizador().get(e.getCodUtilizador());
                            u.adicionaPendentes(e);
                            p.insereAceite(codEncomenda);
                            atual.removePendente(e);
                            System.out.println();
                            System.out.println("Encomenda pronta!\n");
                        } else {
                            System.out.println();
                            System.out.println(" Encomenda inexistente.");
                        }
                    }
                } else if (escolha == 4) {
                    registoL(atual);
                }
            }catch (InputMismatchException e){
                opcao();
                break;
            }
        }while(escolha!=5);
    }

    public void registoL(Loja v){
        if(v.getRegistoU().size() ==0) System.out.println("Ainda nao efetuou encomendas.");
        else{
            for(Encomenda e: v.getRegistoU()){
                System.out.println(" Encomenda: " + e.getCodEncomenda());
                System.out.print("\t\t\t Loja: ");
                System.out.println(e.getCodLoja());
                System.out.print("\t\t\t Peso: ");
                System.out.println(e.getPeso());
                System.out.println("\t\t\t Descricao:");
                for(LinhaEncomenda l: e.getE()) {
                    System.out.print("\t\t\t\t\t\t Produto: ");
                    System.out.println(l.getCodProduto());
                    System.out.print("\t\t\t\t\t\t Nome: ");
                    System.out.println(l.getDescricao());
                    System.out.print("\t\t\t\t\t\t Peso [Kg/U]: ");
                    System.out.println(l.getPesoU());
                    System.out.print("\t\t\t\t\t\t Preco [€/U]: ");
                    System.out.println(l.getPrecoU());
                }
            }
        }
    }

    /* ------------------------------------ V O L U N T A R I O  ------------------------------------*/

    public int classificacaoV(){
        int escolha;
        System.out.println("Como classifica este voluntario?");
        System.out.println("1 - Pessimo");
        System.out.println("2 - Mau");
        System.out.println("3 - Neutro");
        System.out.println("4 - Bom ");
        System.out.println("5 - Excelente");
        System.out.println();
        Scanner sc = new Scanner(System.in);
        escolha = sc.nextInt();
        if(escolha < 0 || escolha > 5){
            while(escolha < 0 || escolha > 5){
                System.out.println("CComo classifica este voluntario?");
                System.out.println("1 - Pessimo");
                System.out.println("2 - Mau");
                System.out.println("3 - Neutro");
                System.out.println("4 - Bom ");
                System.out.println("5 - Excelente");
                System.out.println();
                escolha = sc.nextInt();
            }
        }
        return escolha;
    }

    public void estado(){
        System.out.println("---------- V O L U N T A R I O ----------\n");
        System.out.println("1 - Area Cliente");
        System.out.println("2 - Classificacao");
        System.out.println("3 - Registo de encomendas");
        System.out.println("4 - Sair");
    }

    public void AreaCliente(Voluntario v){
        System.out.println("------- A R E A   D E   C L I E N T E -------");
        System.out.println("1 - Meu Perfil;");
        System.out.println("2 - Editar perfil");
        System.out.println("3 - Sair");
        Scanner sc = new Scanner(System.in);
        int num = sc.nextInt();
        if(num == 1){
            System.out.println("Codigo: " +v.getCodVolu() + ";");
            System.out.println("Nome: " + v.getNome() + ";");
            System.out.println("Localizacao: (" + v.getGps().getLatitude() + ", "+ v.getGps().getLongitude() + ");");
            if(v.isLivre())
                System.out.println("Estado: Livre");
            else
                System.out.println("Estado: Ocupado");
            if(v.isMeds())
                System.out.println("Tem certificado de medicamentos.");
            else
                System.out.println("Não tem certificado de medicamentos.");
        }
        else if(num == 2){
            System.out.println("Pretende alterar o que?");
            System.out.println("1 - Codigo\n2 - Nome\n3 - Localizacao\n4 - Estado\n5 - Certificado de medicamentos ");
            int escolha = sc.nextInt();
            if(escolha == 1){
                System.out.println("Inserir novo codigo:");
                String cod = sc.next();
                v.setCodVolu(cod);
            }
            else if(escolha == 2){
                System.out.println("Inserir novo nome:");
                String nome = sc.next();
                v.setNome(nome);
            }
            else if(escolha == 3){
                System.out.println("Inserir coordenada X:");
                double x = sc.nextDouble();
                System.out.println("Inserir coordenada Y:");
                double y = sc.nextDouble();
                Coordenadas c = new Coordenadas(x,y);
                v.setGps(c);
            }
            else if(escolha == 4){
                v.setLivre(!v.isLivre());
                System.out.println("Estado atualizado.");
            }
            else if(escolha == 5){
                v.setMeds(!v.isMeds());
                System.out.println("Estado atualizado.");
            }
        }
    }

    public void registo(Voluntario v){
        if(v.getRegistoV().size() ==0) System.out.println("Ainda nao efetuou encomendas.");
        else{
            for(Map.Entry<Encomenda,LocalDateTime> vol: v.getRegistoV().entrySet()){
                System.out.println("Encomenda: " + vol.getKey().getCodEncomenda());
                System.out.print("\t\t\t  Loja: ");
                System.out.println(vol.getKey().getCodLoja());
                System.out.print("\t\t\t Peso: ");
                System.out.println(vol.getKey().getPeso());
                System.out.println("\t\t\t Descricao:");
                for(LinhaEncomenda l: vol.getKey().getE()) {
                    System.out.print("\t\t\t\t\t\t Produto: ");
                    System.out.println(l.getCodProduto());
                    System.out.print("\t\t\t\t\t\t Nome: ");
                    System.out.println(l.getDescricao());
                    System.out.print("\t\t\t\t\t\t Peso [Kg/U]: ");
                    System.out.println(l.getPesoU());
                    System.out.print("\t\t\t\t\t\t Preco [€/U]: ");
                    System.out.println(l.getPrecoU());
                }
            }
        }
    }

    public void media(double media){
        System.out.println("Classificacao: " + media);
    }

    /* ------------------------------------ T R A N S P O R T A D O R A  ------------------------------------*/

    public void menuTransp(){
        System.out.println("---------- T R A N S P O R T A D O R A S ----------\n");
        System.out.println("1 - Area Cliente");
        System.out.println("2 - Classificacao");
        System.out.println("3 - Registo de encomendas");
        System.out.println("4 - Faturacao total.");
        System.out.println("5 - Sair");
    }

    public void menuTranspMulti(){
        System.out.println("---------- T R A N S P O R T A D O R A S ----------\n");
        System.out.println("1 - Area Cliente");
        System.out.println("2 - Classificacao");
        System.out.println("3 - Registo de encomendas");
        System.out.println("4 - Faturacao total.");
        System.out.println("5 - Encomendas por entregar");
        System.out.println("6 - Sair");
    }

    public void areaClienteT(Transportadora t){
        System.out.println("------- A R E A   D E   C L I E N T E -------");
        System.out.println("1 - Meu Perfil;");
        System.out.println("2 - Editar Perfil");
        System.out.println("3 - Sair");
        Scanner sc = new Scanner(System.in);
        int num = sc.nextInt();
        if(num == 1){
            System.out.println("Codigo: " + t.getCodEmpr());
            System.out.println("Nome: " + t.getNomeEmpr());
            System.out.println("NIF: " + t.getNif());
            System.out.println("Localizacao: (" + t.getGps().getLatitude() + ", "  + t.getGps().getLongitude()+  ")");
            System.out.println("Raio: " + t.getRaio());
            System.out.println("Preco por km: "+ t.getPreco_km());
            System.out.println("Preco por peso: "  + t.getPreco_peso());
            System.out.println("Velocidade media: " + t.getVelocidade());
            if(t.isMeds())
                System.out.println("Tem certificado de medicamentos");
            else System.out.println("Não tem certificado de medicamentos ");
        }
        if(num == 2){
            System.out.println("Pretende alterar o que?");
            System.out.println("1 - Codigo\n2 - Nome\n3 - NIF\n4 - Localizacao\n5 - Raio\n" +
                               "6 - Preco por Km\n7 - Preco por peso\n8 - Velocidade Media\n9 - Certificado de Medicamentos");
            int escolha = sc.nextInt();
            if(escolha == 1){
                System.out.println("Inserir novo codigo:");
                String cod = sc.next();
               t.setCodEmpr(cod);
            }
            else if(escolha == 2){
                System.out.println("Inserir novo nome:");
                String nome = sc.next();
                t.setNomeEmpr(nome);
            }
            else if(escolha == 3){
                System.out.println("Inserir NIF:");
                int nif = sc.nextInt();
                t.setNif(nif);
            }
            else if(escolha == 4){
                System.out.println("Inserir coordenada X:");
                double x = sc.nextDouble();
                System.out.println("Inserir coordenada Y:");
                double y = sc.nextDouble();
                Coordenadas c = new Coordenadas(x,y);
                t.setGps(c);
            }
            else if(escolha == 5){
                System.out.println("Inserir raio");
                double raio = sc.nextDouble();
                t.setRaio(raio);
            }
            else if(escolha == 6){
                System.out.println("Inserir preco por km:");
                double preco_km = sc.nextDouble();
                t.setPreco_km(preco_km);
            }
            else if(escolha == 7){
                System.out.println("Inserir preco por peso:");
                double preco_peso = sc.nextDouble();
                t.setPreco_peso(preco_peso);
            }
            else if(escolha == 8){
                System.out.println("Inserir velocidade media:");
                double vel = sc.nextDouble();
                t.setVelocidade(vel);
            }
            else if(escolha == 9) {
                t.aceitaMedicamentos(!t.isMeds());
                System.out.println("Estado atualizado.");
            }
        }
    }

    public String transportadoraDisponivel(Map<String,Map.Entry<Double,Double>>trans,TrazAquiModel p){
        Scanner sc = new Scanner(System.in);
        List<String> aux = trans.entrySet().stream().map(Map.Entry::getKey).collect(Collectors.toList());
        for(Map.Entry<String,Map.Entry<Double,Double>> e: trans.entrySet()){
            Transportadora t = p.getTransportador().get(e.getKey());
            if(t instanceof TransportadoraMulti)
                System.out.println("Transportadora Multi (" + ((TransportadoraMulti) t).getNumero() + "): " + e.getKey()+ ": Custo encomenda -> " + e.getValue().getKey()
                                   + "; Tempo de encomenda -> " + e.getValue().getValue());
            else
                System.out.println("Transportadora Normal: " + e.getKey()+ ": Custo encomenda -> " + e.getValue().getKey()
                                   + "; Tempo de encomenda -> " + e.getValue().getValue());
        }
        System.out.println("Escolha a transportadora: ");
        String tran = sc.next();
        while (!(aux.contains(tran) && tran.charAt(0) != 't')) {
            System.out.println("Opção invalida.\n");
            System.out.println("Escolha a transportadora: ");
            tran = sc.next();
        }
        return tran;
    }

    public void transportadoraInvalida(){
        System.out.println("Transportadora escolhida invalida.\n");
    }

    public int classificacaoT(){
        int escolha;
        System.out.println("Como classifica esta transportadora?");
        System.out.println("1 - Pessimo");
        System.out.println("2 - Mau");
        System.out.println("3 - Neutro");
        System.out.println("4 - Bom ");
        System.out.println("5 - Excelente");
        System.out.println();
        Scanner sc = new Scanner(System.in);
        escolha = sc.nextInt();
        if(escolha < 0 || escolha > 5){
            while(escolha < 0 || escolha > 5){
                System.out.println("Como classifica esta transportadora?");
                System.out.println("1 - Pessimo");
                System.out.println("2 - Mau");
                System.out.println("3 - Neutro");
                System.out.println("4 - Bom ");
                System.out.println("5 - Excelente");
                System.out.println();
                escolha = sc.nextInt();
            }
        }
        return escolha;
    }

    public void registoT(Transportadora v){
        if(v.getRegistoT().size() ==0) System.out.println("Ainda nao efetuou encomendas.");
        else{
            for(Map.Entry<Encomenda,LocalDateTime> vol: v.getRegistoT().entrySet()){
                System.out.println(" Encomenda: " + vol.getKey().getCodEncomenda());
                System.out.print("\t\t\t Loja: ");
                System.out.println(vol.getKey().getCodLoja());
                System.out.print("\t\t\t Peso: ");
                System.out.println(vol.getKey().getPeso());
                System.out.println("\t\t\t Descricao:");
                for(LinhaEncomenda l: vol.getKey().getE()) {
                    System.out.print("\t\t\t\t\t\t Produto: ");
                    System.out.println(l.getCodProduto());
                    System.out.print("\t\t\t\t\t\t Nome: ");
                    System.out.println(l.getDescricao());
                    System.out.print("\t\t\t\t\t\t Peso [Kg/U]: ");
                    System.out.println(l.getPesoU());
                    System.out.print("\t\t\t\t\t\t Preco [€/U]: ");
                    System.out.println(l.getPrecoU());
                }
            }
        }
    }

    public Map.Entry<LocalDateTime,LocalDateTime> dadosFaturacao(){
        Scanner s = new Scanner(System.in);
        System.out.print("Dia: ");
        int di =s.nextInt();
        System.out.print("Mes: ");
        int mi = s.nextInt();
        System.out.print("Ano ");
        int ai = s.nextInt();
        System.out.print("Dia: ");
        int df =s.nextInt();
        System.out.print("Mes: ");
        int mf = s.nextInt();
        System.out.print("Ano ");
        int af = s.nextInt();
        LocalDateTime ti = LocalDateTime.of(ai,mi,di,0,0);
        LocalDateTime tf = LocalDateTime.of(af,mf,df,0,0);
        return new AbstractMap.SimpleEntry<>(ti,tf);
    }

    public void faturacao(LocalDateTime ti, LocalDateTime tf,double faturacao){
        System.out.println("Faturacao total de " +ti.getDayOfMonth() + "-" + ti.getMonth() + "-" + ti.getYear() + " a  " +tf.getDayOfMonth() +"-" +  tf.getMonth()  + "-" + tf.getYear() + ": " + faturacao);
    }

    public void encomendasPorEntregar(TransportadoraMulti t){
        int i = 1;
        System.out.println("Capacidade de transporte: " + t.getNumero() + "\n");
        if(t.getPendentes().size() == 0)
            System.out.println("Não tem entregas a fazer.");
        for(Encomenda e: t.getPendentes())
            System.out.println(i + " -> " + e.getCodEncomenda());
    }
}
