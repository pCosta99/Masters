package trazaqui;

import java.io.*;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import trazaqui.Exceptions.*;
import java.time.format.DateTimeParseException;

public class Menu implements Serializable{

    private BaseDados b_dados;
    private Armazena a_armazena;

    public Menu(Armazena armazena){
        b_dados = lerEstado("Estado.bin");
        a_armazena=armazena;
        menuInicial();
    }

    /**
     * ALTERAR LOCALIZAÇAO
     */
    private void mudarLocalizacaoUtilizador(String username){
        Scanner s = new Scanner(System.in);
        double cordx, cordy;
        System.out.println("Posicao X:");
        try{
            cordx = s.nextDouble();
        }
        catch(InputMismatchException e){
            System.out.println("Opcao invalida");
            return;
        }
        System.out.println("Posicao Y:");
        try{
            cordy = s.nextDouble();
        }
        catch(InputMismatchException e){
            System.out.println("Opcao invalida");
            return;
        }
        b_dados.setLocalizacaoUtilizador(username, cordx, cordy);
    }

    private void mudarLocalizacaoLoja(String username){
        Scanner s = new Scanner(System.in);
        double cordx, cordy;
        System.out.println("Posicao X:");
        try{
            cordx = s.nextDouble();
        }
        catch(InputMismatchException e){
            System.out.println("Opcao invalida");
            return;
        }
        System.out.println("Posicao Y:");
        try{
            cordy = s.nextDouble();
        }
        catch(InputMismatchException e){
            System.out.println("Opcao invalida");
            return;
        }
        b_dados.setLocalizacaoloja(username, cordx, cordy);
    }

    private void mudarLocalizacaoTransportadora(String username){
        Scanner s = new Scanner(System.in);
        double cordx, cordy;
        System.out.println("Posicao X:");
        try{
            cordx = s.nextDouble();
        }
        catch(InputMismatchException e){
            System.out.println("Opcao invalida");
            return;
        }
        System.out.println("Posicao Y:");
        try{
            cordy = s.nextDouble();
        }
        catch(InputMismatchException e){
            System.out.println("Opcao invalida");
            return;
        }
        b_dados.setLocalizacaotransportadora(username, cordx, cordy);
    }

    private void mudarLocalizacaoVoluntario(String username){
        Scanner s = new Scanner(System.in);
        double cordx, cordy;
        System.out.println("Posicao X:");
        try{
            cordx = s.nextDouble();
        }
        catch(InputMismatchException e){
            System.out.println("Opcao invalida");
            return;
        }
        System.out.println("Posicao Y:");
        try{
            cordy = s.nextDouble();
        }
        catch(InputMismatchException e){
            System.out.println("Opcao invalida");
            return;
        }
        b_dados.setLocalizacaovoluntario(username, cordx, cordy);
    }

    public double calculaPreco(double preco,double stock,double quant){
        return ((preco*quant)/stock);
    }

    public double calculaPeso(List<LinhaEncomenda> l){
        double res=0;
        for(LinhaEncomenda e : l){
            res+=e.getQuantidade();
        }
        return res;
    }

    public void fazerEncomenda(String codLoja,String codUtilizador){
        ArrayList<LinhaEncomenda> encs = new ArrayList<>();
        try {
            String opcao;
            do {
                System.out.println("1 - Adicionar um produto ao carrinho");
                System.out.println("2 - Remover um produto do carrinho");
                System.out.println("3 - Efetuar encomenda");
                System.out.println("0 - Retroceder");

                Scanner s = new Scanner(System.in);
                opcao = s.nextLine() ;

                if (opcao.equals("0")){
                    break;
                }

                switch (opcao) {
                    case "1":
                        System.out.println(b_dados.getLoja(codLoja).getCatalogoProdutos().toString());
                        System.out.println("Introduza o código do produto!");
                        opcao = s.nextLine();
                        System.out.print(opcao);
                        System.out.print("\n");
                        Produto p = buscaCatalogo(codLoja).getProduto(opcao);
                        System.out.println("Indique a quantidade");
                        double q = Double.parseDouble(s.nextLine());
                        if(q<=p.getStock()) {
                            try {
                                b_dados.atualizaQuantidade(codLoja, opcao, q);
                            } catch (ProdutoNaoExisteException e) {
                                e.printStackTrace();
                            }
                        }
                        else{
                            System.out.println("Quantidade do produto insuficiente");
                            break;
                        }
                        System.out.print(q);
                        System.out.print("\n");
                        double preco = calculaPreco(p.getPreco(), p.getStock(), q);
                        System.out.print(preco);
                        System.out.print("\n");
                        LinhaEncomenda l = new LinhaEncomenda(opcao, p.getDescricao(), preco, q);
                        encs.add(l);
                        System.out.print("Carrinho:");
                        System.out.print(encs);
                        System.out.print("\n");
                        break;
                    case "2":
                        System.out.println("Introduza o código do produto!");
                        opcao = s.nextLine();
                        System.out.print(opcao);
                        System.out.print("\n");
                        System.out.println("Indique a quantidade");
                        double qu = Double.parseDouble(s.nextLine());
                        b_dados.atualizaQuantidade(codLoja,opcao,qu);
                        int index =0;
                        for(LinhaEncomenda le : encs) {
                            if (le.getCodProd().equals(opcao)) {
                                if(le.getQuantidade()==qu){
                                    break;
                                }else{
                                    le.setQuantidade(le.getQuantidade()-qu);
                                    le.setPreco(calculaPreco(le.getPreco(),le.getQuantidade(),qu));
                                    index=-1;
                                    break;
                                }
                            }
                            else{
                                System.out.print("O produto não se encontra no seu carrinho!\n");
                            }
                            index+=1;
                        }

                        if(index!=-1){
                            encs.remove(index);
                        }
                        System.out.println(encs);
                        break;

                    case "3":
                        double peso = calculaPeso(encs);
                        Encomenda e = b_dados.novaEncomenda(codUtilizador, codLoja, peso, encs);
                        System.out.println("Escolha o método de entrega");
                        metodoEncomenda(codUtilizador,codLoja,e.getcodEncomenda());
                        encs=new ArrayList<>();
                        break;
                    default:
                        System.out.print("Opção inválida\n\n");
                        break;

                }
            }
            while (true);
        }
        catch(CatalogoNaoExisteException e) {
            System.out.println("Nao existe catalogo");
        }
        catch(ProdutoNaoExisteException e) {
            System.out.println("Nao existe produto");
        }
        catch(InputMismatchException e) {
            System.out.println("Entrada inválida");
        }
    }

    private void verLojas(String codUtilizador) {
        Scanner s = new Scanner(System.in);
        String opcao = "";
        try {
            do {
                Set<String> alfa = b_dados.lojasOrdemAlfabetica();
                System.out.println("Escreva o nome da loja que deseja visitar!");
                lojasOrdemAlfabeticaDisplay(alfa);

                System.out.println("0 - Retroceder");

                opcao = s.nextLine();
                if(opcao.equals("0")){
                    break;
                }
                System.out.print("\n");
                for (LogLoja l : b_dados.getLojas().values()) {
                    if (l.getNome().equals(opcao)) {
                        fazerEncomenda(l.getCodLoja(), codUtilizador);
                    }
                }
            }
            while (opcao.equals(""));
        } catch (NaoExisteLojasRegistadasException e) {
            System.out.println(e.getMessage());
        } catch (InputMismatchException e) {
            System.out.println("Entrada inválida");
        }
    }

    /**
     *Lojas por ordem alfabetica
     */
    public void lojasOrdemAlfabeticaDisplay(Set<String> s) {
        Iterator<String> it = s.iterator();
        while (it.hasNext()) {
            System.out.println(it.next());
        }
    }

    public CatalogoProdutos buscaCatalogo(String cod) throws CatalogoNaoExisteException{
        for(CatalogoProdutos c : a_armazena.getCatalogos()){
            if(cod.equals(c.getCodLoja())){
                return c;
            }
        }
        throw new CatalogoNaoExisteException();
    }

    /**
     * ASSOCIAR CONTA CLIENTE, LOJA, TRANSPORTADORA, VOLUNTARIO
     */
    private void associarContaCliente(){
        Scanner s = new Scanner(System.in);
        int i=0;
        while (i==0) {
            System.out.println("Introduza o seu Codigo de Utilizador");
            String cod = s.nextLine();
            if(a_armazena.existeCodUser(cod)) {
                i=1;
                Utilizador u = a_armazena.getUtilizador(cod);
                System.out.println(u.toString());
                System.out.println("Introduza o seu Username");
                String username = s.nextLine();
                System.out.println("Introduza o seu Password");
                String pass = s.nextLine();

                try {
                    b_dados.associaUtilizador(u.getCodUtilizador(), u.getNome(), u.getGps(), username, pass);
                } catch (UtilizadorExisteException e) {
                    System.out.println(e.getMessage());
                } catch (UsernameJaEstaEmUsoException e) {
                    System.out.println(e.getMessage());
                }
            }
            else
                System.out.println("Esse código não existe tente de novo");
        }
    }

    private void associarContaLoja() {
        Scanner s = new Scanner(System.in);
        int i = 0;
        while (i == 0) {
            System.out.println("Introduza o seu Codigo de Loja");
            String cod = s.nextLine();
            if (a_armazena.existeCodLoja(cod)) {
                i = 1;
                Loja l = a_armazena.getLoja(cod);
                System.out.println(l.toString());
                System.out.println("Introduza o seu Username");
                String username = s.nextLine();
                System.out.println("Introduza o seu Password");
                String pass = s.nextLine();
                CatalogoProdutos cp = new CatalogoProdutos();
                HashMap<String, Encomenda> encs = new HashMap<>();
                for (Encomenda e : a_armazena.getEncomendas().values()) {
                    if (e.getcodLoja().equals(cod)) {
                        encs.put(e.getcodEncomenda(), e.clone());
                    }
                }
                try {
                    cp = buscaCatalogo(cod);
                } catch (CatalogoNaoExisteException e) {
                    System.out.println(e.getMessage());
                }

                try {
                    b_dados.associaLoja(l.getCodLoja(), l.getNome(), l.getGps(), username, pass, cp);
                    for (Encomenda e : encs.values()) {
                        if (a_armazena.checkAceite(e)) {
                            b_dados.associaAceite(e.getcodEncomenda(), e.getcodUtilizador(), e.getcodLoja(), e.getPeso(), e.getLinhas());
                        } else {
                            b_dados.associaEncomenda(e.getcodEncomenda(), e.getcodUtilizador(), e.getcodLoja(), e.getPeso(), e.getLinhas());
                        }
                    }
                } catch (LojaExisteException e) {
                    System.out.println(e.getMessage());
                } catch (UsernameJaEstaEmUsoException e) {
                    System.out.println(e.getMessage());
                } catch (EncomendaExisteException e) {
                    System.out.println(e.getMessage());
                }
            } else
                System.out.println("Esse código não existe tente de novo");
        }
    }

    private void associarContaTransportadora() {
        Scanner s = new Scanner(System.in);
        int i = 0;
        while (i == 0) {
            System.out.println("Introduza o seu Codigo de Transportadora");
            String cod = s.nextLine();
            if (a_armazena.existeCodTrans(cod)) {
                i=1;
                Transportadora t = a_armazena.getTransportadora(cod);
                System.out.println(t.toString());
                System.out.println("Introduza o seu Username");
                String username = s.nextLine();
                System.out.println("Introduza o seu Password");
                String pass = s.nextLine();


                try {
                    b_dados.associaTransportadora(t.getCodEmpresa(), t.getNome(), t.getGps(), t.getNif(), t.getRaio(), t.getPrecokm(), username, pass);
                } catch (TransportadoraExisteException e) {
                    System.out.println(e.getMessage());
                } catch (UsernameJaEstaEmUsoException e) {
                    System.out.println(e.getMessage());
                }
            } else
                System.out.println("Esse código não existe tente de novo");

        }
    }

    private void associarContaVoluntario(){
        boolean disponibilidade = true;
        Scanner s = new Scanner(System.in);
        int i=0;
        while(i==0) {
            System.out.println("Introduza o seu Codigo de Voluntário");
            String cod = s.nextLine();
            if (a_armazena.existeCodVol(cod)) {
                i=1;
                Voluntario v = a_armazena.getVoluntario(cod);
                System.out.println(v.toString());
                System.out.println("Introduza o seu Username");
                String username = s.nextLine();
                System.out.println("Introduza o seu Password");
                String pass = s.nextLine();
                try {
                    b_dados.associaVoluntario(v.getCodVoluntario(), v.getNome(), v.getGps(), v.getRaio(), username, pass, disponibilidade);
                } catch (VoluntarioExisteException e) {
                    System.out.println(e.getMessage());
                } catch (UsernameJaEstaEmUsoException e) {
                    System.out.println(e.getMessage());
                }
            }
            else
                System.out.println("Esse código não existe tente de novo");
        }
    }

    /**
     * REGISTO CLIENTE, LOJA, TRANSPORTADORA, VOLUNTARIO
     */
    private void registoCliente(){

        Scanner s = new Scanner(System.in);
        System.out.println("Introduza o seu Nome");
        String nome = s.nextLine();
        System.out.println("Introduza o seu Username");
        String username = s.nextLine();
        System.out.println("Introduza o seu Password");
        String pass = s.nextLine();
        System.out.println("Introduza a sua Coordenada X");
        double gpsx= Double.parseDouble(s.nextLine());
        System.out.println("Introduza a sua Coordenada Y");
        double gpsy = Double.parseDouble(s.nextLine());
        Localizacao pos = new Localizacao(gpsx,gpsy);

        try{
            b_dados.novoUtilizador(nome, pos,username,pass);
        }

        catch(UtilizadorExisteException e){
            System.out.println(e.getMessage());
        }
        catch(UsernameJaEstaEmUsoException e){
            System.out.println(e.getMessage());
        }

    }

    private void registoLoja(){

        Scanner s = new Scanner(System.in);
        System.out.println("Introduza o seu Nome");
        String nome = s.nextLine();
        System.out.println("Introduza o seu Username");
        String username = s.nextLine();
        System.out.println("Introduza o seu Password");
        String pass = s.nextLine();
        System.out.println("Introduza a sua Coordenada X");
        double gpsx= Double.parseDouble(s.nextLine());
        System.out.println("Introduza a sua Coordenada Y");
        double gpsy = Double.parseDouble(s.nextLine());
        Localizacao pos = new Localizacao(gpsx,gpsy);

        try{
            b_dados.novaLoja(nome,pos,username,pass);
        }

        catch(LojaExisteException e){
            System.out.println(e.getMessage());
        }
        catch(UsernameJaEstaEmUsoException e){
            System.out.println(e.getMessage());
        }

    }

    private void registoTransportadora(){

        Scanner s = new Scanner(System.in);
        System.out.println("Introduza o seu Nome");
        String nome = s.nextLine();
        System.out.println("Introduza o seu Username");
        String username = s.nextLine();
        System.out.println("Introduza o seu Password");
        String pass = s.nextLine();
        System.out.println("Introduza o seu NIF");
        String nif = s.nextLine();
        System.out.println("Introduza a sua Coordenada X");
        double gpsx= Double.parseDouble(s.nextLine());
        System.out.println("Introduza a sua Coordenada Y");
        double gpsy = Double.parseDouble(s.nextLine());
        Localizacao pos = new Localizacao(gpsx,gpsy);
        System.out.println("Introduza o seu raio de atuação");
        double raio= Double.parseDouble(s.nextLine());
        System.out.println("Introduza o preço cobrado por kilometro");
        double preco= Double.parseDouble(s.nextLine());

        try{
            b_dados.novaTransportadora(nome,pos,nif,raio,preco,username,pass);
        }

        catch(TransportadoraExisteException e){
            System.out.println(e.getMessage());
        }
        catch(UsernameJaEstaEmUsoException e){
            System.out.println(e.getMessage());
        }

    }

    private void registoVoluntario(){
        Scanner s = new Scanner(System.in);
        System.out.println("Introduza o seu Nome");
        String nome = s.nextLine();
        System.out.println("Introduza o seu Username");
        String username = s.nextLine();
        System.out.println("Introduza o seu Password");
        String pass = s.nextLine();
        System.out.println("Introduza a sua Coordenada X");
        double gpsx= Double.parseDouble(s.nextLine());
        System.out.println("Introduza a sua Coordenada Y");
        double gpsy = Double.parseDouble(s.nextLine());
        Localizacao pos = new Localizacao(gpsx,gpsy);
        System.out.println("Introduza o seu raio de atuação");
        double raio= Double.parseDouble(s.nextLine());

        try{
            b_dados.novoVoluntario(nome,pos,raio,username,pass,true);
        }

        catch(VoluntarioExisteException e){
            System.out.println(e.getMessage());
        }
        catch(UsernameJaEstaEmUsoException e){
            System.out.println(e.getMessage());
        }
    }

    /**
     * CONSULTA DADOS CLIENTE, LOJA, TRANSPORTADORA, VOLUNTARIO
     */
    private void consultadadosUtilizador(String username){
        Scanner s = new Scanner(System.in); int opcao = 0;
        try{
            do{
                System.out.println("Informações de Conta ");
                System.out.println(b_dados.getUtilizadores().get(username).toString());
                System.out.println("1-Mudar localizacao");
                System.out.println("0 - Retroceder");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 1:
                        mudarLocalizacaoUtilizador(username);
                        break;
                    default:
                        System.out.print("Opção inválida\n\n");
                        break;

                }
            }while(opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuCliente(username);

        }
    }

    private void consultadadosLoja(String username){
        Scanner s = new Scanner(System.in); int opcao = 0;
        try{
            do{
                System.out.println("Informações de Conta ");
                System.out.println(b_dados.getLojas().get(username).toString());
                System.out.println("1-Mudar localizacao");
                System.out.println("0 - Retroceder");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 1:
                        mudarLocalizacaoLoja(username);
                        break;
                    default:
                        System.out.print("Opção inválida\n\n");
                        break;

                }
            }while(opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuLoja(username);

        }
    }

    private void consultadadosTransportadora(String username){
        Scanner s = new Scanner(System.in); int opcao = 0;
        try{
            do{
                System.out.println("Informações de Conta ");
                System.out.println(b_dados.getTrasnportadoras().get(username).toString());
                System.out.println("1-Mudar localizacao");
                System.out.println("0 - Retroceder");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 1:
                        mudarLocalizacaoTransportadora(username);
                        break;
                    default:
                        System.out.print("Opção inválida\n\n");
                        break;

                }
            }while(opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuTransportadora(username);

        }
    }

    private void consultadadosVoluntario(String username){
        Scanner s = new Scanner(System.in); int opcao = 0;
        try{
            do{
                System.out.println("Informações de Conta ");
                System.out.println(b_dados.getVoluntarios().get(username).toString());
                System.out.println("1-Mudar localizacao");
                System.out.println("0 - Retroceder");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 1:
                        mudarLocalizacaoVoluntario(username);
                        break;
                    default:
                        System.out.print("Opção inválida\n\n");
                        break;

                }
            }while(opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuVoluntario(username);

        }
    }

    private void consultarProdutos(String cod){
        for(CatalogoProdutos c : a_armazena.getCatalogos()){
            if(c.getCodLoja().equals(cod)){
                System.out.println(c);
            }
        }
    }


    /**
     * MENUS CLIENTE, LOJA, TRANSPORTADORA, VOLUNTARIO
     */
    private void menuCliente(String username){
        a_armazena.juntaCatalogos();
        a_armazena.JuntaProdutos();
        ArrayList<String> encs=b_dados.buscapraClassificar(username);
        if(!encs.isEmpty())
            menuClassifica(encs,username);
        Scanner s = new Scanner(System.in);
        int opcao = 0;
        String op="";

        try{
            do{
                System.out.println("Escolha o que pretende fazer");
                System.out.println("1 - Ver Lojas");
                System.out.println("2 - Consultar Dados Pessoais");
                System.out.println("3 - Historico de Encomendas");
                System.out.println("4 - Ver estado de encomenda");


                System.out.println("0 - Retroceder");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 0:
                        break;
                    case 1:
                        verLojas(b_dados.getUtilizadores().get(username).getCodUtilizador());
                        break;
                    case 2:
                        consultadadosUtilizador(username);
                        break;
                    case 3:
                        b_dados.buscaHistoricoDisplay(b_dados.buscaHistoricoUtilizador(b_dados.getUtilizadores().get(username).getCodUtilizador()));
                        break;
                    case 4:
                        System.out.println("As encomendas que ainda não foram entregues são");
                        b_dados.displayencsnaoentregues(username);
                        EstadoEncomenda();
                        break;
                    default:
                        System.out.print("Opção inválida\n\n");
                        break;

                }
            }while(opcao != 0);
        }
        catch (UtilizadorNaoExisteException e){
            System.out.println(e.getMessage());
            menuCliente(username);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuCliente(username);

        }
    }

    private void menuLoja(String username){
        a_armazena.juntaCatalogos();
        a_armazena.JuntaProdutos();
        Scanner s = new Scanner(System.in); int opcao = 0;
        try{
            do{
                System.out.println("Escolha o que pretende fazer");
                System.out.println("1 - Alterar Catálogo de Produtos");
                System.out.println("2 - Consultar dados pessoais");
                System.out.println("3 - Consultar encomendas");
                System.out.println("4 - Histórico");
                System.out.println("0 - Retroceder");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 0:
                        break;
                    case 1:
                        menuAlterarProdutos(username);
                        break;
                    case 2:
                        consultadadosLoja(username);
                        break;
                    case 3:
                        verEncomendas(b_dados.getLojas().get(username).getCodLoja());
                        break;
                    case 4:
                        b_dados.buscaHistoricoDisplay(b_dados.buscaHistoricoLoja(b_dados.getLojas().get(username).getCodLoja()));
                        break;
                    default:
                        System.out.print("Opção inválida\n\n");
                        break;

                }
            }while(opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuLoja(username);
        }
        catch (LojaNaoExisteException e){
            System.out.println(e.getMessage());
            menuLoja(username);
        }
    }

    private void menuTransportadora(String username){
        Scanner s = new Scanner(System.in); int opcao = 0;
        try{
            do{
                System.out.println("Escolha o que pretende fazer");
                System.out.println("1 - Encomendas dentro do raio");
                System.out.println("2 - Dados pessoais");
                System.out.println("3 - Historico de encomendas");
                System.out.println("4 - Classificação");
                System.out.println("5 - Sinalizar que uma encomenda foi entregue");
                System.out.println("6 - Lucro num determinado dia");
                System.out.println("0 - Retroceder");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 0:
                        break;
                    case 1:
                        menuEncomendasTrans(username);
                        break;
                    case 2:
                        consultadadosTransportadora(username);
                        break;
                    case 3:
                        b_dados.buscaHistoricoDisplay(b_dados.buscaHistoricoTransportadora(b_dados.getTrasnportadoras().get(username).getCodEmpresa()));
                        break;
                    case 4:
                        b_dados.classifMediaTrans(b_dados.getTrasnportadoras().get(username));
                        break;
                    case 5:
                        menuEntregueTrans(username);
                        break;
                    case 6:
                        menuprecoperiodo(username);
                        break;
                    default:
                        System.out.print("Opção inválida\n\n");
                        break;

                }
            }while(opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuTransportadora(username);

        }
        catch(TransportadoraNaoExisteException e){
            System.out.println(e.getMessage());
            menuTransportadora(username);
        }
    }

    private void menuVoluntario(String username){
        Scanner s = new Scanner(System.in); int opcao = 0;
        try{
            do{
                System.out.println("Escolha o que pretende fazer");
                System.out.println("1 - Consultar encomendas no meu raio");
                System.out.println("2 - historico de encomendas");
                System.out.println("3 - Dados pessoais");
                System.out.println("4 - Classificação");
                System.out.println("5 - Sinalizar que uma encomenda foi entregue");
                System.out.println("0 - Retroceder");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 0:
                        break;
                    case 1:
                        if(b_dados.getVoluntarios().get(username).getDisponibilidade())
                            menuEncomendasVol(username);
                        else
                            System.out.println("Você já se encontrar a transportar uma encomenda.");
                        break;
                    case 2:
                        b_dados.buscaHistoricoDisplay(b_dados.buscaHistoricoVoluntario(b_dados.getVoluntarios().get(username).getCodVoluntario()));
                        break;
                    case 3:
                        consultadadosVoluntario(username);
                        break;
                    case 4:
                        b_dados.classifMediaVol(username);
                        break;
                    case 5:
                        menuEntregueVol(username);
                        break;
                    default:
                        System.out.print("Opção inválida\n\n");
                        break;

                }
            }while(opcao != 0);
        }
        catch (VoluntarioNaoExisteException e){
            System.out.println(e.getMessage());
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuVoluntario(username);

        }
    }

    /**
     * LOGIN CLIENTE, LOJA, TRANSPORTADORA, VOLUNTARIO
     */
    private void loginCliente(){
        Scanner s = new Scanner(System.in); int contador = 0;
        String username,pass,k;
        try{
            do{
                System.out.println("Introduza o seu username");
                username= s.nextLine();
                System.out.println("Introduza a sua password");
                pass = s.nextLine();
                contador++;
                k = b_dados.checkUserPassUtil(username,pass);
                if(k.equals("NOK")) System.out.println("Dados de acesso inválidos ou não registado\n");
            }while(k.equals("NOK")&& contador < 3);
            if(k.equals("NOK")) //menuInicial();
                return;

            else menuCliente(username);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuInicial();
            return;

        }
    }

    private void loginLoja(){
        Scanner s = new Scanner(System.in); int contador = 0;
        String username,pass,k;
        try{
            do{
                System.out.println("Introduza o seu username");
                username= s.nextLine();
                System.out.println("Introduza a sua password");
                pass = s.nextLine();
                contador++;
                k = b_dados.checkUserPassLoj(username,pass);
                if(k.equals("NOK")) System.out.println("Dados de acesso inválidos ou não registado\n");
            }while(k.equals("NOK")&& contador < 3);
            if(k.equals("NOK")) //menuInicial();
                return;

            else menuLoja(username);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuInicial();
            return;

        }
    }

    private void loginTransportadora(){
        Scanner s = new Scanner(System.in);
        int contador = 0;
        String username,pass,k;
        try{
            do{
                System.out.println("Introduza o seu username");
                username= s.nextLine();
                System.out.println("Introduza a sua password");
                pass = s.nextLine();
                contador++;
                k = b_dados.checkUSerPassTrans(username,pass);
                if(k.equals("NOK")) System.out.println("Dados de acesso inválidos ou não registado\n");
            }while(k.equals("NOK")&& contador < 3);
            if(k.equals("NOK")) //menuInicial();
                return;

            else menuTransportadora(username);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuInicial();
            return;

        }
    }

    private void loginVoluntario(){
        Scanner s = new Scanner(System.in); int contador = 0;
        String username,pass,k;
        try{
            do{
                System.out.println("Introduza o seu username");
                username= s.nextLine();
                System.out.println("Introduza a sua password");
                pass = s.nextLine();
                contador++;
                k = b_dados.checkUserPassVol(username,pass);
                if(k.equals("NOK")) System.out.println("Dados de acesso inválidos ou não registado\n");
            }while(k.equals("NOK")&& contador < 3);
            if(k.equals("NOK")) //menuInicial();
                return;

            else menuVoluntario(username);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuInicial();
            return;

        }
    }

    /**
     *SUBMENUS CLIENTE, LOJA, TRANSPORTADORA, VOLUNTARIO
     */
    private void submenuCliente(){
        Scanner s = new Scanner(System.in);
        int opcao = 0;

        try{
            do{
                System.out.println("1 - Criar Conta");
                System.out.println("2 - Já possui conta? Log In");
                System.out.println("3 - Associar conta");
                System.out.println("0 - Sair");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 1:
                        registoCliente();
                        break;
                    case 2:
                        loginCliente();
                        break;
                    case 3:
                        associarContaCliente();
                        break;
                    case 0:
                        break;

                    default:
                        System.out.println("Opção inválida");
                        break;
                }
            } while (opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuInicial();

        }
    }

    private void submenuLoja(){
        Scanner s = new Scanner(System.in);
        int opcao = 0;

        try{
            do{
                System.out.println("1 - Criar Conta");
                System.out.println("2 - Já possui conta? Log In");
                System.out.println("3 - Associar conta");
                System.out.println("0 - Sair");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 1:
                        registoLoja();
                        break;
                    case 2:
                        loginLoja();
                        break;
                    case 3:
                        associarContaLoja();
                        break;
                    case 0:
                        break;

                    default:
                        System.out.println("Opção inválida");
                        break;
                }
            } while (opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuInicial();

        }
    }

    private void submenuTransportadora(){
        Scanner s = new Scanner(System.in);
        int opcao = 0;

        try{
            do{
                System.out.println("1 - Criar Conta");
                System.out.println("2 - Já possui conta? Log In");
                System.out.println("3 - Associar conta");
                System.out.println("0 - Sair");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 1:
                        registoTransportadora();
                        break;
                    case 2:
                        loginTransportadora();
                        break;
                    case 3:
                        associarContaTransportadora();
                        break;
                    case 0:
                        break;

                    default:
                        System.out.println("Opção inválida");
                        break;
                }
            } while (opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuInicial();

        }
    }

    private void submenuVoluntario(){
        Scanner s = new Scanner(System.in);
        int opcao = 0;

        try{
            do{
                System.out.println("1 - Criar Conta");
                System.out.println("2 - Já possui conta? Log In");
                System.out.println("3 - Associar conta");
                System.out.println("0 - Sair");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 1:
                        registoVoluntario();
                        break;
                    case 2:
                        loginVoluntario();
                        break;
                    case 3:
                        associarContaVoluntario();
                        break;
                    case 0:
                        break;

                    default:
                        System.out.println("Opção inválida");
                        break;
                }
            } while (opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuInicial();

        }
    }


    /**
     * MENU INICIAL
     * São apresentadas as primeiras opções ao utilizador
     */
    private void menuInicial(){
        Scanner s = new Scanner(System.in);
        int opcao = 0;

        try{
            do{

                System.out.println("Bem-vindo ao Traz Aqui! Escolha uma funcionalidade.");
                System.out.println("1 - Cliente");
                System.out.println("2 - Loja");
                System.out.println("3 - Transportadora");
                System.out.println("4 - Voluntario");
                System.out.println("5 - Ver top10 de utilizadores");
                System.out.println("6 - Ver top10 de Empresas Transportadoras");
                System.out.println("0 - Sair");

                opcao = s.nextInt();
                System.out.print("\n");

                switch(opcao){
                    case 1:
                        submenuCliente();
                        break;
                    case 2:
                        submenuLoja();
                        break;
                    case 3:
                        submenuTransportadora();
                        break;
                    case 4:
                        submenuVoluntario();
                        break;
                    case 5:
                        List<LogUtilizador> topu=b_dados.top10utilizadores();
                        b_dados.displaytopusers(topu);
                        break;
                    case 6:
                        List<LogTransportadora> topt=b_dados.top10transportadoras();
                        b_dados.displaytoptrans(topt);
                    case 0:
                        guardaEstado("Estado.bin");
                        break;

                    default:
                        System.out.println("Opção inválida");
                        break;
                }
            } while (opcao != 0);
        }
        catch(InputMismatchException e){
            System.out.println("Entrada inválida");
            menuInicial();

        }
    }

    //Método que permite uma loja ver as encomendas que lhe foram emitidas
    public void verEncomendas(String cod){
        Scanner s = new Scanner(System.in);
        String opcao = "";
        try {
            do {
                ArrayList<Encomenda> encs=b_dados.buscaEncomendas(cod);
                if(encs.isEmpty()){
                    System.out.println("Não existem encomendas!");
                    break;
                }
                else {
                    System.out.println("Escreva o codigo de encomenda que está pronta a entregar:");
                    b_dados.buscaEncomendasDisplay(encs);
                    System.out.println("0 - Retroceder");

                    opcao = s.nextLine();
                    if(opcao.equals("0")){
                        break;
                    }
                    System.out.print("\n");
                    for(Encomenda e: encs){
                        if(e.getcodEncomenda().equals(opcao)){
                            b_dados.addEncomendaDisponivel(e);
                            b_dados.removeEncomenda(e);
                        }
                        else{
                            System.out.println("Não existe uma encomenda com esse código!");
                        }
                    }
                    encs=b_dados.buscaEncomendas(cod);
                    if(encs.isEmpty()) {
                        System.out.println("Não existem mais encomendas.");
                        break;
                    }
                }
            }
            while(true);
        }
        catch (LojaNaoExisteException e){
            System.out.println(e.getMessage());
        }
        catch(InputMismatchException e) {
            System.out.println("Entrada inválida");
        }
    }

    //menu para um cliente decidir o método de entrega que deseja usar
    public void metodoEncomenda(String codUtilizador, String codLoja, String codEncomenda){
        int i=0;
        try{
            do{
                Scanner s= new Scanner(System.in);

                System.out.println("1-Entrega por uma empresa transportadora");
                System.out.println("(Sujeito a custos adicionais)");
                System.out.println("2-Entrega por um voluntário");
                System.out.println("(Sujeito a tempo de espera)");
                String opcao= s.nextLine();

                switch(opcao){
                    case "1":
                        ArrayList<LogTransportadora> lt=b_dados.buscatransportadoras(codUtilizador,codLoja);
                        if (lt.isEmpty()){
                            System.out.println("Não existem empresas transportadoras registadas na sua localização");
                            System.out.println("Deseja:");
                            System.out.println("1-Aguardar por um voluntário");
                            System.out.println("2-Cancelar encomenda");
                            String op = s.nextLine();
                            if (op.equals("1")){
                                b_dados.addVol(codEncomenda);
                                System.out.println("Pedido efetuado com sucesso");
                                System.out.print("Encomenda efetuada com sucesso!\n");
                                System.out.print("Codigo da sua Encomenda:" + codEncomenda);
                                System.out.print("\n");
                                i=1;
                                break;
                            }
                            else if (op.equals("2")){
                                b_dados.removeEncomenda(b_dados.getEncomendas().get(codEncomenda));
                                System.out.println("Encomenda cacelada com sucesso!");
                                i=1;
                                break;
                            }
                        }
                        else {
                            while (true) {
                                b_dados.transportadoraDisplay(lt);
                                System.out.println("Escreva o código de transportadora que queira contratar");
                                String op = s.nextLine();
                                int c = 0;
                                for (LogTransportadora t : lt) {
                                    if (t.getCodEmpresa().equals(op)) {
                                        b_dados.addFlag(op, codEncomenda);
                                        c = -1;
                                        break;
                                    }
                                }
                                if (c == 0) {
                                    System.out.println("Código de transportadora não existe");
                                    System.out.println("Tente de Novo");
                                }
                                else
                                    break;
                            }
                            System.out.println("Pedido efetuado com sucesso");
                            System.out.print("Encomenda efetuada com sucesso!\n");
                            System.out.print("Codigo da sua Encomenda:" + codEncomenda);
                            System.out.print("\n");
                            i=1;
                        }
                        break;
                    case "2":
                        b_dados.addVol(codEncomenda);
                        System.out.println("Pedido efetuado com sucesso");
                        System.out.print("Encomenda efetuada com sucesso!\n");
                        System.out.print("Codigo da sua Encomenda:" + codEncomenda);
                        System.out.print("\n");
                        i=1;
                        break;
                    default:
                        System.out.println("Opção inválida");
                        break;
                }

            }while(i==0);
        }
        catch (InputMismatchException e){
            System.out.println(e.getMessage());
        }
    }

    //método que printa uma mensagem com o estado em que uma determinada encomenda se encontra
    public void EstadoEncomenda(){
        Scanner s=new Scanner(System.in);
        try {
            System.out.println("Digite o código de encomenda para ver o seu estado");
            String opcao = s.nextLine();
            b_dados.estadodeEncomenda(opcao);
        }
        catch (EncomendaNaoExisteException e){
            System.out.println(e.getMessage());
        }
    }

    //menu para um cliente classificar uma transportadora/voluntário
    public void menuClassifica(ArrayList<String> encs,String username){
        Scanner s=new Scanner(System.in);
        do{
            b_dados.displaypraClassificar(encs);
            System.out.println("Digite o código de transportadora/voluntário que deseja classificar:");
            System.out.println("0 - Retroceder");
            String opcao=s.nextLine();
            if(opcao.equals("0")){
                break;
            }
            for(String cod: encs){
                if(b_dados.getHistorico().get(cod).getCod().equals(opcao)){
                    System.out.println("Escolha a classificação");
                    System.out.println("1 - ★");
                    System.out.println("2 - ★★");
                    System.out.println("3 - ★★★");
                    System.out.println("4 - ★★★★");
                    System.out.println("5 - ★★★★★");
                    String op=s.nextLine();
                    int i=0;
                    while(i==0) {
                        try{
                            switch (op) {
                                case "1": {
                                    Classificacao c = new Classificacao(1.0);
                                    if(opcao.charAt(0)=='t') {
                                        b_dados.classifTrans(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    else if(opcao.charAt(0)=='v'){
                                        b_dados.classifVol(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    i=1;
                                    break;
                                }
                                case "2": {
                                    Classificacao c = new Classificacao(2.0);
                                    if(opcao.charAt(0)=='t'){
                                        b_dados.classifTrans(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    else if(opcao.charAt(0)=='v'){
                                        b_dados.classifVol(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    i=1;
                                    break;
                                }
                                case "3": {
                                    Classificacao c = new Classificacao(3.0);
                                    if(opcao.charAt(0)=='t'){
                                        b_dados.classifTrans(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    else if(opcao.charAt(0)=='v'){
                                        b_dados.classifVol(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    i=1;
                                    break;
                                }
                                case "4": {
                                    Classificacao c = new Classificacao(4.0);
                                    if(opcao.charAt(0)=='t'){
                                        b_dados.classifTrans(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    else if(opcao.charAt(0)=='v'){
                                        b_dados.classifVol(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    i=1;
                                    break;
                                }
                                case "5": {
                                    Classificacao c = new Classificacao(5.0);
                                    if(opcao.charAt(0)=='t'){
                                        b_dados.classifTrans(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    else if(opcao.charAt(0)=='v'){
                                        b_dados.classifVol(opcao, c);
                                        b_dados.removeClassifica(cod);
                                    }
                                    i=1;
                                    break;
                                }
                                default:
                                    System.out.println("Opção Invávlida!");
                                    break;
                            }
                        }
                        catch (TransportadoraNaoExisteException e){
                            System.out.println(e.getMessage());
                        }
                        catch (VoluntarioNaoExisteException e){
                            System.out.println(e.getMessage());
                        }
                        catch (InputMismatchException e){
                            System.out.println(e.getMessage());
                        }
                    }
                    break;
                }
            }
            encs=b_dados.buscapraClassificar(username);
        }while(!encs.isEmpty());
    }

    //menu para um voluntário escolher a encomenda que decide entregar
    public void menuEncomendasVol(String username){
        ArrayList<Encomenda> encs=b_dados.buscaEncomendaVoluntario(username);
        if(encs.isEmpty()){
            System.out.println("Não existem encomendas disponiveis na sua localização");
        }
        else{
            Scanner s=new Scanner(System.in);
            b_dados.buscaEncomendasDisplay(encs);
            int check=0;
            while(check==0) {
                System.out.println("Digite o código de encomenda que escolheu entregar");
                System.out.println("0 - Retroceder");
                String opcao = s.nextLine();
                if(opcao.equals("0"))
                    break;
                for (Encomenda e : encs) {
                    if (e.getcodEncomenda().equals(opcao)) {
                        String cod = b_dados.getVoluntarios().get(username).getCodVoluntario();
                        String nome = b_dados.getVoluntarios().get(username).getNome();
                        double kms=b_dados.getVoluntarios().get(username).getGps().distLocalizacao(b_dados.getLoja(e.getcodLoja()).getGps())+ b_dados.getVoluntarios().get(username).getGps().distLocalizacao(b_dados.getUtilizador(e.getcodUtilizador()).getGps());
                        b_dados.novaEntrega(cod, nome, e.getcodEncomenda(), e.getcodUtilizador(), e.getcodLoja(), e.getPeso(), e.getLinhas(),kms);
                        b_dados.removeEncomendaDisponivel(e);
                        b_dados.getVoluntarios().get(username).setDisponibilidade(false);
                        check=1;
                        break;
                    }
                }
                if(check==0){
                    System.out.println("Código errado ou não existe!");
                }
            }
        }
    }

    //menu para uma transportadora sinalizar que foi entregar uma encomenda
    public void menuEncomendasTrans(String username){
        ArrayList<Encomenda> encs=b_dados.buscaEncomendasTransportadora(username);
        if(encs.isEmpty()){
            System.out.println("Não existem encomendas disponiveis na sua localização");
        }
        else{
            Scanner s=new Scanner(System.in);
            while(!encs.isEmpty()) {
                b_dados.buscaEncomendasDisplay(encs);
                System.out.println("Digite o código de encomenda que a sua empresa está pronta a entregar");
                String opcao = s.nextLine();
                int check=0;
                for (Encomenda e : encs) {
                    if (e.getcodEncomenda().equals(opcao)) {
                        String cod = b_dados.getTrasnportadoras().get(username).getCodEmpresa();
                        String nome = b_dados.getTrasnportadoras().get(username).getNome();
                        double kms=b_dados.getTrasnportadoras().get(username).getGps().distLocalizacao(b_dados.getLoja(e.getcodLoja()).getGps())+ b_dados.getTrasnportadoras().get(username).getGps().distLocalizacao(b_dados.getUtilizador(e.getcodUtilizador()).getGps());
                        b_dados.novaEntrega(cod, nome, e.getcodEncomenda(), e.getcodUtilizador(), e.getcodLoja(), e.getPeso(), e.getLinhas(),kms);
                        b_dados.removeEncomendaDisponivel(e);
                        check=1;
                        encs=b_dados.buscaEncomendasTransportadora(username);
                        break;
                    }
                }
                if(check==0){
                    System.out.println("Código errado ou não existe!");
                }
            }
            System.out.println("Não existem mais encomendas disponiveis!");
        }
    }

    //menu para uma transportadora sinalizar que uma encomenda foi entregue
    public void menuEntregueTrans(String username){
        Scanner s= new Scanner(System.in);
        ArrayList<Historico> encs=b_dados.buscaEncomendaEntregueTrans(username);
        if (encs.isEmpty()){
            System.out.println("Não existem encomendas por entregar.");
        }
        else {
            do {
                b_dados.buscaHistoricoDisplay(encs);
                System.out.println("0 - Retroceder");
                System.out.println("Digite o código de encomenda que deseja sinalizar como entregue:");
                String opcao = s.nextLine();
                if(opcao.equals("0"))
                    break;
                int check=0;
                for(Historico h:encs){
                    if(h.getcodEncomenda().equals(opcao)){
                        h.setDate();
                        b_dados.addHistorico(h);
                        b_dados.removeEntrega(h);
                        b_dados.addClassifica(h.getcodEncomenda());
                        check=1;
                        break;
                    }
                }
                if(check==0)
                    System.out.println("Código inválido ou não exite.");
                encs=b_dados.buscaEncomendaEntregueTrans(username);
            }while(!encs.isEmpty());
            if (encs.isEmpty())
                System.out.println("Não existem mais encomendas por entregar");

        }
    }

    //menu para um voluntário sinalizar que uma encomenda foi entregue
    public void menuEntregueVol(String username){
        Scanner s=new Scanner(System.in);
        Historico h=b_dados.buscaEncomendaEntregueVol(username);
        if(h.equals(new Historico()))
            System.out.println("Não existe nenhuma encomenda a ser entregue");

        else{
            do{
                System.out.println(h.toString());
                System.out.println("Deseja confirmar que esta encomenda foi entregue?");
                System.out.println("1 - Confirmar");
                System.out.println("0 - Retroceder");
                String opcao=s.nextLine();
                if(opcao.equals("0")){
                    break;
                }
                else if(opcao.equals("1")){
                    h.setDate();
                    b_dados.addHistorico(h);
                    b_dados.removeEntrega(h);
                    b_dados.getVoluntarios().get(username).setDisponibilidade(true);
                    break;
                }
                else{
                    System.out.println("Opção invalida!");
                }
            }while(true);
        }
    }

    //método para guardar uma classe num ficheiro
    public void guardaEstado(String filename){
        try {
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(filename));
            out.writeObject(b_dados);
            out.close();
        } catch (FileNotFoundException e) {
            System.out.println(e.getMessage());
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }

    //método para ler um estado e carregá-lo
    public BaseDados lerEstado(String filename){
        try{
            ObjectInputStream in= new ObjectInputStream(new FileInputStream(filename));
            BaseDados b= (BaseDados) in.readObject();
            in.close();
            return b;
        } catch (FileNotFoundException e) {
            System.out.println(e.getMessage());
        } catch (IOException e) {
            System.out.println(e.getMessage());
        } catch (ClassNotFoundException e) {
            System.out.println(e.getMessage());
        }
        return new BaseDados();
    }

    //menu que permite a uma loja alterar o seu catalogo de produtos
    public void menuAlterarProdutos(String username){
        Scanner s= new Scanner(System.in);
        CatalogoProdutos cp=b_dados.getLojas().get(username).getCatalogoProdutos();
        do {
            System.out.println("Escolha o que pretende fazer");
            System.out.println("1 - Adicionar um produto");
            System.out.println("2 - Atualizar um produto");
            System.out.println("3 - Remover um produto");
            System.out.println("0 - Retroceder");
            String opcao=s.nextLine();
            if(opcao.equals("0"))
                break;
            switch (opcao){
                case "1":
                    System.out.println("Escreva a descrição do produto");
                    String descricao=s.nextLine();
                    System.out.println("Introduza o preço do produto");
                    double preco=s.nextDouble();
                    System.out.println("Introduza a quantidade disponivel");
                    double stock=s.nextDouble();
                    Produto p = b_dados.novoProduto(username,descricao,preco,stock);
                    b_dados.addProduto(username,p);
                    break;
                case "2":
                    int i=0;
                    while(i==0){
                        LogLoja l=b_dados.getLojas().get(username);
                        System.out.println(l.getCatalogoProdutos().toString());
                        System.out.println("Escreva o código de produto que deseja atualizar:");
                        String codigo=s.nextLine();
                        if(b_dados.produtoExiste(username,codigo)) {
                            i = 1;
                            System.out.println("Introduza o novo stock do produto:");
                            double newstock = s.nextDouble();
                            b_dados.updatestock(username, newstock, codigo);
                        }

                        else
                            System.out.println("Esse Produto não existe tente de novo");
                    }
                    break;
                case "3":
                    int c=0;
                    while(c==0){
                        LogLoja l=b_dados.getLojas().get(username);
                        System.out.println(l.getCatalogoProdutos().toString());
                        System.out.println("Escreva o código de produto que deseja remover:");
                        String cod=s.nextLine();
                        if(b_dados.produtoExiste(username,cod)){
                            c=1;
                            Produto prod=b_dados.buscaProduto(username,cod);
                            b_dados.removeProduto(username,prod);
                        }
                        else
                            System.out.println("Esse Produto não existe tente de novo");
                    }
                    break;
                default:
                    System.out.println("Entrada inválida");
                    break;
            }
            System.out.println("O seu catálogo:");
            System.out.println(b_dados.getLojas().get(username).getCatalogoProdutos().toString());

        }while(true);
    }

    //menu que permite a uma transportadora ver o seu total fatorado num determinado dia
    public void menuprecoperiodo(String username) {
        Scanner s=new Scanner(System.in);
        LogTransportadora t=b_dados.getTrasnportadoras().get(username);
        double count=0;
        try {
            ArrayList<Historico> hist = b_dados.buscaHistoricoTransportadora(t.getCodEmpresa());
            System.out.println("Indique o ano:");
            int ano=s.nextInt();
            System.out.println("Indique o mês");
            int mes=s.nextInt();
            System.out.println("Indique o dia");
            int dia=s.nextInt();
            for(Historico h: hist){
                LocalDateTime date=h.getDate();
                if(date.getYear()==ano && date.getMonthValue()==mes && date.getDayOfMonth()==dia){
                    count+=h.getKmspercorridos()*t.getPrecokm();
                }
            }
            System.out.println("O total fatorado nesse dia foi de " + count +"€");
        }
        catch (TransportadoraNaoExisteException e){
            System.out.println(e.getMessage());
        }
    }
}