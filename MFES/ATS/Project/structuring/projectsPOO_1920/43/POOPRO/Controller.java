import Exceptions.*;
import Modelo.Encomendas.Encomenda;
import Modelo.Encomendas.LinhaEncomenda;
import Modelo.Produtos.Produto;
import Modelo.Utilizadores.Cliente;
import Modelo.Utilizadores.Loja;
import Modelo.Utilizadores.Transportadora;
import Modelo.Utilizadores.Voluntario;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class Controller {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */

    private Model model;
    private View view;

    /**
     * CONSTRUTOR VAZIO
     */

    public Controller() {
        this.model = null;
        this.view = null;
    }

    /**
     * GETTERS
     */

    public Model getModel() {
        return this.model;
    }

    public View getView() {
        return this.view;
    }

    /**
     * SETTERS
     */

    public void setModel(Model model) {
        this.model = model;
    }

    public void setView(View view) {
        this.view = view;
    }

    /**
     * MÉTODOS PÁGINA
     */
    /*
    public void populador() {

        try {
            this.model.registarCliente("A","A","A","A","123456777",1,1);
        } catch (ClienteInvalidoException e) {
            e.printStackTrace();
        } catch (ValorInvalidoException e) {
            e.printStackTrace();
        }

        try {
            this.model.registarLoja("B","B","B","B","123457777",2,2,true,0);
        } catch (LojaInvalidaException e) {
            e.printStackTrace();
        } catch (ValorInvalidoException e) {
            e.printStackTrace();
        }

        try {
            this.model.registarLoja("E","E","E","E","123467777",5,5,false,20);
        } catch (LojaInvalidaException e) {
            e.printStackTrace();
        } catch (ValorInvalidoException e) {
            e.printStackTrace();
        }

        try {
            this.model.registarTransportadora("C","C","C","C","123458777",3,3,50,2,10);
        } catch (ValorInvalidoException e) {
            e.printStackTrace();
        } catch (TransportadoraInvalidaException e) {
            e.printStackTrace();
        }

        try {
            this.model.registarVoluntario("D","D","D","D","123459777",4,4,15);
        } catch (ValorInvalidoException e) {
            e.printStackTrace();
        } catch (VoluntarioInvalidoException e) {
            e.printStackTrace();
        }


        Produto produto1 = new Produto("sdads",212,212);
        Produto produto2 = new Produto("sdadfdss",212,212);
        Produto produto3 = new Produto("sdadsfdsa",212,212);
        Produto produto4 = new Produto("sd22adsfa",21,22);
        Produto produto5 = new Produto("sdadfsadfs",212,212);
        Produto produto6 = new Produto("sdadsdafasffdss",212,212);
        Produto produto7 = new Produto("sdadsfdsafasfdsa",212,212);
        Produto produto8 = new Produto("sd22afdsafasdsfa",21,22);
        Produto produto9 = new Produto("sd22afdsafasdsfa",21,22);

        this.model.getLoja("B").adicionarProduto(produto1);
        this.model.getLoja("B").adicionarProduto(produto2);
        this.model.getLoja("B").adicionarProduto(produto3);
        this.model.getLoja("B").adicionarProduto(produto5);
        this.model.getLoja("B").adicionarProduto(produto6);
        this.model.getLoja("B").adicionarProduto(produto7);
        this.model.getLoja("B").adicionarProduto(produto8);

        this.model.getLoja("E").adicionarProduto(produto4);
        this.model.getLoja("E").adicionarProduto(produto9);

        Encomenda enc1 = new Encomenda("B","A");
        LinhaEncomenda linha1 = new LinhaEncomenda(produto1,2);
        enc1.adicionarLinhaEncomenda(linha1);

        Encomenda enc2 = new Encomenda("B","A");
        LinhaEncomenda linha2 = new LinhaEncomenda(produto2,2);
        enc2.adicionarLinhaEncomenda(linha2);

        Encomenda enc3 = new Encomenda("E","A");
        LinhaEncomenda linha3 = new LinhaEncomenda(produto4,3);
        enc3.adicionarLinhaEncomenda(linha3);

        this.model.getLoja("B").adicionarPedido(enc1);
        this.model.getLoja("B").adicionarPedido(enc2);

        this.model.getLoja("E").adicionarPedido(enc3);

    }


    private void carregaLog() {
        List<String> linhas;
        Scanner sc = new Scanner(System.in);
        this.view.imprimeLinhaCM("De qual ficheiro CSV pretende carregar a info?:");
        String file = sc.nextLine();

        try {
            linhas = Files.readAllLines(Paths.get(file));
            linhas.forEach(this::parseLine);
        } catch (IOException exc) {
            this.view.imprimeLinhaCM(exc.getMessage());
            return;
        }
        this.view.imprimeLinhaCM("Ficheiro CSV carregado com sucesso!");
        return;
    }

    */

    public void iniciarPaginaInicial() throws IOException, ClassNotFoundException {

        //populador();

        int opcao=0;
        boolean quit = false;
        while(!quit) {
            Scanner input = new Scanner(System.in);
            this.view.limparEcra();
            this.view.paginaInicial();
            try {
                opcao = input.nextInt();
            }
            catch(InputMismatchException e){
                System.out.println("ERRO! INTRODUZA UM INTEIRO");
                iniciarPaginaInicial();
            }
            switch (opcao) {
                case 1:
                    iniciarPaginaLogin();
                    break;
                case 2:
                    iniciarPaginaRegisto();
                    break;
                case 3:
                    iniciarPaginaQueries();
                    break;
                case 4:
                    this.view.imprimeLinhaSM("Insira o nome do ficheiro onde pretende guardar o estado:");
                    Scanner sc = new Scanner(System.in);
                    String f = sc.nextLine();
                    this.model.guardaEstado(f);
                    break;
                case 5:
                    this.view.imprimeLinhaSM("Insira o nome do ficheiro que pretende carregar:");
                    Scanner sci = new Scanner(System.in);
                    String fi = sci.nextLine();
                    setModel(this.model.carregaEstado(fi));
                    break;
                case 6:
                    logParser();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarPaginaLogin() {
        int opcao=0;
        boolean quit = false;
        while(!quit) {
            Scanner input = new Scanner(System.in);
            this.view.limparEcra();
            this.view.paginaLogin();
            try {
                opcao = input.nextInt();
            }
            catch(InputMismatchException e){
                System.out.println("ERRO! INTRODUZA UM INTEIRO");
                iniciarPaginaLogin();
            }
            switch (opcao) {
                case 1:
                    iniciarLoginCliente();
                    break;
                case 2:
                    iniciarLoginLoja();
                    break;
                case 3:
                    iniciarLoginTransportadora();
                    break;
                case 4:
                    iniciarLoginVoluntario();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarPaginaRegisto() {
        int opcao=0;
        boolean quit = false;
        while(!quit) {
            Scanner input = new Scanner(System.in);
            this.view.limparEcra();
            this.view.paginaRegisto();
            try {
                opcao = input.nextInt();
            }
            catch(InputMismatchException e){
                System.out.println("ERRO! INTRODUZA UM INTEIRO");
                iniciarPaginaRegisto();
            }
            switch (opcao) {
                case 1:
                    iniciarRegistoCliente();
                    break;
                case 2:
                    iniciarRegistoLoja();
                    break;
                case 3:
                    iniciarRegistoTransportadora();
                    break;
                case 4:
                    iniciarRegistoVoluntario();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarPaginaQueries() {
        int opcao=0;
        boolean quit = false;
        while(!quit) {
            Scanner input = new Scanner(System.in);
            this.view.limparEcra();
            this.view.paginaQueries();
            try {
                opcao = input.nextInt();
            }
            catch(InputMismatchException e){
                System.out.println("ERRO! INTRODUZA UM INTEIRO");
                iniciarPaginaQueries();
            }            switch (opcao) {
                case 1:
                    iniciarQuerietotalFaturadoPeríodoTransportadora();
                    break;
                case 2:
                    iniciaQuerieempresasMaisUtilizadas();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    /**
     * MÉTODOS LOGIN
     */

    public void iniciarLoginCliente() {
        String cod, password;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaCM("*LOGIN*");
        this.view.imprimeLinhaSM("CODIGO DE UTILIZADOR: ");
        cod = sc.nextLine();
        this.view.imprimeLinhaSM("PASSWORD: ");
        password = sc.nextLine();

        try {
            this.model.loginCliente(cod,password);
        } catch (ClienteInvalidoException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("CLIENTE INVALIDO");
            return;
        } catch (PasswordInvalidaException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("PASSWORD INVALIDA");
            return;
        }

        iniciarMenuCliente();

        iniciarTerminarSessao();
    }

    public void iniciarLoginLoja() {
        String cod, password;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaCM("*LOGIN*");
        this.view.imprimeLinhaSM("CODIGO DE UTILIZADOR: ");
        cod = sc.nextLine();
        this.view.imprimeLinhaSM("PASSWORD: ");
        password = sc.nextLine();

        try {
            this.model.loginLoja(cod,password);
        } catch (LojaInvalidaException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("LOJA INVALIDA");
            return;
        } catch (PasswordInvalidaException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("PASSWORD INVALIDA");
            return;
        }

        iniciarMenuLoja();

        iniciarTerminarSessao();
    }

    public void iniciarLoginTransportadora() {
        String cod, password;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaCM("*LOGIN*");
        this.view.imprimeLinhaSM("CODIGO DE UTILIZADOR: ");
        cod = sc.nextLine();
        this.view.imprimeLinhaSM("PASSWORD: ");
        password = sc.nextLine();

        try {
            this.model.loginTransportadora(cod,password);
        } catch (TransportadoraInvalidaException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("TRANSPORTADORA INVALIDO");
            return;
        } catch (PasswordInvalidaException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("PASSWORD INVALIDA");
            return;
        }

        iniciarMenuTransportadora();

        iniciarTerminarSessao();
    }

    public void iniciarLoginVoluntario() {
        String cod, password;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaCM("*LOGIN*");
        this.view.imprimeLinhaSM("CODIGO DE UTILIZADOR: ");
        cod = sc.nextLine();
        this.view.imprimeLinhaSM("PASSWORD: ");
        password = sc.nextLine();

        try {
            this.model.loginVoluntario(cod,password);
        } catch (VoluntarioInvalidoException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("VOLUNTARIO INVALIDO");
            return;
        } catch (PasswordInvalidaException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("PASSWORD INVALIDA");
            return;
        }

        iniciarMenuVoluntario();

        iniciarTerminarSessao();
    }

    /**
     * MÉTODOS REGISTAR
     */

    private void iniciarRegistoCliente() {
        String nome, mail, password, codUtilizador, nif;
        double latitude, longitude;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaCM("*REGISTO*");
        this.view.imprimeLinhaSM("NOME: ");
        nome = sc.nextLine();
        this.view.imprimeLinhaSM("MAIL: ");
        mail = sc.nextLine();
        this.view.imprimeLinhaSM("PASSWORD: ");
        password = sc.nextLine();
        this.view.imprimeLinhaSM("CODIGO DE UTILIZADOR: ");
        codUtilizador = sc.nextLine();
        this.view.imprimeLinhaSM("NIF: ");
        nif = sc.nextLine();
        this.view.imprimeLinhaCM("*LOCALIZAÇÃO DA SUA CASA*");
        this.view.imprimeLinhaSM("LATITUDE: ");
        latitude = sc.nextDouble();
        this.view.imprimeLinhaSM("LONGITUDE: ");
        longitude = sc.nextDouble();

        try {
            this.model.registarCliente(nome, mail, password, codUtilizador, nif, latitude, longitude);
        } catch (ClienteInvalidoException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("CLIENTE INVALIDO");
            return;
        } catch (ValorInvalidoException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("VALOR INVALIDO");
            return;
        }
    }

    private void iniciarRegistoLoja() {
        String nome, mail, password, codUtilizador,nif;
        boolean temFila = false;
        int op, tempoMedio = 0;
        double latitude, longitude;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaCM("*REGISTO*");
        this.view.imprimeLinhaSM("NOME: ");
        nome = sc.nextLine();
        this.view.imprimeLinhaSM("MAIL: ");
        mail = sc.nextLine();
        this.view.imprimeLinhaSM("PASSWORD: ");
        password = sc.nextLine();
        this.view.imprimeLinhaSM("CODIGO DE UTILIZADOR: ");
        codUtilizador = sc.nextLine();
        this.view.imprimeLinhaSM("NIF: ");
        nif = sc.nextLine();
        this.view.imprimeLinhaCM("*LOCALIZAÇÃO DA SUA LOJA*");
        this.view.imprimeLinhaSM("LATITUDE: ");
        latitude = sc.nextDouble();
        this.view.imprimeLinhaSM("LONGITUDE: ");
        longitude = sc.nextDouble();
        this.view.imprimeLinhaCM("*QUER IMPLEMENTA SERVIÇO DE FILA DE ESPERA?*");
        this.view.imprimeLinhaSM("*[1] SIM | [0] NÃO *");
        do {
            op = sc.nextInt();
        } while(op != 1 && op != 0);
        if(op==1) {
            temFila = true;
        } else {
            temFila = false;
            this.view.imprimeLinhaCM("*QUAL O TEMPO MEDIO?*");
            this.view.imprimeLinhaSM("TEMPO MEDIO: ");
            tempoMedio = sc.nextInt();
        }
        try {
            this.model.registarLoja(nome, mail, password, codUtilizador, nif, latitude, longitude, temFila, tempoMedio);
        } catch (LojaInvalidaException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("LOJA INVALIDA");
            return;
        } catch (ValorInvalidoException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("VALOR INVALIDO");
            return;
        }
    }

    private void iniciarRegistoTransportadora() {
        String nome, mail, password, codUtilizador, nif;
        double latitude, longitude,raioAcao, taxaKM;
        int nrTrabalhadores;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaCM("*REGISTO*");
        this.view.imprimeLinhaSM("NOME: ");
        nome = sc.nextLine();
        this.view.imprimeLinhaSM("MAIL: ");
        mail = sc.nextLine();
        this.view.imprimeLinhaSM("PASSWORD: ");
        password = sc.nextLine();
        this.view.imprimeLinhaSM("CODIGO DE UTILIZADOR: ");
        codUtilizador = sc.nextLine();
        this.view.imprimeLinhaSM("NIF: ");
        nif = sc.nextLine();
        this.view.imprimeLinhaCM("*LOCALIZAÇÃO DA SUA EMPRESA*");
        this.view.imprimeLinhaSM("LATITUDE: ");
        latitude = sc.nextDouble();
        this.view.imprimeLinhaSM("LONGITUDE: ");
        longitude = sc.nextDouble();
        this.view.imprimeLinhaCM("*VALORES*");
        this.view.imprimeLinhaSM("RAIO DE ACAO: ");
        raioAcao = sc.nextDouble();
        this.view.imprimeLinhaSM("TAXA POR KM: ");
        taxaKM = sc.nextDouble();
        this.view.imprimeLinhaSM("NR DE TRABALHADORES: ");
        nrTrabalhadores = sc.nextInt();
        try {
            this.model.registarTransportadora(nome, mail, password, codUtilizador, nif, latitude, longitude, raioAcao, taxaKM, nrTrabalhadores);
        } catch (TransportadoraInvalidaException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("TRANSPORTADORA INVALIDA");
            return;
        } catch (ValorInvalidoException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("VALOR INVALIDO");
            return;
        }
    }

    private void iniciarRegistoVoluntario() {
        String nome, mail, password, codUtilizador, nif;
        double latitude, longitude, raioAcao;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaCM("*REGISTO*");
        this.view.imprimeLinhaSM("NOME: ");
        nome = sc.nextLine();
        this.view.imprimeLinhaSM("MAIL: ");
        mail = sc.nextLine();
        this.view.imprimeLinhaSM("PASSWORD: ");
        password = sc.nextLine();
        this.view.imprimeLinhaSM("CODIGO DE UTILIZADOR: ");
        codUtilizador = sc.nextLine();
        this.view.imprimeLinhaSM("NIF: ");
        nif = sc.nextLine();
        this.view.imprimeLinhaCM("*SUA LOCALIZACÃO ATUAL*");
        this.view.imprimeLinhaSM("LATITUDE: ");
        latitude = sc.nextDouble();
        this.view.imprimeLinhaSM("LONGITUDE: ");
        longitude = sc.nextDouble();
        this.view.imprimeLinhaSM("RAIO DE ACAO: ");
        raioAcao = sc.nextDouble();
        try {
            this.model.registarVoluntario(nome, mail, password, codUtilizador, nif, latitude, longitude, raioAcao);
        } catch (VoluntarioInvalidoException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("VOLUNTARIO INVALIDO");
            return;
        } catch (ValorInvalidoException e) {
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("VALOR INVALIDO");
            return;
        }
    }

    /**
     * METODOS MENU
     */

    public void iniciarMenuCliente(){
        int opcao;
        boolean quit = false;
        Scanner sc = new Scanner(System.in);

        while(!quit) {
            this.view.limparEcra();
            this.view.menuCliente();
            opcao = sc.nextInt();
            switch (opcao) {
                case 1:
                    iniciarMenuCriarEncomenda();
                    break;
                case 2:
                    iniciarMenuListaPedidosC();
                case 3:
                    iniciarMenuListaEncomendasRegisto();
                    break;
                case 4:
                    iniciaAtualizaClassificacao();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuLoja() {
        int opcao;
        boolean quit = false;
        Scanner sc = new Scanner(System.in);

        while(!quit) {
            this.view.limparEcra();
            this.view.menuLoja();
            opcao = sc.nextInt();
            switch (opcao) {
                case 1:
                    iniciarMenuListaPedidosL();
                    break;
                case 2:
                    iniciarMenuListaPedidosAceitesL();
                    break;
                case 3:
                    iniciarMenuListaPedidosProntoL();
                    break;
                case 4:
                    iniciarMenuProdutosLoja();
                    break;
                case 5:
                    iniciarMenuListaEncomendasRegistoL();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuTransportadora() {
        int opcao;
        boolean quit = false;
        Scanner sc = new Scanner(System.in);

        while(!quit) {
            this.view.limparEcra();
            this.view.menuTransportadora();
            opcao = sc.nextInt();
            switch (opcao) {
                case 1:
                    iniciarMenuListaPedidosT();
                    break;
                case 2:
                    iniciarMenuListaPedidosAceitesT();
                    break;
                case 3:
                    iniciarMenuListaEncomendasRegistoT();
                    break;
                case 4:
                    iniciarClassificacaoAtualT();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuVoluntario() {
        int opcao;
        boolean quit = false;
        Scanner sc = new Scanner(System.in);

        while(!quit) {
            this.view.limparEcra();
            this.view.menuVoluntario();
            opcao = sc.nextInt();
            switch (opcao) {
                case 1:
                    iniciarMenuListaPedidosV();
                    break;
                case 2:
                    iniciarMenuListaPedidosAceitesV();
                    break;
                case 3:
                    iniciarMenuListaEncomendasRegistoT();
                    break;
                case 4:
                    iniciarClassificacaoAtualV();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    /**
     * MÉTODO LISTAR REGISTO
     */

    public void iniciarMenuListaEncomendasRegisto() {
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lEncomendas = new ArrayList<>(this.model.listaEncomendas());
        Listagem<Encomenda> lc = new Listagem<>(lEncomendas);

        while(!quit) {
            this.view.limparEcra();
            lc.mostrarLista(view);
            this.view.menuListaEncomendas();
            opcao = sc.nextInt();
            switch (opcao) {
                case 1:
                    lc.retrocedePagina();
                    break;
                case 2:
                    lc.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }

    }

    public void iniciarMenuListaEncomendasRegistoL() {
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lEncomendas = new ArrayList<>(this.model.listaEncomendasLojas());
        Listagem<Encomenda> lc = new Listagem<>(lEncomendas);

        while(!quit) {
            this.view.limparEcra();
            lc.mostrarLista(view);
            this.view.menuListaEncomendas();
            opcao = sc.nextInt();
            switch (opcao) {
                case 1:
                    lc.retrocedePagina();
                    break;
                case 2:
                    lc.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }

    }

    public void iniciarMenuListaEncomendasRegistoT() {
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lEncomendas = new ArrayList<>(this.model.listaEncomendasTransp());
        Listagem<Encomenda> lc = new Listagem<>(lEncomendas);

        while(!quit) {
            this.view.limparEcra();
            lc.mostrarLista(view);
            this.view.menuListaEncomendas();
            opcao = sc.nextInt();
            switch (opcao) {
                case 1:
                    lc.retrocedePagina();
                    break;
                case 2:
                    lc.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }

    }

    /**
     * MÉTODO MENU CRIAR ENCOMENDA
     */

    public void iniciarMenuCriarEncomenda() {
        String codLoja, codProduto;
        int quantidade;

        codLoja = iniciarListaLoja();

        if(codLoja != null ) {
            iniciarListaProdutos(codLoja);
        }

    }

    /**
     * MÉTODO LISTA LOJAS
     */

    public String iniciarListaLoja() {
        String codLoja = null;
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        if(this.model.getLojas().values().size() == 0) {
            this.view.imprimeLinhaCM("NÃO EXISTEM LOJAS");
            //return null;
        }
        List<Loja> lLojas = new ArrayList<>(this.model.getLojas().values());
        Listagem<Loja> lc = new Listagem<>(lLojas);

        while(!quit) {
            this.view.limparEcra();
            lc.mostrarLista(view);
            this.view.menuListaLojas();
            opcao = sc.nextInt();
            switch (opcao) {
                case 1:
                    codLoja = iniciarSelecionarLoja();
                    quit = true;
                    break;
                case 2:
                    lc.retrocedePagina();
                    break;
                case 3:
                    lc.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }

        return codLoja;
    }

    /**
     * MÉTODO SELECIONAR LOJAS
     */

    public String iniciarSelecionarLoja() {
        String codLoja = null;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaSM("SELECIONE O CODIGO DA LOJA: ");
        codLoja = sc.nextLine();

        return codLoja;
    }

    /**
     * MÉTODO LISTA PRODUTOS
     */

    public void iniciarListaProdutos(String codLoja) {
        String codProduto = null;
        int quantidade = 0;
        Map<String,Integer> valores = new HashMap<>();
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        Loja loja = this.model.getLoja(codLoja);
        if(loja.getProdutos().values().size() == 0) {
            this.view.imprimeLinhaCM("NÃO EXISTEM PRODUTOS");
            //return;
        }
        List<Produto> lProdutos = new ArrayList<>(loja.getProdutos().values());
        Listagem<Produto> lp = new Listagem<>(lProdutos);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuListaProdutos();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    this.view.imprimeLinhaSM("SELECIONE O CODIGO DO PRODUTO: ");
                    codProduto = sc.nextLine();
                    this.view.imprimeLinhaSM("SELECIONE A QUANTIDADE: ");
                    quantidade = sc.nextInt();
                    valores.put(codProduto,quantidade);
                    sc.nextLine();
                    //quit = true;
                    break;
                case 2:
                    try {
                        this.model.criarEncomenda(codLoja,valores);
                    } catch (ValorInvalidoException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("VALOR INVALIDO");
                        return;
                    } catch (LojaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("LOJA INVALIDA");
                        return;
                    } catch (ProdutoInvalidoException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("PRODUTO INVALIDO");
                        return;
                    }
                    quit = true;
                    break;
                case 3:
                    lp.retrocedePagina();
                    break;
                case 4:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }


    /**
     * MÉTODO PRODUTOS LOJA
     */

    public void iniciarMenuProdutosLoja() {
        String descri;
        double valorUnitario, pesoUnitario;
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        Loja loja = this.model.getLoja(this.model.getUtilizador().getCodUtilizador());
        if(loja.getProdutos().values().size() == 0) {
            this.view.imprimeLinhaCM("NÃO EXISTEM PRODUTOS");
            //return;
        }
        List<Produto> lProdutos = new ArrayList<>(loja.getProdutos().values());
        Listagem<Produto> lp = new Listagem<>(lProdutos);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuProdutosLoja();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    this.view.imprimeLinhaSM("DESCRICAO DO PRODUTO:");
                    descri = sc.nextLine();
                    this.view.imprimeLinhaSM("VALOR UNITARIO: ");
                    valorUnitario = Double.parseDouble(sc.nextLine());
                    this.view.imprimeLinhaSM("PESO UNITARIO: ");
                    pesoUnitario = Double.parseDouble(sc.nextLine());
                    try {
                        this.model.adicionarProduto(descri,valorUnitario,pesoUnitario);
                    } catch (ValorInvalidoException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("VALOR INVALIDO");
                        return;
                    }
                    quit = true;
                    break;
                case 2:
                    lp.retrocedePagina();
                    break;
                case 3:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    /**
     * MÉTODO TERMINAR SESSÃO
     */

    public void iniciarTerminarSessao() {
        this.view.imprimeLinhaCM("SESSAO TERMINADA");
        this.model.terminarSessao();
    }

    /**
     * MÉTODO LISTA PEDIDOS
     */

    public void iniciarMenuListaPedidosC() {
        String codEncomenda;
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lPedidos = new ArrayList<>(this.model.getUtilizador().getPedidos());
        Listagem<Encomenda> lp = new Listagem<>(lPedidos);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuPedidos();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    this.view.imprimeLinhaSM("CODIGO DA ENCOMENDA QUE PRETENDE ACEITAR: ");
                    codEncomenda = sc.nextLine();
                    try {
                        this.model.aceitaPedidoC(codEncomenda);
                    } catch (EncomendaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("ENCOMENDA INVALIDA");
                        return;
                    }
                    List<Encomenda> lPedidosU1 = new ArrayList<>(this.model.getUtilizador().getPedidos());
                    lp.atualizaClasse(lPedidosU1);
                    break;
                case 2:
                    this.view.imprimeLinhaSM("CODIGO DA ENCOMENDA QUE PRETENDE REJEITAR: ");
                    codEncomenda = sc.nextLine();
                    try {
                        this.model.rejeitaPedidoC(codEncomenda);
                    } catch (EncomendaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("ENCOMENDA INVALIDA");
                        return;
                    }
                    List<Encomenda> lPedidosU2 = new ArrayList<>(this.model.getUtilizador().getPedidos());
                    lp.atualizaClasse(lPedidosU2);
                    break;
                case 3:
                    lp.retrocedePagina();
                    break;
                case 4:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuListaPedidosL() {
        String codEncomenda;
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lPedidos = new ArrayList<>(this.model.getUtilizador().getPedidos());
        Listagem<Encomenda> lp = new Listagem<>(lPedidos);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuPedidos();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    this.view.imprimeLinhaSM("CODIGO DA ENCOMENDA QUE PRETENDE ACEITAR: ");
                    codEncomenda = sc.nextLine();
                    try {
                        this.model.aceitaPedidoL(codEncomenda);
                    } catch (EncomendaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("ENCOMENDA INVALIDA");
                        return;
                    }
                    List<Encomenda> lPedidosU1 = new ArrayList<>(this.model.getUtilizador().getPedidos());
                    lp.atualizaClasse(lPedidosU1);
                    break;
                case 2:
                    this.view.imprimeLinhaSM("CODIGO DA ENCOMENDA QUE PRETENDE REJEITAR: ");
                    codEncomenda = sc.nextLine();
                    try {
                        this.model.rejeitaPedidoL(codEncomenda);
                    } catch (EncomendaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("ENCOMENDA INVALIDA");
                        return;
                    }
                    List<Encomenda> lPedidosU2 = new ArrayList<>(this.model.getUtilizador().getPedidos());
                    lp.atualizaClasse(lPedidosU2);
                    break;
                case 3:
                    lp.retrocedePagina();
                    break;
                case 4:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuListaPedidosAceitesL() {
        String codEncomenda;
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lPedidosA = new ArrayList<>(this.model.getLoja(this.model.getUtilizador().getCodUtilizador()).getPedidosAceites());
        Listagem<Encomenda> lp = new Listagem<>(lPedidosA);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuPedidosAceites();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    this.view.imprimeLinhaSM("CODIGO DA ENCOMENDA PRONTA: ");
                    codEncomenda = sc.nextLine();
                    try {
                        this.model.pedidoProntoL(codEncomenda);
                    } catch (EncomendaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("ENCOMENDA INVALIDA");
                        return;
                    }
                    List<Encomenda> lPedidosAN = new ArrayList<>(this.model.getLoja(this.model.getUtilizador().getCodUtilizador()).getPedidosAceites());
                    lp.atualizaClasse(lPedidosAN);
                    break;
                case 2:
                    lp.retrocedePagina();
                    break;
                case 3:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuListaPedidosProntoL() {
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lPedidosP = new ArrayList<>(this.model.getLoja(this.model.getUtilizador().getCodUtilizador()).getPedidosProntos());
        Listagem<Encomenda> lp = new Listagem<>(lPedidosP);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuPedidosProntos();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    lp.retrocedePagina();
                    break;
                case 2:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuListaPedidosT() {
        String codEncomenda;
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lPedidos = new ArrayList<>(this.model.getPedidosAceitesLojas());
        Listagem<Encomenda> lp = new Listagem<>(lPedidos);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuPedidosTransportadora();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    this.view.imprimeLinhaSM("CODIGO DA ENCOMENDA QUE PRETENDE ENTREGAR: ");
                    codEncomenda = sc.nextLine();
                    try {
                        this.model.selecionaPedidoT(lPedidos,codEncomenda);
                    } catch (EncomendaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("ENCOMENDA INVALIDA");
                        return;
                    } catch (ValorInvalidoException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("VALOR INVALIDO");
                        return;
                    } catch (TransportadoraInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("TRANSPORTADORA INVALIDA");
                        return;
                    }
                case 2:
                    lp.retrocedePagina();
                    break;
                case 3:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuListaPedidosV() {
        String codEncomenda;
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lPedidos = new ArrayList<>(this.model.getPedidosAceitesLojas());
        Listagem<Encomenda> lp = new Listagem<>(lPedidos);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuPedidosTransportadora();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    this.view.imprimeLinhaSM("CODIGO DA ENCOMENDA QUE PRETENDE ENTREGAR: ");
                    codEncomenda = sc.nextLine();
                    try {
                        this.model.selecionaPedidoV(lPedidos,codEncomenda);
                    } catch (EncomendaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("ENCOMENDA INVALIDA");
                        return;
                    } catch (ValorInvalidoException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("VALOR INVALIDO");
                        return;
                    } catch (VoluntarioInvalidoException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("VOLUNTARIO INVALIDO");
                        return;
                    }
                case 2:
                    lp.retrocedePagina();
                    break;
                case 3:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuListaPedidosAceitesT() {
        String codEncomenda;
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lPedidosA = new ArrayList<>(this.model.getTransportadora(this.model.getUtilizador().getCodUtilizador()).getPedidosAceites());
        Listagem<Encomenda> lp = new Listagem<>(lPedidosA);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuPedidosAceites();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    this.view.imprimeLinhaSM("CODIGO DA ENCOMENDA PRONTA: ");
                    codEncomenda = sc.nextLine();
                    try {
                        this.model.pedidoEntregueT(codEncomenda);
                        this.view.imprimeLinhaCM("PEDIDO ENTREGUE");
                    } catch (EncomendaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("ENCOMENDA INVALIDA");
                        return;
                    }
                    List<Encomenda> lPedidosAN = new ArrayList<>(this.model.getTransportadora(this.model.getUtilizador().getCodUtilizador()).getPedidosAceites());
                    lp.atualizaClasse(lPedidosAN);
                    break;
                case 2:
                    lp.retrocedePagina();
                    break;
                case 3:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarMenuListaPedidosAceitesV() {
        String codEncomenda;
        boolean quit = false;
        int opcao;
        Scanner sc = new Scanner(System.in);
        List<Encomenda> lPedidosA = new ArrayList<>(this.model.getVoluntarios(this.model.getUtilizador().getCodUtilizador()).getPedidos());
        Listagem<Encomenda> lp = new Listagem<>(lPedidosA);

        while(!quit) {
            this.view.limparEcra();
            lp.mostrarLista(view);
            this.view.menuPedidosAceites();
            opcao = Integer.parseInt(sc.nextLine());
            switch (opcao) {
                case 1:
                    this.view.imprimeLinhaSM("CODIGO DA ENCOMENDA PRONTA: ");
                    codEncomenda = sc.nextLine();
                    try {
                        this.model.pedidoEntregueV(codEncomenda);
                        this.view.imprimeLinhaCM("PEDIDO ENTREGUE");
                    } catch (EncomendaInvalidaException e) {
                        this.view.imprimeLinhaCM("");
                        this.view.imprimeLinhaCM("ENCOMENDA INVALIDA");
                        return;
                    }
                    List<Encomenda> lPedidosAN = new ArrayList<>(this.model.getVoluntarios(this.model.getUtilizador().getCodUtilizador()).getPedidos());
                    lp.atualizaClasse(lPedidosAN);
                    break;
                case 2:
                    lp.retrocedePagina();
                    break;
                case 3:
                    lp.proximaPagina();
                    break;
                case 0:
                    quit = true;
                    break;
                default:
                    this.view.imprimeLinhaCM("OPÇÃO INVÁLIDA");
                    break;
            }
        }
    }

    public void iniciarQuerietotalFaturadoPeríodoTransportadora(){
        String cod, inicio, fim;
        Scanner sc = new Scanner(System.in);
        LocalDate data_inicio;
        LocalDate data_fim;
        double totalFaturado = 0;

        this.view.imprimeLinhaSM("*INSIRA O CÓDIGO DA EMPRESA:");
        cod = sc.nextLine();
        this.view.imprimeLinhaSM("INSIRA A DATA DE INÍCIO NO FORMATO DD-MM-YYYY:");
        inicio = sc.nextLine();
        this.view.imprimeLinhaSM("INSIRA A DATA DE FIM NO FORMATO DD-MM-YYYY:");
        fim = sc.nextLine();

        //formatar datas
        DateTimeFormatter formato = DateTimeFormatter.ofPattern("dd-MM-yyyy");
        try {
            data_inicio = LocalDate.parse(inicio, formato);
            data_fim = LocalDate.parse(fim, formato);
        } catch (java.time.format.DateTimeParseException e){
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("DATA NUM FORMATO INVÁLIDO");
            return;
        }

        try {
            totalFaturado = this.model.totalFaturadoPeríodoTransportadora(cod, data_inicio, data_fim);
        }catch ( java.lang.NullPointerException e){
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("TRANSPORTADORA INVÁLIDA");
            return;
        }

        this.view.imprimeLinhaCM("");
        this.view.imprimeLinhaCM("O total faturado no período de " + inicio + " a " + fim + " foi de " + totalFaturado);


        iniciarPaginaQueries();

    }

    public void iniciaQuerieempresasMaisUtilizadas(){
        List<String> res = new ArrayList<>();


        for(int i =0; i<10;i++){

            res.set(i,this.model.empresasMaisUtilizadas().get(i));
        }

        for(int i =0; i<res.size();i++){

            this.view.imprimeLinhaSM(res.get(i).toString());
        }
        iniciarPaginaQueries();
    }


    public void iniciaAtualizaClassificacao(){
        String cod;
        int classi;
        Scanner sc = new Scanner(System.in);

        this.view.imprimeLinhaSM("*INSIRA O CÓDIGO DA ENCOMENDA QUE PRETENDE CLASSIFICAR:");
        cod = sc.nextLine();
        this.view.imprimeLinhaSM("INSIRA A CLASSIFICAÇÃO QUE PRETENDE ENTRE 1 E 5");
        classi = sc.nextInt();

        if((classi==1 || classi==2 || classi==3 || classi==4 || classi==5)) {

           try {
               this.model.atualizaClassificacao(cod, classi);
           }
           catch (TransporteInvalidoException e) {
               this.view.imprimeLinhaCM("");
               this.view.imprimeLinhaCM("TRANSPORTE INVÁLIDA");
               return;
            }
           catch (EncomendaInvalidaException e) {
               this.view.imprimeLinhaCM("");
               this.view.imprimeLinhaCM("ENCOMENDA00 INVÁLIDA");
               return;
           }
        }
        else{
            this.view.imprimeLinhaCM("");
            this.view.imprimeLinhaCM("NOTA INVÁLIDA");
            return;
        }
    }

    public void iniciarClassificacaoAtualV(){
        this.view.imprimeLinhaCM("");
        this.view.imprimeLinhaCM(this.model.notaV());
    }

    public void iniciarClassificacaoAtualT(){
        this.view.imprimeLinhaCM("");
        this.view.imprimeLinhaCM(this.model.notaT());
    }


    /**
     * MÉTODOS DE PARSING
     */

    public StringBuilder geradorRandom() {
        String AlphaNumericString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "0123456789" + "abcdefghijklmnopqrstuvxyz";
        StringBuilder sb = new StringBuilder(5);
        for (int i = 0; i < 5; i++) {
            int index = (int)(AlphaNumericString.length() * Math.random());
            sb.append(AlphaNumericString.charAt(index));
        }
        return sb;
    }

    public StringBuilder nifRandom() {
        String AlphaNumericString = "0123456789";
        StringBuilder sb = new StringBuilder(9);
        for (int i = 0; i < 9; i++) {
            int index = (int)(AlphaNumericString.length() * Math.random());
            sb.append(AlphaNumericString.charAt(index));
        }
        return sb;
    }

    private static int getRandomNumberInRange(int min, int max) {

        if (min >= max) {
            throw new IllegalArgumentException("max must be greater than min");
        }

        Random r = new Random();
        return r.nextInt((max - min) + 1) + min;
    }

    public void logParser(){
        List<String> linhas = lerFicheiro("logs.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    parseCliente(linhaPartida[1]); // criar um Utilizador
                    break;
                case "Loja":
                    parseLoja(linhaPartida[1]);
                    break;
                case "Voluntario":
                    parseVoluntario(linhaPartida[1]); // criar um Utilizador
                    break;
                case "Transportadora":
                    parseTransportadora(linhaPartida[1]);
                    break;
                case "Encomenda":
                    parseEncomenda(linhaPartida[1]);
                    break;
                case "Aceite":
                    parseAceite(linhaPartida[1]);
                    break;

                default:
                    this.view.imprimeLinhaCM("LINHA INVALIDA");
                    break;
            }
        }
        this.view.imprimeLinhaCM("done!");
    }

    public void parseCliente(String input){

        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double longitude = Double.parseDouble(campos[2]);
        double latitude = Double.parseDouble(campos[3]);

        if(latitude>90) {
            latitude = 90;
        }
        if(latitude<-90) {
            latitude = -90;
        }
        if(longitude>180) {
            longitude = 180;
        }
        if(longitude<-180) {
            longitude = -180;
        }

        StringBuilder mail = geradorRandom();
        StringBuilder pass = geradorRandom();
        StringBuilder snif = nifRandom();
        mail.append("@gmail.com");
        String email = mail.toString();
        String pwd = pass.toString();
        String nif = snif.toString();

        try {
            this.model.registarCliente(nome,email,pwd,codUtilizador,nif,latitude,longitude);
        } catch (ClienteInvalidoException e) {
            e.printStackTrace();
        } catch (ValorInvalidoException e) {
            e.printStackTrace();
        }

        Cliente cliente = new Cliente(nome,email,pwd,codUtilizador,nif,latitude,longitude);
        System.out.println(cliente);
    }

    public void parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        Boolean temFilaespera = true;
        int tempoEspera = 0;
        int numeroFila = 0;
        int tempoMedio = 0;
        double longitude = Double.parseDouble(campos[2]);
        double latitude = Double.parseDouble(campos[3]);

        if(latitude>90) {
            latitude = 90;
        }
        if(latitude<-90) {
            latitude = -90;
        }
        if(longitude>180) {
            longitude = 180;
        }
        if(longitude<-180) {
            longitude = -180;
        }

        StringBuilder mail = geradorRandom();
        StringBuilder pass = geradorRandom();
        StringBuilder snif = nifRandom();
        mail.append("@gmail.com");
        String email = mail.toString();
        String pwd = pass.toString();
        String nif = snif.toString();

        try {
            this.model.registarLoja(nomeLoja,email,pwd,codLoja,nif,latitude,longitude,temFilaespera,tempoMedio);
        } catch (LojaInvalidaException e) {
            e.printStackTrace();
        } catch (ValorInvalidoException e) {
            e.printStackTrace();
        }

        Loja loja = new Loja(nomeLoja,email,pwd,codLoja,nif,latitude,longitude,temFilaespera,tempoMedio);
        System.out.println(loja);
    }

    public void parseVoluntario(String input){
        String[] campos = input.split(",");
        String codV  = campos[0];
        String nome = campos[1];
        double longitude = Double.parseDouble(campos[2]);
        double latitude = Double.parseDouble(campos[3]);
        //double raioAcao = Double.parseDouble(campos[4]);
        double raioAcao = 999999999999999999999.0;

        if(latitude>90) {
            latitude = 90;
        }
        if(latitude<-90) {
            latitude = -90;
        }
        if(longitude>180) {
            longitude = 180;
        }
        if(longitude<-180) {
            longitude = -180;
        }

        StringBuilder mail = geradorRandom();
        StringBuilder pass = geradorRandom();
        StringBuilder snif = nifRandom();
        mail.append("@gmail.com");
        String email = mail.toString();
        String pwd = pass.toString();
        String nif = snif.toString();

        try {
            this.model.registarVoluntario(nome,email,pwd,codV,nif,latitude,longitude,raioAcao);
        } catch (VoluntarioInvalidoException e) {
            e.printStackTrace();
        } catch (ValorInvalidoException e) {
            e.printStackTrace();
        }

        Voluntario voluntario = new Voluntario(nome,email,pwd,codV,nif,latitude,longitude,raioAcao);
        System.out.println(voluntario);

    }

    public void parseTransportadora(String input){
        String[] campos = input.split(",");
        String codT  = campos[0];
        String nome = campos[1];
        double longitude = Double.parseDouble(campos[2]);
        double latitude = Double.parseDouble(campos[3]);
        String nif = campos[4];
        //double raioAcao = Double.parseDouble(campos[5]);
        double raioAcao = 999999999999999999999.0;
        double taxaKM = Double.parseDouble(campos[6]);

        if(latitude>90) {
            latitude = 90;
        }
        if(latitude<-90) {
            latitude = -90;
        }
        if(longitude>180) {
            longitude = 180;
        }
        if(longitude<-180) {
            longitude = -180;
        }

        StringBuilder mail = geradorRandom();
        StringBuilder pass = geradorRandom();
        mail.append("@gmail.com");
        String email = mail.toString();
        String pwd = pass.toString();

        int nrTrabalhadores = getRandomNumberInRange(10,50);

        try {
            this.model.registarTransportadora(nome,email,pwd,codT,nif,latitude,longitude,raioAcao,taxaKM,nrTrabalhadores);
        } catch (TransportadoraInvalidaException e) {
            e.printStackTrace();
        } catch (ValorInvalidoException e) {
            e.printStackTrace();
        }

        Transportadora transportadora = new Transportadora(nome,email,pwd,codT,nif,latitude,longitude,raioAcao,taxaKM,nrTrabalhadores);
        System.out.println(transportadora);
    }

    public void parseEncomenda(String input) {
        String[] campos = input.split(",");
        String codE = campos[0];
        String codUt = campos[1];
        String codLoja = campos[2];
        Double peso = Double.parseDouble(campos[3]);
        Double precoLoja = 0.0;
        Double precoTrans = 0.0;
        Double precoTotal = 0.0;
        int quantidade = 0;
        Map<String, LinhaEncomenda> produtosE = new HashMap<>();
        Encomenda nEncomenda = new Encomenda(codE,codLoja,codUt);

        for (int i = 4; campos.length > i+4; i += 4) {
            Produto p = new Produto();
            p.setCodProduto(campos[i]);
            p.setDescricao(campos[i + 1]);
            p.setPesoUnitario(peso);
            quantidade = (int)Double.parseDouble(campos[i + 2]);
            p.setValorUnitario(Double.parseDouble(campos[i + 3]));
            this.model.getLoja(codLoja).adicionarProduto(p);
            LinhaEncomenda l = new LinhaEncomenda(p,quantidade);
            produtosE.put(p.getCodProduto(), l);
            nEncomenda.adicionarLinhaEncomenda(l);
            nEncomenda.atualizarPesoTotal(p.getPesoUnitario()*quantidade);
            nEncomenda.atualizarPrecoLoja(p.getValorUnitario()*quantidade);
            System.out.println(p);
        }

        this.model.getLoja(codLoja).adicionarPedido(nEncomenda);

        System.out.println(nEncomenda);
    }

    public void parseAceite(String input){
        String[] campos = input.split(",");
        String codE  = campos[0];
        //Loja loja = null;
        //Encomenda enc = null;
        for(Map.Entry<String,Loja> l : this.model.getLojas().entrySet()) {
            for(Encomenda e: l.getValue().getPedidos()) {
                if(e.getCodEncomenda().matches(codE)) {
                    System.out.println("LOJA:" + l.getValue());
                    System.out.println("ACEITE:" + e);
                    this.model.getLoja(l.getValue().getCodUtilizador()).removerPedido(e);
                    this.model.getLoja(l.getValue().getCodUtilizador()).adicionarPedidoAceite(e);
                    this.model.getLoja(l.getValue().getCodUtilizador()).adicionarQueue();
                }
            }
        }
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
}
