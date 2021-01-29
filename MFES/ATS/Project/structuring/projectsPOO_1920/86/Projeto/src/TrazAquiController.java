import java.io.IOException;
import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.regex.PatternSyntaxException;
import java.util.stream.Collectors;


/**
 * Classe responsavel por fazer e ligação entre o Model e o view e tambem responsável por fazer a iteração com o utilizador.
 */
public class TrazAquiController implements ITrazAquiController {
    private TrazAquiModel model;
    private TrazAquiView view;
    private Menu menu;

    /**
     * Construtor não parametrizado que inicia o controller no menu inicial.
     */
    public TrazAquiController() {
        this.menu = Menu.Inicial;
    }

    /**
     * Setter do model.
     */
    public void setModel(TrazAquiModel model) {
        this.model = model;
    }

    /**
     * Setter do view.
     */
    public void setView(TrazAquiView view) {
        this.view = view;
    }

    /**
     * Método privado que retorna o número de un código de loja.
     */
    private int parseLoja(String l) {
        String[] r = l.split("l");
        return Integer.parseInt(r[1]);
    }

    /**
     * Método privado que retorna o número de un código de produto.
     */
    private int parsePrd(String l) {
        String[] r = l.split("p");
        return Integer.parseInt(r[1]);
    }

    /**
     * Método privado que retorna o número de un código de encomenda.
     */
    private int parseEnc(String l) {
        String[] r = l.split("e");
        return Integer.parseInt(r[1]);
    }

    /**
     * Método responsável por criar uma nova conta para o sistema com os inputs feitos pelo utilizador.
     */
    public void registaConta() {
        this.view.askForPrint(ITrazAquiView.Print.SignUp, "");
        int signup = Input.lerInt();
        switch (signup) {
            case 1:
                view.askForPrint(ITrazAquiView.Print.NomeUt, "");
                String ut = Input.lerString();
                if (ut.charAt(0) != 'u') {
                    view.askForPrint(ITrazAquiView.Print.Invalid, "");
                    break;
                }
                view.askForPrint(ITrazAquiView.Print.Nome, "");
                String nome = Input.lerString();
                view.askForPrint(ITrazAquiView.Print.Pass, "");
                String pass = Input.lerString();
                view.askForPrint(ITrazAquiView.Print.Coordenada, "x");
                double x = Input.lerDouble();
                view.askForPrint(ITrazAquiView.Print.Coordenada, "y");
                double y = Input.lerDouble();
                try {
                    this.model.criaUtilizador(ut, nome, x, y, pass);
                } catch (DuplicateUserException e) {
                    this.view.askForPrint(ITrazAquiView.Print.Erro, e.getMessage());
                }
                this.menu = Menu.Inicial;
                break;
            case 2:
                view.askForPrint(ITrazAquiView.Print.NomeUt, "");
                String vt = Input.lerString();
                if (vt.charAt(0) != 'v') {
                    view.askForPrint(ITrazAquiView.Print.Invalid, "");
                    break;
                }
                view.askForPrint(ITrazAquiView.Print.Nome, "");
                String nomev = Input.lerString();
                view.askForPrint(ITrazAquiView.Print.Pass, "");
                String passv = Input.lerString();
                view.askForPrint(ITrazAquiView.Print.Coordenada, "x");
                double xv = Input.lerDouble();
                view.askForPrint(ITrazAquiView.Print.Coordenada, "y");
                double yv = Input.lerDouble();
                view.askForPrint(ITrazAquiView.Print.Cena, "raio");
                double raioV = Input.lerDouble();
                try {
                    this.model.criaVoluntario(vt, nomev, xv, yv, raioV, passv);
                } catch (DuplicateUserException e) {
                    this.view.askForPrint(ITrazAquiView.Print.Erro, e.getMessage());
                }
                this.menu = Menu.Inicial;
                break;
            case 3:
                view.askForPrint(ITrazAquiView.Print.NomeUt, "");
                String tt = Input.lerString();
                if (tt.charAt(0) != 't') {
                    view.askForPrint(ITrazAquiView.Print.Invalid, "");
                    break;
                }
                view.askForPrint(ITrazAquiView.Print.Nome, "");
                String nomet = Input.lerString();
                view.askForPrint(ITrazAquiView.Print.Pass, "");
                String passt = Input.lerString();
                view.askForPrint(ITrazAquiView.Print.Coordenada, "x");
                double xt = Input.lerDouble();
                view.askForPrint(ITrazAquiView.Print.Coordenada, "y");
                double yt = Input.lerDouble();
                view.askForPrint(ITrazAquiView.Print.Cena, "nif[9 digitos]");
                int nif;
                while (true) {
                    nif = Input.lerInt();
                    if (nif >= 100000000 && nif <= 999999999)
                        break;
                    else view.askForPrint(ITrazAquiView.Print.Invalid, "");
                }
                view.askForPrint(ITrazAquiView.Print.Cena, "raio");
                double raiot = Input.lerDouble();
                view.askForPrint(ITrazAquiView.Print.Cena, "preco por Km");
                double precoKM = Input.lerDouble();
                view.askForPrint(ITrazAquiView.Print.Cena, "preco por Kg");
                double precoPeso = Input.lerDouble();
                try {
                    this.model.criaTransporter(tt, nomet, xt, yt, nif, raiot, precoKM, precoPeso, passt);
                } catch (DuplicateUserException e) {
                    this.view.askForPrint(ITrazAquiView.Print.Erro, e.getMessage());
                }
                this.menu = Menu.Inicial;
                break;
            case 4:
                view.askForPrint(ITrazAquiView.Print.NomeUt, "");
                String lt = Input.lerString();
                if (lt.charAt(0) != 'l') {
                    view.askForPrint(ITrazAquiView.Print.Invalid, "");
                    break;
                }
                view.askForPrint(ITrazAquiView.Print.Nome, "");
                String nomel = Input.lerString();
                view.askForPrint(ITrazAquiView.Print.Pass, "");
                String passl = Input.lerString();
                view.askForPrint(ITrazAquiView.Print.Coordenada, "x");
                double xl = Input.lerDouble();
                view.askForPrint(ITrazAquiView.Print.Coordenada, "y");
                double yl = Input.lerDouble();
                try {
                    this.model.criaLoja(lt, nomel, xl, yl, passl);
                } catch (DuplicateUserException e) {
                    this.view.askForPrint(ITrazAquiView.Print.Erro, e.getMessage());
                }
                this.menu = Menu.Inicial;
                break;
            case 5:
                this.menu = Menu.Inicial;
                break;
        }
    }

    /**
     * Método responsável pela iteraçao com o utilizador e pela requisição dos dados do model.
     */
    public void start() {
        String erro = "";
        boolean flag = true;
        while (flag) {
            switch (menu) {
                case Inicial:
                    this.view.askForPrint(ITrazAquiView.Print.Inicial, "");
                    int inicial = Input.lerInt();
                    switch (inicial) {
                        case 1:
                            this.menu = Menu.Login;
                            break;
                        case 2:
                            this.menu = Menu.SignUp;
                            break;
                        case 3:
                            this.menu = Menu.Save;
                            break;
                        case 4:
                            this.menu = Menu.Load;
                            break;
                        case 5:
                            view.imprimeTopCli(this.model.topMelhoresClientes());
                            menu = Menu.Inicial;
                            break;
                        case 6:
                            view.imprimeTopTr(this.model.topMelhoresTransportes());
                            menu = Menu.Inicial;
                            break;
                        case 7:
                            flag = false;
                            break;
                    }
                    break;

                case SignUp:
                    this.registaConta();
                    break;

                case Login:
                    this.view.askForPrint(ITrazAquiView.Print.Mail, "");
                    String mail = Input.lerString();
                    this.view.askForPrint(ITrazAquiView.Print.Pass, "");
                    String pass = Input.lerString();
                    Entidade e;
                    try {
                        e = this.model.login(mail, pass);
                    } catch (Exception z) {
                        this.view.askForPrint(ITrazAquiView.Print.BadLogin, "");
                        break;
                    }
                    if (e != null) {
                        this.model.setLoginEnt(e);
                    } else {
                        this.view.askForPrint(ITrazAquiView.Print.BadLogin, "");
                        break;
                    }
                    switch (mail.charAt(0)) {
                        case 'u':
                            menu = Menu.Utilizador;
                            break;
                        case 'v':
                            menu = Menu.Voluntario;
                            break;
                        case 't':
                            menu = Menu.Transportadora;
                            break;
                        case 'l':
                            menu = Menu.Loja;
                            break;
                        default:
                            break;
                    }
                    break;
                case Utilizador:
                    view.askForPrint(ITrazAquiView.Print.Utilizador, this.model.login.getEnt().getCodigo());
                    int input = Input.lerInt();
                    switch (input) {
                        case 1:
                            menu = Menu.Utilizador1;
                            break;
                        case 2:
                            menu = Menu.Utilizador2;
                            break;
                        case 3:
                            menu = Menu.Utilizador3;
                            break;
                        case 4:
                            menu = Menu.Inicial;
                            break;
                    }
                    break;
                case Utilizador1:
                    Loja l = null;
                    List<LinhaEncomenda> linhasE = new ArrayList<>();
                    List<Loja> lojas = this.model.getLojas().values().stream().sorted(Comparator.comparingInt(a -> parseLoja(a.getCodigo()))).collect(Collectors.toList());
                    view.imprimeLojas(lojas.stream().map(a-> new AbstractMap.SimpleEntry<>(a.getCodigo(),a.getNome())).collect(Collectors.toList()));
                    view.askForPrint(ITrazAquiView.Print.Quit,String.valueOf(lojas.size()+1));
                    int inputLoja;
                    while (true) {
                        inputLoja = Input.lerInt();
                        if (inputLoja == lojas.size()+1){
                            menu = Menu.Utilizador;
                            break;
                        }
                        if (inputLoja > 0 && inputLoja <= lojas.size()) {
                            l = lojas.get(inputLoja - 1);
                            break;
                        } else {
                            view.askForPrint(ITrazAquiView.Print.Invalid, "");
                        }
                    }
                    if(inputLoja == lojas.size()+1) break;
                    Map<String, Map.Entry<String, Double>> produtosMap = l.getListaProdutos();
                    List<Map.Entry<String, Map.Entry<String, Double>>> produtos = produtosMap.entrySet().stream().sorted(Comparator.comparingInt(a -> parsePrd(a.getKey()))).collect(Collectors.toList());
                    view.imprimeProds(produtos);
                    String inputProduto = "";
                    while (true) {
                        view.askForPrint(ITrazAquiView.Print.Produto, "");
                        inputProduto = Input.lerString();
                        if (inputProduto.equals("q")) {
                            break;
                        }
                        if (produtosMap.containsKey(inputProduto)) {
                            view.askForPrint(ITrazAquiView.Print.Quantidade, "");
                            double quantidade = Input.lerDouble();
                            linhasE.add(new LinhaEncomenda(inputProduto, produtosMap.get(inputProduto).getKey(), produtosMap.get(inputProduto).getValue(), quantidade));
                            view.askForPrint(ITrazAquiView.Print.Carrinho, inputProduto);
                        } else {
                            view.askForPrint(ITrazAquiView.Print.Invalid, "");
                        }
                    }
                    if (linhasE.size() > 0) {
                        int inputem;
                        boolean em = false;
                        while(true) {
                            view.askForPrint(ITrazAquiView.Print.EncomendaMedica, "");
                            inputem = Input.lerInt();
                            if (inputem == 1) {
                                em = true;
                                break;
                            }
                            else if (inputem == 2) {
                                em = false;
                                break;
                            }
                            else view.askForPrint(ITrazAquiView.Print.Invalid, "");
                        }
                        String cod = this.model.gereCodEncomenda();
                        this.model.addEncomendaLoja(new Encomenda(cod, this.model.getLoginEnte().getCodigo(), l.getCodigo(), linhasE.size() * Math.random() * 5 + 3, linhasE,em));
                        view.askForPrint(ITrazAquiView.Print.Success, "Encomenda " + cod);
                    } else
                        view.askForPrint(ITrazAquiView.Print.NotEncomenda, "");
                    menu = Menu.Utilizador;
                    break;

                case Utilizador2:
                    List<Encomenda> encomendas = this.model.listaEncomendasUtilizador(this.model.getLoginEnte().getCodigo()).stream()
                            .sorted(Comparator.comparingInt(a -> parseEnc(a.getCodEncomenda())))
                            .collect(Collectors.toList());
                    if (encomendas.size() == 0) {
                        this.menu = Menu.Utilizador;
                        view.askForPrint(ITrazAquiView.Print.NotEncomendas, "");
                        break;
                    }
                    view.imprimeEncs(encomendas.stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList()),
                                     encomendas.stream().filter(Encomenda::isEntregue).map(Encomenda::getCodEncomenda).collect(Collectors.toList()));
                    Encomenda ut2 = null;
                    int input2;
                    while (true) {
                        input2 = Input.lerInt();
                        if (input2 > 0 && input2 <= encomendas.size()) {
                            ut2 = encomendas.get(input2 - 1);
                            break;
                        }
                        else if(input2==encomendas.size()+1){
                            menu = Menu.Utilizador;
                            break;
                        }
                        else {
                            view.askForPrint(ITrazAquiView.Print.Invalid, "");
                        }
                    }
                    if(input2==encomendas.size()+1) break;
                    if (!ut2.isEntregue()) {
                        Set<Transportes> disp;
                        disp = this.model.transportesValidosUtilizador((Utilizador) this.model.getLoginEnte(), ut2);
                        if(disp.isEmpty()){
                            this.view.askForPrint(ITrazAquiView.Print.NTrans,"");
                            menu = Menu.Utilizador;
                            break;
                        }
                        boolean sair = false;
                        Iterator<Transportes> it = disp.iterator();
                        while (it.hasNext() && !sair) {
                            Transportes temp = it.next();
                            double dist = model.getDistancia(this.model.getLojas().get(ut2.getCodLoja()), (Utilizador) this.model.login.getEnt(), temp);
                            if (temp instanceof Transportadoras) {
                                view.imprimeTransp(temp.getCodigo(), temp.getNome(), String.format("%.2f", dist), ((Transportadoras) temp).calculaPreco(dist, ut2.getPeso())
                                                    ,this.model.tempoTransporte(ut2,temp),this.model.getClassificacao(temp));
                                view.askForPrint(ITrazAquiView.Print.TransYn, "");
                                String aceitar;
                                boolean entregar;
                                while (true) {
                                    aceitar = Input.lerString();
                                    if (aceitar.equals("Y") || aceitar.equals("")) {
                                        entregar = true;
                                        break;
                                    } else if (aceitar.equals("n")) {
                                        entregar = false;
                                        break;
                                    } else view.askForPrint(ITrazAquiView.Print.Invalid, "");
                                }
                                if (entregar) {
                                    this.model.fazEntrega(this.model.getLojas().get(ut2.getCodLoja()), ut2, temp);
                                    //Classificar
                                    view.askForPrint(ITrazAquiView.Print.Classificar, "transportador");
                                    while (true) {
                                        double input7 = Input.lerDouble();
                                        if (input7 >= 0 && input7 <= 5) {
                                            this.model.classificaTransportadora(temp, input7);
                                            break;
                                        } else {
                                            view.askForPrint(ITrazAquiView.Print.Invalid, "");
                                        }
                                    }
                                    double dists = this.model.getDistancia(this.model.getLojas().get(ut2.getCodLoja()), (Utilizador) this.model.login.getEnt(), temp);
                                    this.model.aumentaKm(temp, dists);
                                    sair = true;
                                }
                            } else {
                                view.imprimeVoluntario(temp.getCodigo(), temp.getNome());
                                this.model.mandaPedido(ut2, (Voluntario) temp);
                                this.model.removeEncomenda(ut2);
                                sair = true;
                            }
                        }
                    } else {
                        Voluntario temp = this.model.getVoluntarios().get(ut2.getCodTrans());
                        view.askForPrint(ITrazAquiView.Print.Classificar, "voluntário " + ut2.getCodTrans());
                        double input7 = Input.lerDouble();
                        while (true) {
                            if (input7 >= 0 && input7 <= 5) {
                                this.model.classificaTransportadora(temp, input7);
                                break;
                            }
                            view.askForPrint(ITrazAquiView.Print.Invalid, "");
                        }
                        double dists2 = this.model.getDistancia(this.model.getLojas().get(ut2.getCodLoja()), (Utilizador) this.model.login.getEnt(), temp);
                        this.model.aumentaKm(temp, dists2);
                        this.model.removeEncomenda(ut2);
                    }
                    this.menu = Menu.Utilizador;
                    break;

                case Utilizador3:
                    view.askForPrint(ITrazAquiView.Print.Data, "");
                    List<Encomenda> a = null;
                    while (true) {
                        if (!erro.equals("")) {
                            view.askForPrint(ITrazAquiView.Print.Erro, erro);
                            view.askForPrint(ITrazAquiView.Print.Data, "");
                            erro = "";
                        }
                        String t1 = Input.lerString();
                        String t2 = Input.lerString();
                        try {
                            String[] t1A = t1.split("/");
                            String[] t2A = t2.split("/");
                            LocalDateTime t1L = LocalDateTime.of(Integer.parseInt(t1A[2]), Integer.parseInt(t1A[1]), Integer.parseInt(t1A[0]), 0, 0);
                            LocalDateTime t2L = LocalDateTime.of(Integer.parseInt(t2A[2]), Integer.parseInt(t2A[1]), Integer.parseInt(t2A[0]), 0, 0);
                            a = this.model.getUtilizadores().get(this.model.getLoginEnte().getCodigo()).historicoData(t1L, t2L);
                            erro = "";
                            break;
                        } catch (NumberFormatException | DateTimeException | PatternSyntaxException er) {
                            erro = "Data Invalida";
                        }
                    }
                    view.askForPrint(ITrazAquiView.Print.DataOp, "");
                    while (true) {
                        int input3 = Input.lerInt();
                        if (input3 == 1) {
                            a = this.model.filterTransportador("Voluntario", a);
                            break;
                        } else if (input3 == 2) {
                            a = this.model.filterTransportador("!Voluntario", a);
                            break;
                        } else {
                            view.askForPrint(ITrazAquiView.Print.Invalid, "");
                        }
                    }
                    if(a.isEmpty()) this.view.askForPrint(ITrazAquiView.Print.HistoricoVazio,"");
                    view.imprimeEnc2(a.stream().map(Encomenda::getCodLoja).collect(Collectors.toList()),
                            a.stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList()),
                            a.stream().map(Encomenda::precoEncomenda).collect(Collectors.toList()),
                            a.stream().filter(Encomenda::isEncomendaMedica).map(Encomenda::getCodEncomenda).collect(Collectors.toList()));
                    menu = Menu.Utilizador;
                    break;

                case Voluntario:
                    view.askForPrint(ITrazAquiView.Print.Voluntario, this.model.login.getEnt().getCodigo());
                    int inputV = Input.lerInt();
                    switch (inputV) {
                        case 1:
                            menu = Menu.Voluntario1;
                            break;
                        case 2:
                            menu = Menu.Voluntario2;
                            break;
                        case 3:
                            menu = Menu.Voluntario3;
                            break;
                        case 4:
                            menu = Menu.Voluntario4;
                            break;
                        case 5:
                            menu = Menu.Inicial;
                            break;
                    }
                    break;

                case Voluntario1:
                    Voluntario v = this.model.getVoluntarios().get(this.model.getLoginEnte().getCodigo());
                    if (v.isFree()) {
                        System.out.println("Não tem encomendas pendentes");
                    } else {
                        System.out.println("Tem um entrega pendente da encomenda " + v.getEnc().getCodEncomenda());
                        System.out.println("Deseja fazer a entrega [Y/n]");
                        String aceitar;
                        boolean entregar;
                        while (true) {
                            aceitar = Input.lerString();
                            if (aceitar.equals("Y") || aceitar.equals("")) {
                                entregar = true;
                                break;
                            } else if (aceitar.equals("n")) {
                                entregar = false;
                                break;
                            } else view.askForPrint(ITrazAquiView.Print.Invalid, "");
                        }
                        if (entregar) {
                            Encomenda t = v.getEnc();
                            this.model.addAceites(t);
                            this.model.fazEntrega(this.model.getLojas().get(v.getEnc().getCodLoja()), v.getEnc(), v);
                            t.setEntregue(true);
                            t.setCodTrans(v.getCodigo());
                            this.model.addAceites(t);
                        } else {
                            this.model.addAceites(v.getEnc());
                            v.setFree(true);
                            v.setEnc(null);
                        }
                    }
                    this.menu = Menu.Voluntario;
                    break;

                case Voluntario2:
                    view.askForPrint(ITrazAquiView.Print.Data, "");
                    List<Encomenda> av = null;
                    while (true) {
                        if (!erro.equals("")) {
                            view.askForPrint(ITrazAquiView.Print.Erro, erro);
                            view.askForPrint(ITrazAquiView.Print.Data, "");
                            erro = "";
                        }
                        String t1 = Input.lerString();
                        String t2 = Input.lerString();
                        try {
                            String[] t1A = t1.split("/");
                            String[] t2A = t2.split("/");
                            LocalDateTime t1L = LocalDateTime.of(Integer.parseInt(t1A[2]), Integer.parseInt(t1A[1]), Integer.parseInt(t1A[0]), 0, 0);
                            LocalDateTime t2L = LocalDateTime.of(Integer.parseInt(t2A[2]), Integer.parseInt(t2A[1]), Integer.parseInt(t2A[0]), 0, 0);
                            av = this.model.getVoluntarios().get(this.model.getLoginEnte().getCodigo()).historicoData(t1L, t2L);
                            erro = "";
                            break;
                        } catch (NumberFormatException | DateTimeException | PatternSyntaxException er) {
                            erro = "Data Invalida";
                        }
                    }
                    if(av.isEmpty()) this.view.askForPrint(ITrazAquiView.Print.HistoricoVazio,"");
                    view.imprimeEnc2(av.stream().map(Encomenda::getCodLoja).collect(Collectors.toList()),
                            av.stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList()),
                            av.stream().map(Encomenda::precoEncomenda).collect(Collectors.toList()),
                            av.stream().filter(Encomenda::isEncomendaMedica).map(Encomenda::getCodEncomenda).collect(Collectors.toList()));
                    menu = Menu.Voluntario;
                    break;

                case Voluntario3:
                    this.view.askForPrint(ITrazAquiView.Print.Classificacao, String.format("%.2f", this.model.getClassificacao((Transportes) this.model.getLoginEnte())));
                    menu = Menu.Voluntario;
                    break;
                case Voluntario4:
                    this.view.askForPrint(ITrazAquiView.Print.VMedico,"");
                    while(true) {
                        int inputvm = Input.lerInt();
                        if (inputvm == 1) {
                            this.model.alteraTransportesMedicos((Transportes) this.model.getLoginEnte(),true);
                            break;
                        }
                        else if (inputvm == 2) {
                            this.model.alteraTransportesMedicos((Transportes) this.model.getLoginEnte(),false);
                            break;
                        }
                        else this.view.askForPrint(ITrazAquiView.Print.Invalid, "");
                    }
                    menu = Menu.Voluntario;
                    break;

                case Loja:
                    view.askForPrint(ITrazAquiView.Print.Loja, this.model.login.getEnt().getCodigo());
                    int inputL = Input.lerInt();
                    switch (inputL) {
                        case 1:
                            menu = Menu.Loja1;
                            break;
                        case 2:
                            menu = Menu.Loja2;
                            break;
                        case 3:
                            menu = Menu.Loja3;
                            break;
                        case 4:
                            menu = Menu.Loja4;
                            break;
                        case 5:
                            menu = Menu.Inicial;
                            break;
                    }
                    break;
                case Loja1:
                    Encomenda aux = null;
                    List<Encomenda> enc = this.model.getPendentesLoja(this.model.getLoginEnte().getCodigo()).stream().sorted(Comparator.comparing(Encomenda::getCodEncomenda)).collect(Collectors.toList());
                    if (enc.isEmpty()) {
                        System.out.println("Não tem encomendas pendentes");
                    } else {
                        view.imprimeEncs(enc.stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList()),
                                enc.stream().filter(Encomenda::isEntregue).map(Encomenda::getCodEncomenda).collect(Collectors.toList()));
                        int inputl;
                        while (true) {
                            inputl = Input.lerInt();
                            if (inputl > 0 && inputl <= enc.size()) {
                                aux = enc.get(inputl - 1);
                                break;
                            }
                            else if(inputl==enc.size()+1){
                                menu = Menu.Loja;
                                break;
                            }
                            else {
                                view.askForPrint(ITrazAquiView.Print.Invalid, "");
                            }
                        }
                        if(inputl==enc.size()+1) break;
                        this.model.addAceites(aux);
                    }
                    this.menu = Menu.Loja;
                    break;
                case Loja2:
                    view.askForPrint(ITrazAquiView.Print.Data, "");
                    List<Encomenda> al = null;
                    while (true) {
                        if (!erro.equals("")) {
                            view.askForPrint(ITrazAquiView.Print.Erro, erro);
                            view.askForPrint(ITrazAquiView.Print.Data, "");
                            erro = "";
                        }
                        String t1 = Input.lerString();
                        String t2 = Input.lerString();
                        try {
                            String[] t1A = t1.split("/");
                            String[] t2A = t2.split("/");
                            LocalDateTime t1L = LocalDateTime.of(Integer.parseInt(t1A[2]), Integer.parseInt(t1A[1]), Integer.parseInt(t1A[0]), 0, 0);
                            LocalDateTime t2L = LocalDateTime.of(Integer.parseInt(t2A[2]), Integer.parseInt(t2A[1]), Integer.parseInt(t2A[0]), 0, 0);
                            Loja tmp = this.model.getLojas().get(this.model.getLoginEnte().getCodigo());
                            al = this.model.getLojas().get(this.model.getLoginEnte().getCodigo()).historicoData(t1L, t2L);
                            erro = "";
                            break;
                        } catch (NumberFormatException | DateTimeException | PatternSyntaxException er) {
                            erro = "Data Invalida";
                        }
                    }
                    if (al.isEmpty()) {
                        view.askForPrint(ITrazAquiView.Print.HistoricoVazio, "");
                    }
                    else {
                        view.imprimeEnc2(al.stream().map(Encomenda::getCodLoja).collect(Collectors.toList()),
                                al.stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList()),
                                al.stream().map(Encomenda::precoEncomenda).collect(Collectors.toList()),
                                al.stream().filter(Encomenda::isEncomendaMedica).map(Encomenda::getCodEncomenda).collect(Collectors.toList()));
                        menu = Menu.Loja;
                    }

                    menu = Menu.Loja;
                    break;
                case Loja3:
                     view.askForPrint(ITrazAquiView.Print.AddProduto,"");
                     while(true) {
                         String inputc = Input.lerString();
                         if (inputc.charAt(0) != 'p') {
                             view.askForPrint(ITrazAquiView.Print.Invalid, "");
                         }
                         else if(this.model.getLojas().get(this.model.getLoginEnte().getCodigo()).getListaProdutos().containsKey(inputc)){
                             view.askForPrint(ITrazAquiView.Print.Invalid,"O produto" + inputc + "ja existe.");
                         }
                         else {
                             String inputn = Input.lerString();
                             Double inputp = Input.lerDouble();
                             this.model.addToCatalogoLoja(this.model.getLoginEnte().getCodigo(),inputc,inputn,inputp);
                             break;
                         }
                     }
                     this.menu = Menu.Loja;
                     break;
                case Loja4:
                    Loja l1 = this.model.getLojas().get(this.model.getLoginEnte().getCodigo());
                    Map<String, Map.Entry<String, Double>> produtosMap1 = l1.getListaProdutos();
                    List<Map.Entry<String, Map.Entry<String, Double>>> produtos1 = produtosMap1.entrySet().stream().sorted(Comparator.comparingInt(a1 -> parsePrd(a1.getKey()))).collect(Collectors.toList());
                    view.imprimeProds(produtos1);
                    this.menu = Menu.Loja;
                    break;
                case Transportadora:
                    view.askForPrint(ITrazAquiView.Print.Transportadora, this.model.login.getEnt().getCodigo());
                    int inputT = Input.lerInt();
                    switch (inputT) {
                        case 1:
                            menu = Menu.Transportadora1;
                            break;
                        case 2:
                            menu = Menu.Transportadora2;
                            break;
                        case 3:
                            menu = Menu.Transportadora3;
                            break;
                        case 4:
                            menu = Menu.Transportadora4;
                            break;
                        case 5:
                            menu = Menu.Inicial;
                            break;
                    }
                    break;
                case Transportadora1:
                    view.askForPrint(ITrazAquiView.Print.Data, "");
                    List<Encomenda> at = null;
                    while (true) {
                        if (!erro.equals("")) {
                            view.askForPrint(ITrazAquiView.Print.Erro, erro);
                            view.askForPrint(ITrazAquiView.Print.Data, "");
                            erro = "";
                        }
                        String t1 = Input.lerString();
                        String t2 = Input.lerString();
                        try {
                            String[] t1A = t1.split("/");
                            String[] t2A = t2.split("/");
                            LocalDateTime t1L = LocalDateTime.of(Integer.parseInt(t1A[2]), Integer.parseInt(t1A[1]), Integer.parseInt(t1A[0]), 0, 0);
                            LocalDateTime t2L = LocalDateTime.of(Integer.parseInt(t2A[2]), Integer.parseInt(t2A[1]), Integer.parseInt(t2A[0]), 0, 0);
                            at = this.model.getTransportadoras().get(this.model.getLoginEnte().getCodigo()).historicoData(t1L, t2L);
                            erro = "";
                            break;
                        } catch (NumberFormatException | DateTimeException | PatternSyntaxException er) {
                            erro = "Data Invalida";
                        }
                    }
                    if(at.isEmpty()) this.view.askForPrint(ITrazAquiView.Print.HistoricoVazio,"");
                    view.imprimeEnc2(at.stream().map(Encomenda::getCodLoja).collect(Collectors.toList()),
                            at.stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList()),
                            at.stream().map(Encomenda::precoEncomenda).collect(Collectors.toList()),
                            at.stream().filter(Encomenda::isEncomendaMedica).map(Encomenda::getCodEncomenda).collect(Collectors.toList()));
                    menu = Menu.Transportadora;
                    break;
                case Transportadora2:
                    this.view.askForPrint(ITrazAquiView.Print.Classificacao, String.format("%.2f", this.model.getClassificacao((Transportes) this.model.getLoginEnte())));
                    menu = Menu.Transportadora;
                    break;
                case Transportadora3:
                    view.askForPrint(ITrazAquiView.Print.Data, "");
                    double tf;
                    while (true) {
                        if (!erro.equals("")) {
                            view.askForPrint(ITrazAquiView.Print.Erro, erro);
                            view.askForPrint(ITrazAquiView.Print.Data, "");
                            erro = "";
                        }
                        String t1 = Input.lerString();
                        String t2 = Input.lerString();
                        try {
                            String[] t1A = t1.split("/");
                            String[] t2A = t2.split("/");
                            LocalDateTime t1L = LocalDateTime.of(Integer.parseInt(t1A[2]), Integer.parseInt(t1A[1]), Integer.parseInt(t1A[0]), 0, 0);
                            LocalDateTime t2L = LocalDateTime.of(Integer.parseInt(t2A[2]), Integer.parseInt(t2A[1]), Integer.parseInt(t2A[0]), 0, 0);
                            tf = this.model.totalFaturado((Transportadoras) this.model.getLoginEnte(), t1L, t2L);
                            erro = "";
                            break;
                        } catch (NumberFormatException | DateTimeException | PatternSyntaxException er) {
                            erro = "Data Invalida";
                        }
                    }
                    this.view.askForPrint(ITrazAquiView.Print.TotalFaturado, String.format("%.2f" , tf));
                    menu = Menu.Transportadora;
                    break;
                case Transportadora4:
                    this.view.askForPrint(ITrazAquiView.Print.VMedico,"");
                    while(true) {
                        int inputtm = Input.lerInt();
                        if (inputtm == 1) {
                            this.model.alteraTransportesMedicos((Transportes) this.model.getLoginEnte(),true);
                            break;
                        }
                        else if (inputtm == 2) {
                            this.model.alteraTransportesMedicos((Transportes) this.model.getLoginEnte(),false);
                            break;
                        }
                        else this.view.askForPrint(ITrazAquiView.Print.Invalid, "");
                    }
                    menu = Menu.Transportadora;
                    break;
                case Save:
                    view.askForPrint(ITrazAquiView.Print.Ficheiro,"gravar");
                    try{
                        String file = Input.lerString();
                        if(file.equals("")) {
                            file = "default.dat";
                        }
                        this.model.save(file);
                        menu = Menu.Inicial;
                    } catch (IOException ioException) {
                        erro = "Ficheiro Invalido.";
                    }
                    break;
                case Load:
                    view.askForPrint(ITrazAquiView.Print.Ficheiro,"carregar");
                    try{
                        String file = Input.lerString();
                        if(file.equals("")) {
                            file = "default.dat";
                        }
                        this.model = null;
                        this.model = TrazAquiModel.load(file);
                        menu = Menu.Inicial;
                    } catch (IOException | ClassNotFoundException ioException) {
                        erro = "Ficheiro Invalido.";
                    }
                    break;


            }
        }
    }
}
