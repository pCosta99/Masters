package TrazAqui;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Collectors;

public class Menu {
    /**
     * Variaveis de instancia
     */
    private boolean exec;
    private FileIO f;
    private Estado e;

    /**
     * Construtor vazio
     */
    public Menu() {
        this.exec = true;
        this.f = new FileIO("teste.txt", "Estado", "Credentials.txt");
        this.e = new Estado();
    }

    /**
     * Metodo que inicia o funcionamento da aplicacao
     * @throws IOException Casos de erro em IO
     * @throws LojaInexistenteException casos de erro em Lojas
     */
    public void run() throws IOException, LojaInexistenteException {
        Scanner inp = new Scanner(System.in);
        int opcao = -1;
        try {
            e = f.readObjectStream();
        }
        catch (IOException | ClassNotFoundException ex) {
            f.loadFromFile(e);
        }
        while(this.exec) {
            while (e.getLogin() == null && this.exec) {
                boolean f = true;
                UI.printMenuInicial();
                while (f) {
                    try {
                        opcao = inp.nextInt();
                    } catch (InputMismatchException ex) {
                        UI.print("Opcao inválida!");
                        inp.nextLine();
                    }
                    if (opcao >= 0 && opcao <= 2) f = false;
                }
                switch (opcao) {
                    case 0:
                        stopExec();
                        break;
                    case 1:
                        try {
                            this.loginUtilizador();
                        } catch (IOException e) {
                            e.printStackTrace();
                        } catch (InvalidInputException e) {
                            UI.print(e.getMessage());
                        }

                        break;
                    case 2:
                        try {
                            this.novoRegisto();
                        } catch (IOException e) {
                            e.printStackTrace();
                        } catch (InvalidInputException | InputMismatchException | ExistingCodeException l) {
                            UI.print(l.getMessage());
                        }
                        break;
                    default:
                        throw new IllegalStateException("Unexpected value: " + opcao);
                }
            }
            if (this.exec)
                switch (e.getLogin().getClass().getSimpleName()) {
                    case "Utilizador":
                        while (this.exec && e.getLogin() != null) {
                            menuUtilizador();
                        }
                        break;
                    case "Transportadora":
                        while (this.exec && e.getLogin() != null) {
                            menuTransportadora();
                        }
                        break;
                    case "Voluntario":
                        while (this.exec && e.getLogin() != null) {
                            menuVoluntario();
                        }
                        break;
                    case "Loja":
                    case "LojaFilaEspera":
                        while (this.exec && e.getLogin() != null) {
                            menuLoja();
                        }
                    default:
                        break;
                }
        }
        try {
            f.saveObjectStream(e);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        UI.goodbye();
    }

    /**
     * Para a execucao
     */
    public void stopExec() {
        this.exec = false;
        e.logoff();
    }

    /**
     * Metodo que faz login de um utilizador
     * @throws IOException Casos de erro em IO
     * @throws InvalidInputException Casos de erro no input
     */
    public void loginUtilizador() throws IOException , InvalidInputException {
        Scanner sc = new Scanner(System.in);
        UI.printInsiraEmail();
        String email = sc.nextLine();
        if (verificaEmail(email)) throw new InvalidInputException("Email invalido!");
        UI.printInsiraPassword();
        String password = sc.nextLine();
        this.e.login(email, password, this.f);
    }

    /**
     * Metodo que verifica se um email esta corretamente inserido
     * @param mail String
     * @return boolean
     */
    private boolean verificaEmail(String mail) {
        String[] tokens;
        String temp;
        tokens = mail.split("@");
        if (tokens.length != 2 || tokens[0].equals("")) return true;
        temp = tokens[1];
        tokens = temp.split("\\.");
        return (tokens.length != 2 || tokens[0].equals(""));
    }

    /**
     * Metodo que cria um novo registo
     * @throws IOException Caso de erro de IO
     * @throws InvalidInputException Caso de erro de input
     * @throws InputMismatchException Caso de erro de na insercao de dados
     * @throws ExistingCodeException Caso de erro na insercao de codigo
     */
    public void novoRegisto() throws IOException, InvalidInputException, InputMismatchException, ExistingCodeException {
        Scanner sc = new Scanner(System.in);

        UI.printTipoRegisto();
        String tipo = sc.nextLine();
        if(!tipo.equals("Utilizador") && !tipo.equals("Loja") && !tipo.equals("LojaFilaEspera") && !tipo.equals("Transportadora") && !tipo.equals("Voluntario"))
            throw new InvalidInputException("Tipo de registo inválido!");
        UI.printInsiraEmail();
        String email = sc.nextLine();
        if (verificaEmail(email)) {
            throw new InvalidInputException("Email inválido!");
        }
        UI.printInsiraPassword();
        String password = sc.nextLine();
        UI.printInsiraCod();
        String cod = sc.nextLine();
        if(!((tipo.equals("Utilizador") && cod.charAt(0) == 'u')
        || (tipo.equals("Loja") || tipo.equals("LojaFilaEspera") && cod.charAt(0) == 'l')
        || (tipo.equals("Transportadora") && cod.charAt(0) == 't')
        || (tipo.equals("Voluntario") && cod.charAt(0) == 'v')))
            throw new InvalidInputException("Código inválido!");
        UI.printInsiraNome();
        String nome = sc.nextLine();
        UI.printInsiraLatitude();
        double lat = sc.nextDouble();
        UI.printInsiraLongitude();
        double longi = sc.nextDouble();
        sc.nextLine();
        if(tipo.equals("Transportadora")) {
            UI.printNif();
            String nif = sc.nextLine();
            UI.printCertificado();
            boolean cert = sc.nextBoolean();
            this.e.registar(email, password, cod, nome, new GPS(lat, longi), this.f, tipo,nif,cert);
        }
        else
            this.e.registar(email, password, cod, nome, new GPS(lat, longi), this.f, tipo,"",false);
    }

    /**
     * Retorna o preco de uma encomenda
     * @param enc Encomenda
     * @param transp String
     * @return double
     */
    public double getPreco(Encomenda enc, String transp) {
        if(e.getTrabalhadores().get(transp) instanceof Transportadora ){
            return e.precoDaEncomendaMenu(enc,transp);
        }
        else{
            return 0.0;
        }

    }

    /**
     * Menu do utilizador 
     * @throws LojaInexistenteException Caso de loja inexistente 
     */
    public void menuUtilizador() throws LojaInexistenteException {
        int opcao =-1;
        Scanner sc = new Scanner(System.in);
            UI.printMenuUtilizador();
            opcao = sc.nextInt();
            sc.nextLine();
            switch (opcao) {
                case 1:
                    Encomenda enc = new Encomenda();
                    double peso,preco, quantidade;
                    boolean fragil, conti = true,med;
                    String codEnc,loja,cod, descricao;
                    enc.setData(LocalDateTime.now());
                    UI.printInsiraCodEnc();
                    codEnc = sc.nextLine();
                    enc.setCod(codEnc);
                    UI.printTransMedica();
                    try{
                        med = sc.nextBoolean();
                        enc.setMedicamentos(med);
                        UI.printInsiraPeso();
                        peso = sc.nextDouble();
                        enc.setPeso(peso);
                        enc.setUtilizador(e.getLogin().getCod());
                        UI.printLojas(e.getLojas());
                        UI.printInsiraCodLoja();
                        sc.nextLine();
                        loja = sc.nextLine();
                        enc.setLoja(loja);
                        while (conti) {
                            try {
                                UI.printFazerDescricao();
                                descricao = sc.nextLine();
                                UI.printIndicarPreco();
                                preco = sc.nextDouble();
                                UI.printIndicarQuant();
                                quantidade = sc.nextDouble();
                                UI.printIndicarFragil();
                                fragil = sc.nextBoolean();
                                sc.nextLine();
                                UI.printIndiqueCodProd();
                                cod = sc.nextLine();
                                enc.addProduto(new LinhaEncomenda(descricao, preco, quantidade, fragil, cod));
                                UI.printDesejaMaisProd();
                                conti = sc.nextBoolean();
                                sc.nextLine();
                            } catch (InputMismatchException e) {
                                UI.printTipoIncorreto();
                            }
                        }
                        e.addEncomendaLoja(loja, enc);
                    }
                    catch (InputMismatchException | LojaInexistenteException e){
                        UI.printTipoIncorreto();
                    }
                    break;
                case 2:
                    LocalDateTime dataInicial = LocalDateTime.now();
                    LocalDateTime dataFinal = LocalDateTime.now();
                    String inicio = "",fim = "";
                    String codigoUtilizador = e.getLogin().getCod();
                    Map<String,Encomenda> lstEnc = e.getUtilizador(codigoUtilizador).getEncomendasConcluidas();
                    UI.printDesejaTransVolTempo();
                    int option = sc.nextInt();
                    sc.nextLine();
                    if(option==3) {
                        if (lstEnc.size() > 0) {
                            try {
                                UI.printDataInicial();
                                inicio = sc.nextLine();
                                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                                dataInicial = LocalDateTime.parse(inicio, formatter);
                                UI.printDataFinal();
                                fim = sc.nextLine();
                                dataFinal = LocalDateTime.parse(fim, formatter);
                            } catch (DateTimeParseException ex) {
                                UI.printFormatoInvalido();
                            }
                        }
                    }
                    Utilizador u = e.getUtilizador(codigoUtilizador);
                    UI.printHistoricoEncomendas(lstEnc,option,u,dataInicial,dataFinal);
                    break;
                case 3:
                    int i=0;
                    for(Map.Entry<String,Estafeta> a : e.getTrabalhadores().entrySet()){
                        if(a.getValue().getPedidosEncomenda().size()>0 ) {
                            for(Encomenda x : a.getValue().getPedidosEncomenda())
                                if(x.getUtilizador().equals(e.getLogin().getCod())) {
                                    UI.printPedidosEncomenda(a.getValue().getPedidosEncomenda(), this, a.getValue().getCod());
                                    i++;
                                }
                        }
                    }
                    if(i>0) {
                        UI.printCodEncAceitar();
                        String codEncomenda = sc.nextLine();
                        UI.printAceitaOuNao();
                        int opc = sc.nextInt();
                        String codEsta = "";
                        Encomenda encomenda = null;
                        for(Map.Entry<String,Estafeta> a : e.getTrabalhadores().entrySet()) {
                            if(a.getValue() instanceof Transportadora)
                            for(Encomenda x : a.getValue().getPedidosEncomenda()) {
                                if(x.getUtilizador().equals(e.getLogin().getCod()) && x.getCod().equals(codEncomenda)) {
                                    codEsta = a.getKey();
                                    encomenda = x;
                                    break;
                                }
                            }
                        }
                        if(opc == 1) {
                            assert encomenda != null;
                            e.addEncomendaEntregue(codEsta,encomenda);
                            this.e.aumentaKms(codEsta,encomenda.getLoja());
                            e.removeEncomendaTransportadora(encomenda.getCod(),codEsta);
                            e.addEncomendaUtilizador(e.getLogin().getCod(), encomenda);
                            UI.printInsiraClass();
                            e.classifica(codEsta,sc.nextInt());
                        }
                        else if(opc == 2) {
                            assert encomenda != null;
                            e.addEncomendaLoja(encomenda.getLoja(),encomenda);
                            e.removeEncomendaTransportadora(encomenda.getCod(),codEsta);
                        }
                    }
                    else UI.print0encParaAceitar();
                    break;
                case 0:
                    stopExec();
                    try {
                        f.saveObjectStream(e);
                    }
                    catch (IOException e) {
                        e.printStackTrace();
                    }
                    break;
                case 4:
                    int j=0;
                    for(Map.Entry<String,Estafeta> a : e.getTrabalhadores().entrySet()){
                        if(a.getValue().getPedidosEncomenda().size()>0) {
                            //UI.printPedidosEncomenda(a.getValue().getPedidosEncomenda(),this, a.getValue().getCod());
                            j++;
                        }
                    }
                    if(j > 0) {
                        UI.printCodEncomendaClass();
                        String codEncomenda = sc.nextLine();
                        String codEsta = "";
                        Encomenda encomenda = null;
                        for(Map.Entry<String,Estafeta> a : e.getTrabalhadores().entrySet()) {
                            for(Encomenda x : a.getValue().getPedidosEncomenda()) {
                                if(x.getUtilizador().equals(e.getLogin().getCod()) && x.getCod().equals(codEncomenda)) {
                                    codEsta = a.getKey();
                                    encomenda = x;
                                    break;
                                }
                            }
                        }
                        e.addEncomendaEntregue(codEsta,encomenda);
                        assert encomenda != null;
                        e.removeEncomendaTransportadora(encomenda.getCod(),codEsta);
                        UI.printInsiraClass();
                        e.classifica(codEsta,sc.nextInt());
                    }
                    else UI.print0ClassVol();
                    break;
                case 5:
                    UI.printTop10(e.getTop10Util().stream().map(Utilizador::getNome).collect(Collectors.toList()));
                    break;
                case 6:
                    UI.printTop10(e.getTop10Trans().stream().map(Transportadora::getNome).collect(Collectors.toList()));
                    break;
                case 7:
                    e.logoff();
                    break;
                default:
                    UI.printIncorrectInput();
                    break;
            }
    }

    /**
     * Menu do voluntario 
     */
    public void menuVoluntario() {
        int opcao;
        Scanner sc = new Scanner(System.in);
        UI.printMenuVoluntario();
        opcao = sc.nextInt();
        String cod = this.e.getLogin().getCod();
        switch (opcao) {
            case 0:
                stopExec();
                try {
                    f.saveObjectStream(e);
                }
                catch (IOException e) {
                    e.printStackTrace();
                }
                exec = false;
                break;
            case 1:
                boolean b = this.e.mudaDisponibilidade(cod);
                if (b) UI.printDisponivel();
                else UI.printIndisponivel();
                break;
            case 2:
                if (!this.e.disponivel(cod)) {
                    UI.printMudeDisp();
                    break;
                }
                List<Encomenda> enc = this.e.encomendasDisponiveis(cod);
                UI.printEncomendas(enc);
                UI.print0NEncomendas();
                UI.printInsiraCodEnc();
                sc.nextLine();
                String codEncomenda = sc.nextLine();
                if (codEncomenda.equals("0")) break;
                if(enc.size()>0) {
                    try {
                        Encomenda e = this.e.removeEncomendaLoja(codEncomenda, cod);
                        if (e==null) {
                            UI.printNoMedica();
                            break;
                        }
                        e.setEstafeta(cod);
                        this.e.addEncomendaUtilizador(e.getUtilizador(),e);
                        this.e.addPedidoDeTransporte(cod,e);
                        UI.printEncomendaEmTrans();
                    } catch (Exception e) {
                        UI.printEncomendaInex();
                    }
                }
                break;
            case 3:
                UI.printSelectRaio();
                e.setRaio(cod,sc.nextDouble());
                break;
            case 4:
                UI.printClassMedia(e.getTrabalhadores().get(cod).getClassMedia());
                break;
            case 5:
                UI.printTop10(e.getTop10Util().stream().map(Utilizador::getNome).collect(Collectors.toList()));
                break;
            case 6:
                UI.printTop10(e.getTop10Trans().stream().map(Estafeta::getNome).collect(Collectors.toList()));
                break;
            case 7:
                this.e.logoff();
                break;
            default:
                break;
        }
    }

    /**
     * Menu da transportadora
     */
    public void menuTransportadora() {
        int opcao;
        Scanner sc = new Scanner(System.in);
        UI.printMenuTransportadora();
        opcao = sc.nextInt();
        String cod = this.e.getLogin().getCod();
        switch(opcao){
            case 0:
                stopExec();
                try {
                    f.saveObjectStream(e);
                }
                catch (IOException e) {
                    e.printStackTrace();
                }
                exec = false;
                break;
            case 1:
                boolean b = this.e.mudaDisponibilidade(cod);
                if (b) UI.printDisponivel();
                else UI.printIndisponivel();
                break;
            case 2:
                for (Loja l : this.e.getLojas().values()) {
                    if (l.getPedidos().size()>0) {
                        UI.printEncomendas(l.getPedidos());
                    }
                }
                UI.print0NEncomendas();
                UI.printInsiraCodEnc();
                sc.nextLine();
                String codEncomenda = sc.nextLine();
                if (codEncomenda.equals("0")) break;
                try {
                    double p = this.e.precoDaEncomenda(codEncomenda, cod);
                    if (p!=-1) UI.printPreco(p);
                    else UI.printEncomendaInex();
                } catch (Exception e) {UI.printEncomendaInex();}
                break;
            case 3:
                if (!this.e.disponivel(cod)) {
                    UI.printMudeDisp();
                    break;
                }
                for (Loja l : this.e.getLojas().values()) {
                    if (l.getPedidos().size()>0) {
                        if (this.e.getTrabalhadores().get(cod).isCertificada()) {
                            UI.printEncomendas(l.getPedidos());
                        } else {
                            UI.printEncomendasNormais(l.getPedidos());
                        }
                    }
                }
                UI.print0NEncomendas();
                UI.printInsiraCodEnc();
                sc.nextLine();
                String codEnc = sc.nextLine();
                if (codEnc.equals("0")) break;
                try {
                    Encomenda encomenda = this.e.removeEncomendaLoja(codEnc,cod);
                    encomenda.setEstafeta(cod);
                    this.e.addPedidoDeTransporte(cod,encomenda);
                    UI.printEncomendaEmTrans();
                } catch (Exception e) {
                    UI.printEncomendaInex();
                }
                break;
            case 4:
                UI.printTop10(e.getTop10Util().stream().map(Utilizador::getNome).collect(Collectors.toList()));
                break;
            case 5:
                UI.printTop10(e.getTop10Trans().stream().map(Estafeta::getNome).collect(Collectors.toList()));
                break;
            case 6:
                LocalDateTime dataInicial, dataFinal;
                try {
                    sc.nextLine();
                    UI.printDataInicial();
                    String inicio = sc.nextLine();
                    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                    dataInicial = LocalDateTime.parse(inicio, formatter);
                    UI.printDataFinal();
                    String fim = sc.nextLine();
                    dataFinal = LocalDateTime.parse(fim, formatter);
                } catch (DateTimeParseException ex) {
                    UI.printFormatoInvalido();
                    break;
                }
                UI.printTotFat(this.e.totalFaturado(cod,dataInicial,dataFinal));
                break;
            case 7:
                UI.printSelectRaio();
                e.setRaio(cod,sc.nextDouble());
                break;
            case 8:
                UI.printSelectPrecoKM();
                e.setPrecokms(cod,sc.nextDouble());
                break;
            case 9:
                UI.printClassMedia(e.getTrabalhadores().get(cod).getClassMedia());
                break;
            case 10:
                this.e.logoff();
                break;
            default:
                break;
        }
    }

    /**
     * Menu da loja
     */
    public void menuLoja() {
        int opcao;
        Scanner sc = new Scanner(System.in);
        UI.printMenuLoja();
        opcao = sc.nextInt();
        String cod = this.e.getLogin().getCod();
        switch(opcao) {
            case 0:
                stopExec();
                try {
                    f.saveObjectStream(e);
                }
                catch (IOException e) {
                    e.printStackTrace();
                }
                break;
            case 1:
                UI.printEncomendas(e.getLoja(e.getLogin().getCod()).getPedidos());
                break;
            case 2:
                if(e.getLogin() instanceof LojaFilaEspera) {
                    LojaFilaEspera l = (LojaFilaEspera) e.getLogin();
                    UI.printTamanhoFilaEspera(l.getTamanhoListaEspera());
                }
                else {
                    UI.printNtemFilaEspera();
                }
                break;
            case 3:
                UI.printTop10(e.getTop10Util().stream().map(Utilizador::getNome).collect(Collectors.toList()));
                break;
            case 4:
                UI.printTop10(e.getTop10Trans().stream().map(Estafeta::getNome).collect(Collectors.toList()));
                break;
            case 5:
                e.logoff();
                break;
            default:
                break;
        }
    }
}
