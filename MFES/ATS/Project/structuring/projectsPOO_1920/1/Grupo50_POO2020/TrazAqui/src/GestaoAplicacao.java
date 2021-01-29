import java.io.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class GestaoAplicacao  implements Serializable {
    private GestorUsers users;
    private GestorClientes clientes;
    private GestorLojas lojas;
    private GestorEstafetas estafetas;
    private GestorDistribuidoras empresas;
    private GestorEncomendas encomendas;

    /**
     * Construtores por Omissão, Parameterizado e de Cópia
     */

    public GestaoAplicacao(){
        this.users = new GestorUsers();
        this.clientes = new GestorClientes();
        this.lojas = new GestorLojas();
        this.estafetas = new GestorEstafetas();
        this.empresas = new GestorDistribuidoras();
        this.encomendas = new GestorEncomendas();
    }

    public GestaoAplicacao(GestorUsers unsUsers, GestorClientes unsClientes, GestorLojas umasLojas, GestorEstafetas unsEstafetas,GestorDistribuidoras umasEmpresas, GestorEncomendas umasEncomendas) {
        this.users = unsUsers;
        this.clientes = unsClientes;
        this.lojas = umasLojas;
        this.estafetas = unsEstafetas;
        this.empresas = umasEmpresas;
        this.encomendas = umasEncomendas;
    }

    public GestaoAplicacao(GestaoAplicacao umaApp){
        this.users = umaApp.getUsers();
        this.clientes = umaApp.getClientes();
        this.lojas = umaApp.getLojas();
        this.estafetas = umaApp.getEstafetas();
        this.empresas = umaApp.getEmpresas();
        this.encomendas = umaApp.getEncomendas();
    }

    /**
     * Método de Clone
     * @return GestãoAplicacao
     */

    public GestaoAplicacao clone(){
        return new GestaoAplicacao(this);
    }

    /**
     * Getters & Setters
     */

    public GestorUsers getUsers() {
        return this.users;
    }

    public GestorClientes getClientes() {
        return this.clientes;
    }

    public GestorLojas getLojas() {
        return this.lojas;
    }

    public GestorEstafetas getEstafetas() {
        return this.estafetas;
    }

    public GestorDistribuidoras getEmpresas() {
        return this.empresas;
    }

    public GestorEncomendas getEncomendas() {
        return this.encomendas;
    }

    public void setUsers(GestorUsers novosUsers) {
        this.users = novosUsers;
    }

    public void setClientes(GestorClientes novosClientes) {
        this.clientes = novosClientes;
    }

    public void setLojas(GestorLojas novasLojas) {
        this.lojas = novasLojas;
    }

    public void setEstafetas(GestorEstafetas novosEstafetas) {
        this.estafetas = novosEstafetas;
    }

    public void setEmpresas(GestorDistribuidoras novasEmpresas) {
        this.empresas = novasEmpresas;
    }

    public void setEncomendas(GestorEncomendas novasEncomendas) {
        this.encomendas = novasEncomendas;
    }


    /**Outros Métodos**/

    public String registarUtilizador(String umUsername, String umEmail, String umaPassword){
        User newUser = new User(umUsername, umEmail, umaPassword);
        try {
            users.addUser(newUser);
        } catch (UserJaExisteException e) {
            return "User já existe";
        }
        return "Utilizador " + newUser.toString() + " registado!";
    }

    /* --------------- Admin --------------- */
    public int getNumeroEntidade(String tipoEntidade){
        switch (tipoEntidade){
            case "User":
                return this.users.tamanho();
            case "Cliente":
                return  this.clientes.tamanho();
            case "Estafeta":
                return this.estafetas.tamanho();
            case "Loja":
                return this.lojas.tamanho();
            case "Transportadora":
                return this.empresas.tamanho();
            case "Encomenda":
                return this.encomendas.tamanho();
            default:
                return 0;
        }
    }

    public double getProfitsEmpresa(String codigoEmpresa){
        try {
            Distribuidora t = this.empresas.getDistribuidora(codigoEmpresa);
            List<String> funcionarios = t.getEstafetas();
            List<String> encomendas = new ArrayList<>();
            for(String cod : funcionarios) {
                encomendas.addAll(this.encomendas.getEncomendasEstafeta(cod));
            }
            List<Encomenda> encs = new ArrayList<>();
            for(String codigoEncomenda : encomendas){
                encs.add(this.encomendas.getEncomenda(codigoEncomenda));
            }
            return  encs.stream()
                    .mapToDouble(Encomenda::getValor)
                    .sum();
        } catch (EntidadeInexistenteException | EncomendaInexistenteException | NullPointerException e) {
            e.getMessage();
        }
        return 0;
    }

    public List<Distribuidora> getXDistribuidorasProfit(int n) {

        List<Distribuidora> dists = this.empresas.getDistribuidoras().values()
                .stream()
                .sorted((e2, e1) -> (int) (getProfitsEmpresa(e1.getCodigo()) - getProfitsEmpresa(e2.getCodigo()))) //ordena de maneira decrescente e2-e1
                .map(Distribuidora::clone)
                .collect(Collectors.toList());

        return dists.stream().limit(n).collect(Collectors.toList());
    }

    /* --------------- Users --------------- */

    /**
     *
     * @param username Código de registo de um usuário da TrazAqui
     * @param tipoEntidade Tipo de entidade que se procura (Cliente, Estafeta, Loja, Transportadora)
     * @return
     */
    public List<String> getEntidade(String username, String tipoEntidade){
        List<String> entidade = new ArrayList<>();

        switch (tipoEntidade){
            case "Cliente":
                String cliente;
                try {
                    cliente = this.users.getUser(username).getCliente();
                    entidade.add(cliente);
                    return cliente.equals("n/a") ? null : entidade;
                } catch (UserInexistenteException e) {
                    e.getMessage();
                }
                break;

            case "Estafeta":
                String estafeta;
                try {
                    estafeta = this.users.getUser(username).getEstafeta();
                    entidade.add(estafeta);
                    return estafeta.equals("n/a") ? null : entidade;
                } catch (UserInexistenteException e) {
                    e.getMessage();
                }
                break;

            case "Loja":
                List<String> lojas;
                try {
                    lojas = this.users.getUser(username).getLojas();
                    entidade.addAll(lojas);
                    return lojas.size() == 0 ? null: entidade;
                } catch (UserInexistenteException e) {
                    e.getMessage();
                }
                break;

            case "Transportadora":
                List<String> transportadoras;
                try {
                    transportadoras = this.users.getUser(username).getLojas();
                    entidade.addAll(transportadoras);
                    return transportadoras.size() == 0 ? null : entidade;
                } catch (UserInexistenteException e) {
                    e.getMessage();
                }
                break;

            default:
                break;
        }
            return null;
    }

    public List<String> getEncomendas(String codigoEntidade, String tipoEntidade, int estadoEncomenda){
        switch (tipoEntidade){
            case "Cliente":
                return new ArrayList<>();
            case "Estafeta":
                return new ArrayList<>();
            case "Loja":
                if(estadoEncomenda >= 0 && estadoEncomenda <=4){
                    try {
                        Loja l = this.lojas.getLoja(codigoEntidade);
                        List<Encomenda> encs = this.encomendas.getHistoricoLoja(codigoEntidade, estadoEncomenda);
                        return encs.stream().map(Encomenda::getCodigo).collect(Collectors.toList());
                    } catch (EntidadeInexistenteException e) {
                        e.getMessage();
                    }
                }
        }
        return null;
    }

    public void criaCliente(String username, String nome, double lat, double lon){
        String codigoCliente = "u" + (this.clientes.numClientes() + 1);
        GPS localizacao = new GPS(lat, lon);

        Cliente c = new Cliente(codigoCliente, nome, localizacao);
        try {
            this.users.getUser(username).setCliente(codigoCliente);
            System.out.println("Estou ligado a um user");
            System.out.println("User " + this.users.getUser(username).getCliente());
            this.clientes.addCliente(c);
        } catch (UserInexistenteException | EntidadeRepetidaException e) {
            e.getMessage();
        }
    }

    public void criaEstafeta(String username, String nome, double lat, double lon, double raio, boolean certificado){
        String codigoEstafeta = "e" + (this.estafetas.numEstafetas() + 1);
        GPS localizacao = new GPS(lat, lon);

        Estafeta e = new Estafeta(codigoEstafeta, nome, localizacao, raio, certificado, true);
        try{
            this.users.getUser(username).setEstafeta(codigoEstafeta);
            this.estafetas.addEstafeta(e);
        } catch (UserInexistenteException | EntidadeRepetidaException ex){
            ex.getMessage();
        }
    }

    public void criaLoja(String username, String nome, double lat, double lon){
        String codigoLoja = "l" + (this.lojas.numLojas() + 1);
        GPS localizacao = new GPS(lat, lon);

        Loja l = new Loja(codigoLoja, nome, localizacao, new ArrayList<>(), new HashMap<>());
        try{
            this.users.getUser(username).adicionaLoja(l.getCodigo());
            this.lojas.addLoja(l);
        } catch (UserInexistenteException | EntidadeRepetidaException e){
            e.getMessage();
        }
    }

    public void criaTransportadora(String username, String nome, double lat, double lon, String nif, double raio, float taxa){
        String codigoTransportadora = "t" + (this.empresas.numDistribuidoras() + 1);
        GPS localizacao = new GPS(lat, lon);

        Distribuidora t = new Distribuidora(codigoTransportadora, nome, localizacao, nif, raio, taxa, new ArrayList<>());
        try{
            this.users.getUser(username).adicionaDistribuidora(t.getCodigo());
            this.empresas.addDistribuidora(t);
        } catch(UserInexistenteException | EntidadeRepetidaException e){
            e.getMessage();
        }
    }

    public Produto criaProduto(String loja, String codigoProduto){
        try {
            Loja l = this.lojas.getLoja(loja);
            List<Produto> produtos = l.getInventario();
            for(Produto p : produtos)
                if(p.getCodigo().equals(codigoProduto))
                    return p;

        } catch (EntidadeInexistenteException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void criaEncomenda(String cliente, String loja, Map<String, Integer> produtos){
        Encomenda enc = new Encomenda();
        enc.setCodigo("e" + (this.encomendas.numEncomendas() + 1));
        enc.setDestinatario(loja);
        List<Produto> prods= new ArrayList<>();
        for(String cod : produtos.keySet()){
            Produto p = criaProduto(loja, cod);
            if(p != null)
                p.setQuantidade(produtos.get(cod));
        }

        enc.setProdutos(prods);
    }

    public String getInfo(String codigoEntidade, String tipoEntidade){
        switch(tipoEntidade){
            case "Cliente":
                try {
                    return this.clientes.getCliente(codigoEntidade).toString();
                } catch (EntidadeInexistenteException | NullPointerException e) {
                    e.getMessage();
                }
                break;
            case "Estafeta":
                try{
                    return this.estafetas.getEstafeta(codigoEntidade).toString();
                } catch (EntidadeInexistenteException | NullPointerException e) {
                    e.getMessage();
                }
                break;
            case "Loja":
                try {
                    return this.lojas.getLoja(codigoEntidade).toString();
                } catch (EntidadeInexistenteException | NullPointerException e) {
                    e.printStackTrace();
                }
                ;
                break;
            case "Transportadora":
                try {
                    return this.empresas.getDistribuidora(codigoEntidade).toString();
                } catch (EntidadeInexistenteException | NullPointerException e) {
                    e.getMessage();
                }
                break;
            case "Encomenda":
                try {
                    return this.encomendas.getEncomenda(codigoEntidade).toString();
                } catch (EncomendaInexistenteException | NullPointerException e) {
                    e.getMessage();
                }
                break;
        }
        return "Sem informação!";
    }

    public List<String> getHistorico(String codigoEntidade, String tipoEntidade){
        switch (tipoEntidade){
            case "Cliente":
                break;
            case "Estafeta":
                return this.encomendas.getEncomendasEstafeta(codigoEntidade);
            case "Loja":
                List<Encomenda> encs = this.encomendas.getHistoricoLoja(codigoEntidade, 4);
                List<Encomenda> tempos =  encs.stream()
                                                .sorted((e2, e1) -> (e1.getDespachada()).isAfter(e2.getDespachada()) ? 0 : 1)
                                                .collect(Collectors.toList());
                List<String> encomendasComTempo = new ArrayList<>();
                for(Encomenda enc : tempos){
                    encomendasComTempo.add(enc.getCodigo() + " -> " + enc.getDespachada());
                }
                return encomendasComTempo;
        }
        return null;
    }

    public List<String> getFuncionarios(String codigoTransportadora){
        try {
            return this.empresas.getDistribuidora(codigoTransportadora).getEstafetas();
        } catch (EntidadeInexistenteException e) {
            e.getMessage();
        }
        return null;
    }

    public List<String> getFuncionarios(String codigoTransportadora, String ordem){
        return null;
    }

    public List<String> getFuncionariosMed(String codigoTransportadora) {
        List<String> certificados = new ArrayList<>();
        try {
            List<String> estafetas = this.empresas.getDistribuidora(codigoTransportadora).getEstafetas();
            for (String cod : estafetas) {
                Estafeta v = this.estafetas.getEstafeta(cod);
                if (v.isCertificado())
                    certificados.add(v.getCodigo());
            }
            return certificados;

        } catch (EntidadeInexistenteException e) {
            e.getMessage();
        }
        return certificados;
    }

    public String alterarEstadoEncomenda(String codigoEntidade, String codigoEncomenda, String tipoEntidade){
        switch (tipoEntidade){
            case "Cliente":
                try {
                    this.encomendas.getEncomenda(codigoEncomenda).alteraEstado();
                    return "Encomenda aceite com sucesso!";
                } catch (EncomendaInexistenteException e) {
                    e.getMessage();
                }
            case "Estafeta":
                try {
                    Encomenda enc = this.encomendas.getEncomenda(codigoEncomenda);
                    Estafeta v = this.estafetas.getEstafeta(codigoEntidade);
                    enc.setEstafeta(v.getCodigo());
                    if(v.isVoluntario() && enc.getEstado() == 0){
                        enc.alteraEstado();
                        return "Encomenda aceite com sucesso";
                    }
                } catch (EncomendaInexistenteException | EntidadeInexistenteException e ) {
                    e.printStackTrace();
                }
                break;

            case "Loja":
                try{
                    Encomenda enc = this.encomendas.getEncomenda(codigoEncomenda);
                    if(enc.getEstado() == 1) {
                        enc.alteraEstado();
                        return "Encomenda Pronta!";
                    }
                    if(enc.getEstado() == 2){
                        enc.alteraEstado();
                        return "Encomenda Despachada!";
                    }
                } catch (EncomendaInexistenteException e) {
                    e.getMessage();
                }
            default:
                break;
        }
        return "Algo correu mal, tente de novo!";
    }

    public List<String> historicoEncomendasPeriodo(String codigoCliente, int ano, int mes){
        LocalDateTime periodo = LocalDateTime.of(ano, mes, 1, 0, 0);

        return encomendas.historicoEncomendas(periodo, codigoCliente)
                .stream().map(Encomenda::getCodigo).collect(Collectors.toList());
    }

    public List<String> getProdutos(String codLoja){
        List<String> prod = new ArrayList<>();
        try {
            Loja l = this.lojas.getLoja(codLoja);
            return l.getInventario().stream().map(Produto::toString).collect(Collectors.toList());

        } catch (EntidadeInexistenteException | NullPointerException e) {
            e.getMessage();
        }

        return prod;
    }


    /* --------------- Parsing ---------------*/
    public void registarUser(User u) throws UserJaExisteException {
        this.users.addUser(u);
    }

    public void registarEstafeta(Estafeta umEstafeta) throws EntidadeRepetidaException {
        this.estafetas.addEstafeta(umEstafeta);
    }

    public void registarLoja(Loja umaLoja) throws EntidadeRepetidaException{
        this.lojas.addLoja(umaLoja);
    }

    public void registarEmpresa(Distribuidora umaEmpresa) throws EntidadeRepetidaException{
        this.empresas.addDistribuidora(umaEmpresa);
    }

    public void registarCliente(Cliente umCliente) throws  EntidadeRepetidaException{
        this.clientes.addCliente(umCliente);
    }

    public void registarEncomenda(Encomenda umaEncomenda){
        this.encomendas.addEncomenda(umaEncomenda);
    }

    public List<String> lojasAbertas(){
        //implementar horário de funcionamento
        return this.lojas.getLojaAsList().stream().map(Loja::getCodigo).collect(Collectors.toList());
    }


    /* --------------- Clientes --------------- *//*
    public List<String> lojasPertoCliente(double raio){
        List<String> lojas = new ArrayList<>();
        try {
            String codigoCliente = this.users.getUser(username).getCliente();
            GPS localizacao = this.clientes.getCliente(codigoCliente).getLocalizacao();
            return this.lojas.lojasPeriferia(localizacao, raio)
                       .stream().map(Loja::getCodigo).collect(Collectors.toList());

        } catch (UserInexistenteException | EntidadeInexistenteException ignored) {

        }
        return lojas;
    }

    public List<String> historicoEncomendas(){
        String codigoCliente = "";
        try {
            codigoCliente = this.users.getUser(username).getCliente();
        } catch (UserInexistenteException e) {
            System.out.println("Não existe um cliente");
        }
        return this.encomendas.getEncomendasCliente(codigoCliente)
                .stream().map(Encomenda::getCodigo).collect(Collectors.toList());
    }
*/

    /* --------------- Estafetas --------------- */
    public List<String> getPertoEstafeta(String codigoEstafeta, String tipoEntidade){

        try {
            Estafeta e = this.estafetas.getEstafeta(codigoEstafeta);
            GPS localizacao = e.getLocalizacao();
            double raio = e.getRaio();

            switch (tipoEntidade) {
                case "Loja":
                    return this.lojas.lojasPeriferia(localizacao, raio)
                            .stream()
                            .map(Entidade::getCodigo)
                            .collect(Collectors.toList());
                case "Cliente":
                    return this.clientes.clientesPeriferia(localizacao, raio)
                            .stream()
                            .map(Entidade::getCodigo)
                            .collect(Collectors.toList());
                case "Encomenda":
                    return this.encomendas.getPedidosPeriferia(e.getCodigo(), e.getLocalizacao(), e.getRaio(), e.isCertificado());
            }
        } catch (EntidadeInexistenteException e) {
            e.getMessage();
        }
        return null;
    }



    /* --------------- Encomendas --------------- */


    public void encomendasRaio(GPS localizacao, double raio){
        this.encomendas.getEncomedasRaio(localizacao, raio);
    }

    /* --------------- Lojas --------------- */


    public List<String> nomesLojas(List<String> codigos){
       List <String> nomes = new ArrayList<>();
        for(String cod : codigos){
           try{
               nomes.add(this.lojas.getLoja(cod).getNome());
           } catch (EntidadeInexistenteException e) {
               System.out.println("Não existe uma loja com o código: " + cod);
           }
        }
        return nomes;
    }

    /* --------------- Lojas --------------- */

/*
    /**
     * Estado Aplicação
     */
    /**
     * Método que valida login e guarda o username que está autenticado
     * @param username Username para ser autenticado
     * @param password Password para ser autenticada
     * @return True caso login seja válido
     * @throws UserInexistenteException caso não exista nenhum username
     */


    public boolean login(String username, String password) throws UserInexistenteException{
        try{
            return password.equals(this.users.getUser(username).getPassword());
        }
        catch(NullPointerException e){
            throw new UserInexistenteException("Não existe um User com o email " + username + " registado na aplicação!");
        }
    }

    public void gravarObj(String nFich) throws IOException{
        ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(nFich));

        oos.writeObject(this);

        oos.flush();
        oos.close();
    }

    public static GestaoAplicacao lerObj(String nFich) throws IOException, ClassNotFoundException {
        ObjectInputStream ois = new ObjectInputStream(new FileInputStream(nFich));

        GestaoAplicacao g = (GestaoAplicacao) ois.readObject();

        ois.close();
        return g;
    }


    /**
     * • o estado da aplicação deverá estar pré-carregado com um conjunto de dados significativos,
     * que permita testar toda a aplicação no dia da entrega.
     * registar um utilizador, um voluntário, uma empresa transportadora e uma loja;
     * • validar o acesso à aplicação utilizando as credenciais (email e password);
     * • inserir pedidos de encomendas a uma loja, por parte de um utilizador;
     * • inserir informação de encomenda pronta a ser entregue, por parte das lojas;
     * • indicar que se pretende transportar a encomenda. No caso de ser uma empresa transportadora
     * o utilizador que solicitou o serviço terá de aprovar que aceita o preço. Tanto voluntários como
     * empresas apenas podem fazer transporte de encomendas de acordo com o raio de acção que
     * tiverem definido;
     * • classificar, por parte do utilizador, o serviço de entrega;
     * • ter acesso no perfil de cada uma das entidades à informação sobre as encomendas transportadas (em função de data/hora de transporte);
     * • indicar o total facturado por uma empresa transportadora num determinado período;
     * • determinar a listagens dos 10 utilizadores que mais utilizam o sistema (em número de encomendas transportadas);
     * • determinar a listagens das 10 empresas transportadoras que mais utilizam o sistema (em
     * número de kms percorridos);
     * • gravar o estado da aplicação em ficheiro, para que seja possível retomar mais tarde a execução.
     */
































    /**Users**/
/*    public boolean existeUser(String email){
        return this.users.containsKey(email);
    }

    public int quantosUsers(){
        return this.users.size();
    }

    public void criarUtilizador(String umNome, String umEmail, String umaPassword) throws UserJaExisteException {
        User u = new User(umNome, umEmail, umaPassword);
        registaUtilizador(u);

    }

    public void registaUtilizador(User u) throws UserJaExisteException{
        if(this.users.putIfAbsent(u.getName(),u.clone())!=null)
            throw new UserJaExisteException("Já existe " + u.getClass().getSimpleName() + " com " + u.getName() + " na aplicação!");
    }


*/
    /**Clientes**/
 /*   public void registaCliente(User u, Cliente c) throws EntidadeRepetidaException{
        if(this.clientes.put(c.getCodigo(), c) != null)
            throw new EntidadeRepetidaException("Já existe um cliente com o código " + c.getCodigo() + " na aplicação!");
        else{
            this.users.get(u.getEmail()).setCliente(c.getCodigo());
        }
    }
    
    public Cliente getCliente(String codigoCliente) throws EntidadeInexistenteException {
        return this.clientes.getCliente(codigoCliente);
    */
    /**Lojas**/
    /*
    public void registaLoja(User u, Loja l) throws EntidadeRepetidaException { //[Exception] - User Inexistente | Loja já existente
        if (this.lojas.putIfAbsent(l.getCodigo(), l.clone()) != null)
            throw new EntidadeRepetidaException("Já existe uma loja com o código " + l.getCodigo() + " na aplicação!");
        else {
            this.users.get(u.getEmail()).adicionaLoja(l.getCodigo());
        }
    }*/

    /** Estafetas**/
 /*   public void registaEstafeta(User u, Estafeta e) throws EntidadeRepetidaException {
        if (this.estafetas.putIfAbsent(e.getCodigo(), e.clone()) != null)
            throw new EntidadeRepetidaException("Já existe um estafeta com o código " + e.getCodigo() + " na aplicação!");
        else {
            this.users.get(u.getEmail()).setEstafeta(e.getCodigo());
        }
    }

    public List<Encomenda> getEntregasEstafeta(String codEstafeta){
        return this.encomendas.values()
                            .stream()
                            .filter(enc -> enc.getEstafeta().equals(codEstafeta))
                            .map(Encomenda::clone)
                            .collect(toList());
    }

    public Estafeta getEstafeta(String codigoEstafeta){
        return this.estafetas.get(codigoEstafeta);
    }

    /**Empresas**/
 /*   public void registaEmpresa(User u, Distribuidora d) throws EntidadeRepetidaException{
        if(this.empresas.putIfAbsent(d.getCodigo(), d.clone()) != null)
            throw new EntidadeRepetidaException("Já existe uma loja com o código " + d.getCodigo() + " na aplicação!");
        else{
            this.users.get(u.getEmail()).adicionaDistribuidora(d.getCodigo());
        }
    }

    public Distribuidora getEmpresa(String codEmpresa){
        return this.empresas.get(codEmpresa).clone();
    }

    public List<Encomenda> getEntregasEmpresa(String codigoEmpresa){
        List <Encomenda> encomendas = new ArrayList<>();

        for(String codigoEstafeta : this.getEmpresa(codigoEmpresa).getEstafetas())
            encomendas.addAll(this.getEntregasEstafeta(codigoEstafeta));

        return encomendas;
    }

    public double getProfitsEmpresa(String codigoEmpresa){
        return this.getEntregasEmpresa(codigoEmpresa).stream()
                                                    .mapToDouble(Encomenda::getValor)
                                                    .sum();
    }

    public List<Distribuidora> getXDistribuidorasProfit(int n) {

        List<Distribuidora> dists = this.empresas.values()
                .stream()
                .sorted((e2, e1) -> (int) (getProfitsEmpresa(e1.getCodigo()) - getProfitsEmpresa(e2.getCodigo()))) //ordena de maneira decrescente e2-e1
                .map(Distribuidora::clone)
                .collect(toList());

        return dists.stream().limit(n).collect(Collectors.toList());
    }


    /**Encomendas**/
  /*  public List<Encomenda> getPedidos(){
        return this.encomendas.values()
                            .stream()
                            .filter(encomenda -> encomenda.getEstado() == 0)
                            .collect(toList());
    }

    public void registaEncomenda(Encomenda e){
        this.encomendas.putIfAbsent(e.getCodigo(), e.clone());
    }
    
    public Encomenda getEncomenda(String codigoEncomenda){
        return this.encomendas.get(codigoEncomenda);
    }
    
    public GPS atualizaLocalizacaoEncomenda(String codigoEncomenda){
        Encomenda e = getEncomenda(codigoEncomenda);
        GPS localizacao =new GPS();
        switch (e.getEstado()){
            case 0:
            case 1:
            case 2:
               // localizacao = this.getLoja(e.getEstabelecimento()).getLocalizacao();
                break;
            case 3:
                localizacao = this.getEstafeta(e.getEstafeta()).getLocalizacao();
                break;
            case 4:
                localizacao = this.getCliente(e.getDestinatario()).getLocalizacao();
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + e.getEstado());
        }

        return localizacao;
    }


     */



}
