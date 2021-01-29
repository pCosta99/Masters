import com.sun.javafx.collections.MappingChange;
import javafx.print.Collation;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.io.*;
public class TrazAqui implements Serializable{
    /**
     * Nome do user
     */
    private String nome;
    /**
     * mapeamento do nome do User para User
     */
    private Map<String,User> users;

    private Map<String, Encomenda> encomendas;

    private List<String> aceites;

    /**
     * Construtor Vazio
     */
    public TrazAqui()
    {
        this.nome = "TrazAqui";
        this.users = new HashMap<String,User>();
        this.encomendas = new HashMap<String,Encomenda>();
        this.aceites = new ArrayList<String>();
    }

    /**
     * Construtor por cópia
     */
    public TrazAqui(TrazAqui c)
    {
        this.nome = c.getNome();
        this.users = c.getUsers();
        this.encomendas = c.getEncomendas();
        this.aceites = c.getAceites();
    }

    /**
     * Construtor por parametros
     */
    public TrazAqui(String nome, Map<String,User> users, Map<String,Encomenda> encomendas, List<String> aceites)
    {
        this.nome = nome;
        //this.users = new HashMap<String,User>();
        setUsers(users);
        setEncomendas(encomendas);
        setAceites(aceites);
    }

    public String getNome()
    {
        return this.nome;
    }

    public void setNome(String nome)
    {
        this.nome = nome;
    }

    private Map<String,User> getUsers()
    {
        return this.users.entrySet()
            .stream()
            .collect(Collectors.toMap(e->e.getKey(), e->e.getValue().clone()));
    }

    private void setUsers(Map<String,User> users)
    {
        this.users = new HashMap<>();
        this.users = users.entrySet()
            .stream()
            .collect(Collectors.toMap(e->e.getKey(),e->e.getValue().clone()));
    }

    private Map<String,Encomenda> getEncomendas()
    {
        return this.encomendas.entrySet()
                .stream()
                .collect(Collectors.toMap(e->e.getKey(), e->e.getValue().clone()));
    }

    private void setEncomendas(Map<String,Encomenda> encomendas)
    {
        this.encomendas = new HashMap<>();
        this.encomendas = encomendas.entrySet()
                .stream()
                .collect(Collectors.toMap(e->e.getKey(),e->e.getValue().clone()));
    }

    public List<String> getAceites() {
        return new ArrayList<>(this.aceites);
    }

    public void setAceites(List<String> aceites) {
        this.aceites = new ArrayList<>(aceites);
    }

    public void existeUser(String nome, String password) throws UserInexistenteException {
        if(!this.users.containsKey(nome) || !corretaPassword(nome,password)) {
            throw new UserInexistenteException("Username ou password errados");
        }
    }

    public void existeUsername(String username) throws UserInexistenteException {
        if(this.users.containsKey(username)){
            throw new UserInexistenteException("Username já existe!");
        }
    }

    public boolean corretaPassword(String username, String password){
        return this.users.get(username).getPassword().equals(password);
    }

    public int quantos()
    {
        return this.users.size();
    }

    public boolean temFila(String username) throws UserInexistenteException {
        if(!this.users.containsKey(username)){
            throw new UserInexistenteException("Loja não existe!");
        }
        else{
            User u = this.users.get(username);
            return u instanceof LojaComFila;
        }
    }

    public Encomenda getEncomenda(String username) throws EncomendaInexistenteException {
        Encomenda e;
        if(!encomendas.containsKey(username))
        {
            throw new EncomendaInexistenteException("A encomenda não existe");
        }
        e = encomendas.get(username).clone();
        return e;
    }

    public void adicionaEncFila(String username,String cod) throws EncomendaInexistenteException {
        ((LojaComFila) this.users.get(username)).addEncFila(this.getEncomenda(cod));
    }

    public void adicionaEncLoja(String username, String cod) throws EncomendaInexistenteException {
        ((Loja) this.users.get(username)).addEncomenda(this.getEncomenda(cod));
    }

    public void retiraEncFila(String username, String cod){
        ((LojaComFila) this.users.get(username)).retiraEncFila();
    }

    public User getUser(String nome) throws UserInexistenteException
    {
        User h;
        if(!users.containsKey(nome))
        {
            throw new UserInexistenteException(nome);
        }
        h = users.get(nome).clone();
        return h;
    }

    public boolean eEmpresa(String username){
        if(this.users.get(username) instanceof Empresa){
            return true;
        }
        return false;
    }

    public void realizaEncomenda(String codEnc, String usernameV){
        this.encomendas.get(codEnc).setTransportador(usernameV);
        ((Loja) this.users.get(this.encomendas.get(codEnc).getLoja())).removeEncomenda(codEnc);
        ((Transportador) this.users.get(usernameV)).addEncomenda(this.encomendas.get(codEnc).clone());
        this.aceites.remove(this.encomendas.get(codEnc).getCodigo());
        this.encomendas.get(codEnc).setPropostas(new ArrayList<>());
    }

    public void realizaProposta(String cod, String username){
        if(!(this.encomendas.get(cod).getPropostas().contains(username))){
            this.encomendas.get(cod).adicionaProposta(this.users.get(username).getUsername());
        }
        //System.out.println(this.encomendas.get(cod));
    }

    public void confirmarEntEncomenda(String cod, String username){
        this.encomendas.get(cod).setEntregue(true);
        this.encomendas.get(cod).setHoraF(LocalDateTime.now());
        ((Transportador) this.users.get(username)).removeEncomenda(cod);
    }

    public static Localizacao criaLocalizacao(double x, double y){
        return new Localizacao(x,y);
    }

    public static Utilizador criaUtilizador(String username, String nome, String password,Localizacao local, int idade, String sexo){
        return new Utilizador(username,nome,password,local,idade,sexo);
    }

    public static Voluntario criaVoluntario(String username, String nome, String password,Localizacao posicao,double raio,
                                     boolean transport, boolean transporte_medico, Map<String,Integer> classificacao, HashMap<String,Encomenda> encomendas, int idade, String sexo){
        return new Voluntario(username,nome,password,posicao,raio,transport,transporte_medico,classificacao,encomendas,idade,sexo);
    }
    public static Empresa criaEmpresa(String username, String nome, String password,Localizacao posicao,double raio,
                                  boolean transport, boolean transporte_medico,
                                  Map<String,Integer> classificacao, HashMap<String,Encomenda> encomendas,
                                  double custo_km, double custo_peso, String nif){
        return new Empresa(username,nome,password,posicao,raio,transport,transporte_medico,classificacao,encomendas,custo_km,custo_peso,nif);
    }

    public static Loja criaLoja(String username, String nome,String password,Localizacao local,HashMap<String,Encomenda> encomendas,
                                HashMap<String,Integer> classificacao,boolean fila,int tamanho){
        if(fila == true){
            return new LojaComFila(username,nome,password,local,encomendas,classificacao,new ArrayList<>(),tamanho);
        }
        else{
            return new LojaSemFila(username,nome,password,local,encomendas,classificacao);
        }
    }

    public String retEncFilaCod(String username) throws UserInexistenteException {
        return ((LojaComFila) this.getUser(username)).retiraEncFilaCod();
    }

    public void verificaAceites(){
        for(String s: this.getAceites()){
            Encomenda e = this.getEncomendas().get(s);
            if(((Loja) this.getUsers().get(e.getLoja())).getEncomendas().containsKey(s) == false && this.getUsers().get(e.getLoja()) instanceof LojaSemFila) {
                ((LojaSemFila) this.users.get(e.getLoja())).addEncomenda(e);
            }
            else {
                if(this.getUsers().get(e.getLoja()) instanceof LojaComFila) {
                    if (((LojaComFila) this.getUsers().get(e.getLoja())).getFila().contains(e) == false) {
                        ((LojaComFila) this.users.get(e.getLoja())).addEncFila(e);
                    }
                }
            }
        }
    }

    public void classifTransportador(String username, String cod, int classi) throws EncomendaInexistenteException, UserInexistenteException {
        Transportador t = (Transportador) this.users.get(this.getEncomenda(cod).getTransportador());
        if(!(t.getClassificacoes().containsKey(username))) {
            t.addClassificacao(username, classi);
        }
        else{
            throw new UserInexistenteException("O utilizador já classificou");
        }
    }

    public void classifLoja(String username, String cod, int classi) throws EncomendaInexistenteException, UserInexistenteException {
        Loja l = (Loja) this.users.get(this.getEncomenda(cod).getLoja());
        if(!(l.getClassificacao().containsKey(username))) {
            l.addClassificacao(username, classi);
        }
        else{
            throw new UserInexistenteException("O utilizador já classificou");
        }
    }

    public boolean contemLoja(String username){
        if (this.users.containsKey(username) && this.users.get(username) instanceof Loja){
            return this.listOfLojas().stream().filter(o->o.getUsername().equals(username)).findFirst().isPresent();
        }
        else{
            return false;
        }
    }



    public String geraCodEncomenda(){
        String cd = Collections.max(this.getEncomendas().keySet());
        String codigo = "e" + (Integer.parseInt(cd.substring(1,cd.length()))+1);
        return codigo;
    }

    public Encomenda criaEncomenda(String codigo, String utilizador, String loja, String transportador, LocalDateTime ldt, double peso, String[][] produtos, int ind,boolean medica, boolean entregue){
        int i = 0;
        List<LinhaEncomenda> lst = new ArrayList<>();
        while(i < ind){
            lst.add(criaLinhaEncomenda(produtos[i][0],produtos[i][1],Double.parseDouble(produtos[i][2]),Integer.parseInt(produtos[i][3])).clone());
            i += 1;
        }
        return new Encomenda(codigo,utilizador,loja,transportador,ldt,LocalDateTime.now(),peso,lst,new ArrayList<>(),medica,entregue);
    }

    public String maiorEncomenda(){
        return Collections.max(this.getEncomendas().keySet());
    }

    public String maiorUser(){
        return Collections.max(this.getUsers().keySet());
    }

    public String user2Vol(String username, Boolean medica, double raio,double latitude,double longitude) throws UserInexistenteException {
        Utilizador u = (Utilizador) this.getUser(username).clone();
        String codv = "v" + (Integer.parseInt(this.maiorUser().substring(1,username.length()))+1);
        Voluntario v = new Voluntario(codv,u.getNome(),u.getPassword(),new Localizacao(latitude,longitude),raio,true,medica,new HashMap<>(),new HashMap<>(),u.getIdade(),u.getSexo());
        this.adicionaUser(v);
        return codv;
    }

    public LinhaEncomenda criaLinhaEncomenda(String ref, String descricao, double preco, int quantidade){
        return new LinhaEncomenda(ref,descricao,preco,quantidade);
    }

    public void adicionaUser(User h)
    {
        this.users.put(h.getUsername(), h.clone());
    }

    public void adicionaEncomenda(Encomenda e){ this.encomendas.put(e.getCodigo(),e.clone());}

    public void adicionaAceites(String s){
        if(!this.aceites.contains(s))
            this.aceites.add(s);
    }

    public List<User> getUsersAsList()
    {
        return users.values()
            .stream()
            .map(User::clone)
            .collect(Collectors.toList());
    }

    public List<Loja> listOfLojas(){
        List<Loja> l = new ArrayList<>();
        for(User u: this.getUsersAsList()) {
            if(u instanceof Loja) {
                if(!(u instanceof LojaComFila && ((LojaComFila) u).getFila().size() >= ((LojaComFila) u).getTamanho()))
                    l.add(((Loja) u).clone());
            }
        }
        return l;
    }

    public List<Encomenda> listOfEncomendas(String username){
        List<Encomenda> l = new ArrayList<>();
        Transportador t = (Transportador) this.users.get(username).clone();
        for(Encomenda e: this.getEncomendas().values()) {
            Localizacao l1 = this.users.get(e.getUtilizador()).getPosicao();
            Localizacao l2 = this.users.get(e.getLoja()).getPosicao();
            //System.out.println(t.dentroRaio(l1) + ";" + t.dentroRaio(l2) + ";" + !(!t.getTransporteMedico() && e.getMedica()) + " " + e.getTransportador().equals("") + ((Loja) this.users.get(e.getLoja())).getEncomendas().containsKey(e.getCodigo()) + e.getCodigo());
            if(t.dentroRaio(l1) && t.dentroRaio(l2) && !(!t.getTransporteMedico() && e.getMedica()) && e.getTransportador().equals("") && ((Loja) this.users.get(e.getLoja())).getEncomendas().containsKey(e.getCodigo())) {
                l.add(e.clone());
            }
        }
        return l;
    }

    public List<Encomenda> listOfEncomendasHistorico(String username){
        List<Encomenda> l = new ArrayList<>();
        for(Encomenda e: this.getEncomendas().values()){
            if(username.charAt(0) == 'u' && this.encomendas.get(e.getCodigo()).getUtilizador().equals(username)){
                l.add(e.clone());
            }
            if((username.charAt(0) == 'v' || username.charAt(0) == 't') && this.encomendas.get(e.getCodigo()).getTransportador().equals(username)){
                l.add(e.clone());
            }
            if(username.charAt(0) == 'l' && this.encomendas.get(e.getCodigo()).getLoja().equals(username)){
                l.add(e.clone());
            }
        }
        return l;
    }

    public List<Encomenda> listOfEncomendasPorEntregar(String username){
        List<Encomenda> l = new ArrayList<>();
        for(Encomenda e: this.getEncomendas().values()){
            if(e.getUtilizador().equals(username) && e.getEntregue() == false){
                l.add(e.clone());
            }
        }
        return l;
    }

    public List<Encomenda> listOfEncomendasPorAceitar(String username){
        List<Encomenda> l = new ArrayList<>();
        for(Encomenda e: this.getEncomendas().values()){
            if(this.getAceites().contains(e.getCodigo()) == false && e.getLoja().equals(username)){
                l.add(e.clone());
            }
        }
        return l;
    }

    public List<Encomenda> listOfEncomendasFila(String username){
        List<Encomenda> l = new ArrayList<>();
        LojaComFila lf = (LojaComFila) this.users.get(username);
        for(Encomenda e: this.getEncomendas().values()){
            //System.out.println(lf.getFila().contains(e));
            if(lf.getFila().contains(e) && e.getLoja().equals(username)){
                l.add(e.clone());
            }
        }
        return l;
    }

    public List<Encomenda> listOfEncomendasProntas(String username){
        List<Encomenda> l = new ArrayList<>();
        Loja lf = (Loja) this.users.get(username);
        for(Encomenda e: this.getEncomendas().values()){
            if(lf.getEncomendas().containsKey(e.getCodigo()) && e.getLoja().equals(username)){
                l.add(e.clone());
            }
        }
        return l;
    }

    public List<String> listOfPropostas(String cod){
        List<String> l = new ArrayList<>();
        for(String s: this.encomendas.get(cod).getPropostas()){
            l.add(s);
        }
        return l;
    }

    public List<Encomenda> listOfEncomendasTransportador(String username){
        List<Encomenda> l = new ArrayList<>();
        Transportador lf = (Transportador) this.users.get(username).clone();
        for(Encomenda e: this.getEncomendas().values()){
            if(lf.getEncomendas().containsKey(e.getCodigo()) && e.getTransportador().equals(username)){
                l.add(e.clone());
            }
        }
        return l;
    }

    public boolean contemEncomenda(List<Encomenda> lst,String username, String cod){
        return lst.stream().filter(o->o.getCodigo().equals(cod)).findFirst().isPresent();
    }

    public boolean contemProposta(List<Encomenda> lst,String username, String cod, String empresa){
        return lst.stream().filter(o->o.getPropostas().contains(empresa)).findFirst().isPresent();
    }

    public String getEncomendaString(String num) throws EncomendaInexistenteException {
        if(this.encomendas.containsKey(num)) {
            return "Codigo: " + this.encomendas.get(num).getCodigo() + "\n" + "Hora do pedido: " + this.encomendas.get(num).getHoraI() + "\n" + "Peso do pedido" + this.encomendas.get(num).getPeso() + "\n" + "Pedido medico?: " + this.encomendas.get(num).getMedica() + "\n" + "Loja: " + this.encomendas.get(num).getLoja() + "\n" + "Coordenadas de entrega: " + this.users.get(this.encomendas.get(num).getUtilizador()).getPosicao() + "\n";
        }
        else{
            throw new EncomendaInexistenteException("A encomenda não existe");
        }
    }

    public List<String> listOfEncomendasInfo(Iterable<Encomenda> lst){
        List<String> end = new ArrayList<>();
        for(Encomenda e: lst){
            String s = "";
            s = "Codigo: " + e.getCodigo() + "\n" + "Hora do pedido: " + e.getHoraI() + "\n" + "Peso do pedido" + e.getPeso() + "\n" + "Pedido medico?: " + e.getMedica() + "\n" + "Loja: " + e.getLoja() + "\n" + "Coordenadas de entrega: " + this.users.get(e.getUtilizador()).getPosicao() + "\n" + "Empresas interessadas" + e.getPropostas() + "\n";
            end.add(s);
        }
        return end;
    }

    public List<String> listOfEncomendasHistoricoInfo(String username, Iterable<Encomenda> it){
        List<String> end = new ArrayList<>();
        for(Encomenda e: it){
            if(username.charAt(0) == 'u' && e.getUtilizador().equals(username)){
                String s = "";
                s = "Codigo: " + e.getCodigo() + "\n" + "Hora do pedido: " + e.getHoraI() + "\n" + "Peso do pedido" + e.getPeso() + "\n" + "Pedido medico?: " + e.getMedica() + "\n" + "Loja: " + e.getLoja() + "\n" + "Coordenadas de entrega: " + this.users.get(e.getUtilizador()).getPosicao() + "\n" + "Empresas interessadas" + e.getPropostas() + "\n";
                end.add(s);
            }
            if((username.charAt(0) == 'v' || username.charAt(0) == 't') && e.getTransportador().equals(username)){
                String s = "";
                s = "Codigo: " + e.getCodigo() + "\n" + "Hora do pedido: " + e.getHoraI() + "\n" + "Peso do pedido" + e.getPeso() + "\n" + "Pedido medico?: " + e.getMedica() + "\n" + "Loja: " + e.getLoja() + "\n" + "Coordenadas de entrega: " + this.users.get(e.getUtilizador()).getPosicao() + "\n" + "Empresas interessadas" + e.getPropostas() + "\n";
                end.add(s);
            }
            if(username.charAt(0) == 'l' && e.getLoja().equals(username)){
                String s = "";
                s = "Codigo: " + e.getCodigo() + "\n" + "Hora do pedido: " + e.getHoraI() + "\n" + "Peso do pedido" + e.getPeso() + "\n" + "Pedido medico?: " + e.getMedica() + "\n" + "Loja: " + e.getLoja() + "\n" + "Coordenadas de entrega: " + this.users.get(e.getUtilizador()).getPosicao() + "\n" + "Empresas interessadas" + e.getPropostas() + "\n";
                end.add(s);
            }
        }
        return end;
    }

    private double custoViagem(String cod, Empresa e){
        Localizacao lu = this.users.get(this.encomendas.get(cod).getUtilizador()).getPosicao();
        Localizacao ll = this.users.get(this.encomendas.get(cod).getLoja()).getPosicao();
        Localizacao le = e.getPosicao();
        double distancia = e.distanciaA(ll) + e.distanciaA(lu);
        return (distancia*e.getCusto_Km()) + (distancia*e.getCusto_Peso());
    }

    public List<String> listOfTransportadoresInfo(String cod, Iterable<String> lst){
        List<String> end = new ArrayList<>();
        for(String st: lst){
            Empresa t = (Empresa) this.users.get(st);
            String s = "";
            s = "Codigo: " + t.getUsername() + "\n" + "Nome: " + t.getNome() + "\n" + "Localizaçao: " +  t.getPosicao() + "\n" + "Raio: " +  t.getRaio() + "\n" + "Classificaçao: " + t.classMedia() + "\n" + "Custo total da viagem: " +  this.custoViagem(cod,t) + "\n";
            end.add(s);
        }
        return end;
    }

    public List<String> listOfLojasInfo(Iterable<Loja> lst){
        List<String> end = new ArrayList<>();
        for(Loja l: lst){
            String s = "";
            if(l instanceof LojaSemFila) {
                s = "Username: " + l.getUsername() + "\n"+ "Nome: " + l.getNome() + "\n" + "Classificações: " + l.getClassificacao().toString() + "\n" + l.getPosicao() .toString()+ "\n";
            }
            else{
                s = "Username: " + l.getUsername() + "\n" + "Nome: " + l.getNome() + "\n" + "Classificações: " + l.getClassificacao() + "\n" + l.getPosicao() + "\n" + "Esta loja tem fila com " + (((LojaComFila) l).getTamanho() - ((LojaComFila) l).getFila().size()) + " vagas" + "\n";
            }
            end.add(s);
        }
        return end;
    }

    public List<Loja> listOfLojasSemFila(){
        List<Loja> l = new ArrayList<>();
        for(User u: this.getUsersAsList()) {
            if(u instanceof LojaSemFila) {
                l.add(((Loja) u).clone());
            }
        }
        return l;
    }

    public List<Loja> listOfLojasComFila(){
        List<Loja> l = new ArrayList<>();
        for(User u: this.getUsersAsList()) {
            if(u instanceof LojaComFila) {
                l.add(((Loja) u).clone());
            }
        }
        return l;
    }

    public Set<Loja> ordenaLojasDistancia(double latitude, double longitude){
        Set<Loja> conj = new TreeSet<>((o1, o2) -> {
            double a = Math.sqrt((Math.pow((o1.getPosicao().getLatitude() - latitude),2))+(Math.pow((o1.getPosicao().getLongitude() - longitude),2)));
            double b = Math.sqrt((Math.pow((o2.getPosicao().getLatitude() - latitude),2))+(Math.pow((o2.getPosicao().getLongitude() - longitude),2)));
            return (int) (a - b);
        });
        for(Loja l: this.listOfLojas()){
            conj.add(l.clone()); }
        return conj;
    }

    public TreeSet<User> ordenarUsers(Comparator<User> c) {
        TreeSet<User> t = new TreeSet<User>(c);
        users.values().forEach(h -> {
            t.add(h.clone());
        });
        return t;
    }

    public List<Encomenda> ordenarEncomendaData(){
        List<Encomenda> lst = new ArrayList<>();
        for(Encomenda e: this.getEncomendas().values()){
            if(e.getHoraF().equals(e.getHoraI()) == false) {
                lst.add(e.clone());
            }
        }
        lst.sort(new ComparadorDataEncomenda());
        return lst;
    }

    public List<Encomenda> ordenarEncomendaPeso(){
        List<Encomenda> lst = new ArrayList<>();
        for(Encomenda e: this.getEncomendas().values()){
            if(e.getHoraF().equals(e.getHoraI()) == false) {
                lst.add(e.clone());
            }
        }
        lst.sort(new ComparadorPesoEncomenda());
        return lst;
    }

    public List<Encomenda> ordenarEncomendaPreco(){
        List<Encomenda> lst = new ArrayList<>();
        for(Encomenda e: this.getEncomendas().values()){
            if(e.getHoraF().equals(e.getHoraI()) == false) {
                lst.add(e.clone());
            }
        }
        lst.sort(new ComparadorPrecoEncomenda());
        return lst;
    }

    public List<Loja> ordenaLojasClass(){
        List<Loja> lst = new ArrayList<>();
        for(Loja l: this.listOfLojas()){
            lst.add(l.clone()); }
        lst.sort(new ClassLojaComparator());
        return lst;
    }

    public Set<Loja> ordenaLojasNome(){
        Set<Loja> conj = new TreeSet<>(new NomeLojaComparator());
        for(Loja l: this.listOfLojas()){
            conj.add(l.clone()); }
        return conj;
    }

    public List<String> ordenarTransportadoraPreco(String cod){
        List<String> lst = this.encomendas.get(cod).getPropostas();
        lst.sort((o1,o2) -> {
            Empresa e1 = (Empresa) this.users.get(o1);
            Empresa e2 = (Empresa) this.users.get(o2);
            return (int)(this.custoViagem(cod,e1)-this.custoViagem(cod,e2));
        });
        return lst;
    }

    public List<Encomenda> ordenarEncomendaDataI(String username){
        List<Encomenda> lst = listOfEncomendasPorEntregar(username);
        lst.sort(new ComparadorDataEncomendaI());
        return lst;
    }
    public List<Encomenda> ordenarEncomendaDataIT(String username){
        List<Encomenda> lst = listOfEncomendas(username);
        lst.sort(new ComparadorDataEncomendaI());
        return lst;
    }

    public List<Encomenda> filtrarEncomendaUser(String username){
        List<Encomenda> lst = new ArrayList<>();
        for(Encomenda e: this.encomendas.values()){
            if(username.charAt(0) == 'u'){
                return lst;
            }
            if((username.charAt(0) == 'v' || username.charAt(0) == 't') && this.encomendas.get(e.getCodigo()).getTransportador().equals(username)){
                lst.add(e.clone());
            }
            if(username.charAt(0) == 'l' && this.encomendas.get(e.getCodigo()).getLoja().equals(username)){
                lst.add(e.clone());
            }
        }
        return lst;
    }

    public List<Encomenda> filtrarEncomendaMedica(){
        List<Encomenda> lst = new ArrayList<>();
        for(Encomenda e: this.encomendas.values()){
            if(e.getMedica()){
                lst.add(e.clone());
            }
        }
        return lst;
    }

    public List<Encomenda> filtrarEncomendaMedica(String username){
        List<Encomenda> lst = listOfEncomendasPorEntregar(username);
        List<Encomenda> end = new ArrayList<>();
        for(Encomenda e: lst){
            if(e.getMedica()){
                end.add(e.clone());
            }
        }
        return end;
    }

    public List<Encomenda> filtrarEncomendaMedicaT(String username){
        List<Encomenda> lst = listOfEncomendas(username);
        List<Encomenda> end = new ArrayList<>();
        for(Encomenda e: lst){
            if(e.getMedica()){
                end.add(e.clone());
            }
        }
        return end;
    }

    public List<Encomenda> filtrarEncomendaDia(){
        List<Encomenda> lst = new ArrayList<>();
        for(Encomenda e: this.encomendas.values()){
            if(e.getHoraF().isAfter(LocalDateTime.now().minusDays(1))){
                lst.add(e.clone());
            }
        }
        return lst;
    }

    public double saldoHoje(String username){
        Empresa em = (Empresa) this.users.get(username);
        double saldo = 0.0;
        for(Encomenda e: this.encomendas.values()){
            if(e.getHoraF().isAfter(LocalDateTime.now().minusDays(1)) && e.getTransportador().equals(username)){
                saldo = saldo + custoViagem(e.getCodigo(),em);
            }
        }
        return saldo;
    }

    public List<Encomenda> filtrarEncomendaSemana(){
        List<Encomenda> lst = new ArrayList<>();
        for(Encomenda e: this.encomendas.values()){
            if(e.getHoraF().isAfter(LocalDateTime.now().minusWeeks(1))){
                lst.add(e.clone());
            }
        }
        return lst;
    }

    public double saldoSemana(String username){
        Empresa em = (Empresa) this.users.get(username);
        double saldo = 0.0;
        for(Encomenda e: this.encomendas.values()){
            if(e.getHoraF().isAfter(LocalDateTime.now().minusWeeks(1)) && e.getTransportador().equals(username)){
                saldo = saldo + custoViagem(e.getCodigo(),em);
            }
        }
        return saldo;
    }

    public List<Encomenda> filtrarEncomendaMes(){
        List<Encomenda> lst = new ArrayList<>();
        for(Encomenda e: this.encomendas.values()){
            if(e.getHoraF().isAfter(LocalDateTime.now().minusMonths(1))){
                lst.add(e.clone());
            }
        }
        return lst;
    }

    public double saldoMes(String username){
        Empresa em = (Empresa) this.users.get(username);
        double saldo = 0.0;
        for(Encomenda e: this.encomendas.values()){
            if(e.getHoraF().isAfter(LocalDateTime.now().minusMonths(1)) && e.getTransportador().equals(username)){
                saldo = saldo + custoViagem(e.getCodigo(),em);
            }
        }
        return saldo;
    }

    public double saldoSempre(String username){
        Empresa em = (Empresa) this.users.get(username);
        double saldo = 0.0;
        for(Encomenda e: this.encomendas.values()){
            if(e.getTransportador().equals(username)){
                saldo = saldo + custoViagem(e.getCodigo(),em);
            }
        }
        return saldo;
    }

    public List<String> top10Utilizadores(){
        List<String> lista = new ArrayList<>();
        for(User u: this.users.values()){
            if(u instanceof Utilizador){
                lista.add(u.getUsername());
            }
        }
        lista.sort(((o1, o2) -> {
            int u1 = 0;
            int u2 = 0;
            for(Encomenda e: this.encomendas.values()){
                if(e.getUtilizador().equals(o1)){
                    u1 += 1;
                }
                if(e.getUtilizador().equals(o2)){
                    u2 += 1;
                }
            }
            return u2-u1;
        }));
        List<String> end = new ArrayList<>();
        int i = 0;
        while(i < 10){
            end.add(lista.remove(0));
            i+=1;
        }
        return end;
    }

    public List<String> top10Voluntarios(){
        List<String> lista = new ArrayList<>();
        for(User u: this.users.values()){
            if(u instanceof Voluntario){
                lista.add(u.getUsername());
            }
        }
        lista.sort(((o1, o2) -> {
            double distancia1 = 0.0;
            double distancia2 = 0.0;
            Localizacao le1 = this.users.get(o1).getPosicao();
            Localizacao le2 = this.users.get(o2).getPosicao();
            for(Encomenda e: this.encomendas.values()){
                Localizacao lu = this.users.get(e.getUtilizador()).getPosicao();
                Localizacao ll = this.users.get(e.getLoja()).getPosicao();
                if(e.getTransportador().equals(o1)) {
                    distancia1 = distancia1 + ((Transportador) (this.users.get(o1))).distanciaA(ll) + ((Transportador) (this.users.get(o1))).distanciaA(lu);
                }
                if(e.getTransportador().equals(o2)) {
                    distancia2 = distancia2+ ((Transportador) (this.users.get(o2))).distanciaA(ll) + ((Transportador) (this.users.get(o2))).distanciaA(lu);
                }
            }
            return (int)(distancia2-distancia1);
        }));
        List<String> end = new ArrayList<>();
        int i = 0;
        while(i < 10){
            if(lista.size()>0) {
                end.add(lista.remove(0));
            }
            else{
                end.add("N/A");
            }
            i+=1;
        }
        return end;
    }

    public List<String> top10Empresas(){
        List<String> lista = new ArrayList<>();
        for(User u: this.users.values()){
            if(u instanceof Empresa){
                lista.add(u.getUsername());
            }
        }
        lista.sort(((o1, o2) -> {
            double distancia1 = 0.0;
            double distancia2 = 0.0;
            Localizacao le1 = this.users.get(o1).getPosicao();
            Localizacao le2 = this.users.get(o2).getPosicao();
            for(Encomenda e: this.encomendas.values()){
                Localizacao lu = this.users.get(e.getUtilizador()).getPosicao();
                Localizacao ll = this.users.get(e.getLoja()).getPosicao();
                if(e.getTransportador().equals(o1)) {
                    distancia1 = distancia1 + ((Empresa) (this.users.get(o1))).distanciaA(ll) + ((Empresa) (this.users.get(o1))).distanciaA(lu);
                }
                if(e.getTransportador().equals(o2)) {
                    distancia2 = distancia2 + ((Empresa) (this.users.get(o2))).distanciaA(ll) + ((Empresa) (this.users.get(o2))).distanciaA(lu);
                }
            }
            return (int)(distancia2-distancia1);
        }));
        List<String> end = new ArrayList<>();
        int i = 0;
        while(i < 10){
            if(lista.size()>0) {
                end.add(lista.remove(0));
            }
            else {
                end.add("N/A");
            }
            i+=1;
        }
        return end;
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.users + "\n")
            .append(this.encomendas + "\n")
            .append(this.aceites + "\n");
        return sb.toString();
    }

    /**
     * Metodo que guarda o estado de uma instancia num ficheiro de texto.
     */
    public void escreveEmFicheiroTxt(String nomeFicheiro) throws FileNotFoundException
    {
        PrintWriter fich = new PrintWriter(nomeFicheiro);
        for(User h: this.users.values())
            fich.println(h.toString());
        fich.flush();
        fich.close();
    }

    /**
     * Metodo que lê um fichero de texto com linhas com imformaçao de User
     */

    public static TrazAqui importaCSV(String nome,String fich) throws FileNotFoundException,IOException
    {
        TrazAqui hi = new TrazAqui();
        hi.setNome(nome);
        String[] linhaPartida;
        List<String> linhas = TrazAqui.lerCSV(fich);
        for(String linha: linhas){
            linhaPartida = linha.split(":",2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    hi.adicionaUser(TrazAqui.csv2Utilizador(linhaPartida[1]));
                    break;
                case "Voluntario":
                    hi.adicionaUser(TrazAqui.csv2Voluntario(linhaPartida[1]));
                    break;
                case "Transportadora":
                    hi.adicionaUser(TrazAqui.csv2Empresa(linhaPartida[1]));
                    break;
                case "Loja":
                    hi.adicionaUser(TrazAqui.csv2Loja(linhaPartida[1]));
                    break;
                case "Encomenda":
                    hi.adicionaEncomenda(TrazAqui.csv2Encomenda(linhaPartida[1]));
                    break;
                case "Aceite":
                    hi.adicionaAceites(linhaPartida[1]);
                    break;
            }
        }

        return hi;
    }

    public void importaCSV(String fich) throws FileNotFoundException,IOException
    {
        this.setNome(nome);
        String[] linhaPartida;
        List<String> linhas = TrazAqui.lerCSV(fich);
        for(String linha: linhas){
            linhaPartida = linha.split(":",2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    this.adicionaUser(TrazAqui.csv2Utilizador(linhaPartida[1]));
                    break;
                case "Voluntario":
                    this.adicionaUser(TrazAqui.csv2Voluntario(linhaPartida[1]));
                    break;
                case "Transportadora":
                    this.adicionaUser(TrazAqui.csv2Empresa(linhaPartida[1]));
                    break;
                case "Loja":
                    this.adicionaUser(TrazAqui.csv2Loja(linhaPartida[1]));
                    break;
                case "Encomenda":
                    this.adicionaEncomenda(TrazAqui.csv2Encomenda(linhaPartida[1]));
                    break;
                case "Aceite":
                    this.adicionaAceites(linhaPartida[1]);
                    break;
            }
        }
    }

    private static List<String> lerCSV(String fich) throws FileNotFoundException,IOException
    {
        List<String> linhas = new ArrayList<>();
        BufferedReader br = new BufferedReader(new FileReader(fich));
        String linha;
        while((linha = br.readLine())!=null)
        {
            linhas.add(linha);
        }
        return linhas;
    }


    private static Utilizador csv2Utilizador(String csv)
    {
        Utilizador u = new Utilizador();
        Random r = new Random();
        Localizacao l = new Localizacao();
        String[] atributos = csv.split(",");
        String username = atributos[0];
        String nome = atributos[1];
        double gpsx = Double.parseDouble(atributos[2]);
        double gpsy = Double.parseDouble(atributos[3]);
        l.setLatitude(gpsx);
        l.setLongitude(gpsy);
        u.setUsername(username);
        u.setNome(nome);
        u.setPosicao(l);
        u.setSexo(u.getRandomSexo());
        u.setIdade(r.nextInt(100));
        return u;
    }

    private static Voluntario csv2Voluntario(String csv)
    {
        Voluntario v = new Voluntario();
        Random r = new Random();
        Localizacao l = new Localizacao();
        String[] atributos = csv.split(",");
        String username = atributos[0];
        String nome = atributos[1];
        double gpsx = Double.parseDouble(atributos[2]);
        double gpsy = Double.parseDouble(atributos[3]);
        double raio = Double.parseDouble(atributos[4]);
        l.setLatitude(gpsx);
        l.setLongitude(gpsy);
        v.setUsername(username);
        v.setNome(nome);
        v.setPosicao(l);
        v.setSexo(v.getRandomSexo());
        v.setIdade(r.nextInt(100));
        v.setTransporteMedico(r.nextBoolean());
        v.setTransporte(r.nextBoolean());
        v.setRaio(raio);
        return v;
    }

    private static Empresa csv2Empresa(String csv)
    {
        Empresa e = new Empresa();
        Random r = new Random();
        Localizacao l = new Localizacao();
        String[] atributos = csv.split(",");
        String username = atributos[0];
        String nome = atributos[1];
        double gpsx = Double.parseDouble(atributos[2]);
        double gpsy = Double.parseDouble(atributos[3]);
        String nif = atributos[4];
        double raio = Double.parseDouble(atributos[5]);
        double custo_km = Double.parseDouble(atributos[6]);
        l.setLatitude(gpsx);
        l.setLongitude(gpsy);
        e.setUsername(username);
        e.setNome(nome);
        e.setPosicao(l);
        e.setNif(nif);
        e.setRaio(raio);
        e.setCusto_Km(custo_km);
        e.setCusto_peso(r.nextInt(5));
        return e;
    }

    private static Loja csv2Loja(String csv)
    {
        Random r = new Random();
        Localizacao l = new Localizacao();
        String[] atributos = csv.split(",");
        String username = atributos[0];
        String nome = atributos[1];
        double gpsx = Double.parseDouble(atributos[2]);
        double gpsy = Double.parseDouble(atributos[3]);
        l.setLatitude(gpsx);
        l.setLongitude(gpsy);
        boolean booleanFila = r.nextBoolean();
        int tam = 1 + r.nextInt(20);
        if(booleanFila){
            LojaSemFila ls = new LojaSemFila();
            ls.setUsername(username);
            ls.setNome(nome);
            ls.setPosicao(l);
            return ls;
        }
        else{
            LojaComFila lf = new LojaComFila();
            lf.setUsername(username);
            lf.setNome(nome);
            lf.setPosicao(l);
            lf.setTamanho(tam);
            return lf;
        }
    }

    private static Encomenda csv2Encomenda(String csv)
    {
        Encomenda e = new Encomenda();
        Random r = new Random();
        String[] atributos = csv.split(",");
        String codigo = atributos[0];
        String usernameU = atributos[1];
        String usernameL = atributos[2];
        double peso = Double.parseDouble(atributos[3]);
        int i = 0;
        for(String s: atributos){
            if(i>3 && i%4==0 && i < atributos.length){
                String[] atributosLE = {"","","",""};
                LinhaEncomenda le = new LinhaEncomenda();
                atributosLE[0] = atributos[i];
                atributosLE[1] = atributos[i+1];
                atributosLE[2] = atributos[i+2];
                atributosLE[3] = atributos[i+3];
                le.setReferencia(atributosLE[0]);
                le.setDescricao(atributosLE[1]);
                le.setQuantidade((int)Double.parseDouble(atributosLE[2]));
                le.setPreco(Double.parseDouble(atributosLE[3]));

                e.adicionaLinha(le);
                i += 4;
            }
            else {
                i += 1;
            }
        }
        e.setUtilizador(usernameU);
        e.setLoja(usernameL);
        e.setMedica(r.nextBoolean());
        e.setPeso(peso);
        e.setCodigo(codigo);
        return e;
    }

    //Método guarda estado
    public void guardaEstado(String nomeFicheiro) throws FileNotFoundException,IOException {
        FileOutputStream fos = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }

    //Método carrega estado
    public static TrazAqui carregaEstado(String nomeFicheiro) throws FileNotFoundException,
            IOException,
            ClassNotFoundException {
        FileInputStream fis = new FileInputStream(nomeFicheiro);
        ObjectInputStream ois = new ObjectInputStream(fis);
        TrazAqui h = (TrazAqui) ois.readObject();
        ois.close();
        return h;
    }

}
