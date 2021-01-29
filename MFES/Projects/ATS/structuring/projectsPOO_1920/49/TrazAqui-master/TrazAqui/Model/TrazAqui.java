package Model;

import Model.Atores.Transportadores.EmpresaTransportadora;
import Model.Atores.Loja;
import Model.Atores.Transportadores.Transporte;
import Model.Atores.Transportadores.Voluntario;
import Model.Atores.User;
import Model.Comparators.DataComparator;
import Model.Comparators.KmComparator;
import Model.Exeptions.LoginErradoException;
import Model.Exeptions.MailRegistadoException;

import java.io.*;


import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

public class TrazAqui implements Serializable {



    private Map<String, Loja> lojas;
    private Map<String, User> users;
    private Map<String, Transporte> transporte;
    private Map<String,Encomenda> encomendas;
    private User userIn;
    private Voluntario voluntarioIn;
    private EmpresaTransportadora empresaIn;
    private Loja lojaIn;
    private Encomenda encomenda;



    public TrazAqui(){
        this.lojas = new HashMap<>();
        this.users = new HashMap<>();
        this.transporte = new HashMap<>();
        this.encomendas = new HashMap<>();
        this.userIn = new User();
        this.voluntarioIn = new Voluntario();
        this.empresaIn = new EmpresaTransportadora();
        this.lojaIn = new Loja();
        this.encomenda = new Encomenda();


    }


    public TrazAqui(Map<String, Loja> lojas, Map<String, User> users, User userIn, EmpresaTransportadora empresa, Voluntario voluntario, Loja loja, Map<String, Encomenda> encomendas, Encomenda enc) {
        this.lojas = lojas;
        this.users = users;
        this.encomendas = encomendas;
        this.userIn = userIn;
        this.voluntarioIn = voluntario;
        this.empresaIn = empresa;
        this.lojaIn = loja;
        this.encomenda = enc;

    }
    public TrazAqui(TrazAqui db) {
        this.lojas = db.getLojas();
        this.users = db.getUsers();
        this.transporte = db.getTransportador();
        this.encomendas = db.getEncomendas();
        this.userIn = db.getClienteIn();
        this.voluntarioIn = db.getVoluntarioIn();
        this.empresaIn = db.getEmpresaIn();
        this.encomenda = db.getEncomenda();
    }


    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> aux = new HashMap<>();
        for(Map.Entry<String,Encomenda> e:this.encomendas.entrySet())
            aux.put(e.getKey(),e.getValue());
        return aux;

    }

    public Map<String, Loja> getLojas() {
        Map <String,Loja> aux = new HashMap<>();
        for(Map.Entry<String,Loja> e:this.lojas.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }




    public Map<String, User> getUsers() {
        Map <String,User> aux = new HashMap<>();
        for(Map.Entry<String,User> e:this.users.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }



    public Map<String, User> getUsersEmail() {
        Map <String,User> aux = new HashMap<>();
        for(Map.Entry<String,User> e:this.users.entrySet())
            aux.put(e.getValue().getEmail(),e.getValue().clone());
        return aux;


    }

    public Map<String, Loja> getLojasEmail() {
        Map <String,Loja> aux = new HashMap<>();
        for(Map.Entry<String,Loja> e:this.lojas.entrySet())
            aux.put(e.getValue().getEmail(),e.getValue().clone());
        return aux;
    }


    public User getClienteIn()
    {
        return this.userIn.clone();
    }

    public void setClienteIn(User userIn) {
        this.userIn = userIn;
    }

    public Voluntario getVoluntarioIn() {
        return voluntarioIn;
    }

    public void setVoluntarioIn(Voluntario voluntarioIn) {
        this.voluntarioIn = voluntarioIn;
    }

    public EmpresaTransportadora getEmpresaIn() {
        return empresaIn;
    }

    public void setEmpresaIn(EmpresaTransportadora empresaIn) {
        this.empresaIn = empresaIn;
    }

    public Loja getLojaIn() {
        return lojaIn;
    }

    public void setLojaIn(Loja lojaIn) {
        this.lojaIn = lojaIn;
    }

    public Encomenda getEncomenda() {
        return encomenda;
    }

    public void setEnc(Encomenda enc) {
        this.encomenda = enc;
    }

    public void setUsers(Map<String, User> usr) {
        this.users = new HashMap<>();
        usr.entrySet().forEach(e-> this.users.put(e.getKey(),
                e.getValue().clone()));
    }


    public void setLojas(Map<String, Loja> lj) {
        this.lojas = new HashMap<>();
        lj.entrySet().forEach(e-> this.lojas.put(e.getKey(),
                e.getValue().clone()));
    }

    /**
     * "getters" auxiliares
     * @return
     */


//retorna os transportadores(empresas+voluntarios)
    public Map<String,Transporte> getTransportador(){
        Map <String,Transporte> aux = new HashMap<>();
        for(Map.Entry<String,Transporte> e:this.transporte.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    public Map<String,Transporte> getTransportadorEmail(){
        Map <String,Transporte> aux = new HashMap<>();
        for(Map.Entry<String,Transporte> e:this.transporte.entrySet())
            aux.put(e.getValue().getEmail(),e.getValue().clone());
        return aux;
    }

//retorna a as empresas de transporte
    public Map<String,EmpresaTransportadora> getEmpresaTransporte(){
        Map <String,EmpresaTransportadora> aux = new HashMap<>();
        for(Map.Entry<String,Transporte> e:this.transporte.entrySet())
            if  (e.getValue() instanceof EmpresaTransportadora){
                EmpresaTransportadora ep = (EmpresaTransportadora) e.getValue();
                aux.put(e.getKey(),ep.clone());
            }

        return aux;
    }


    public Map<String,EmpresaTransportadora> getEmpresaTransporteEmail(){
        Map <String,EmpresaTransportadora> aux = new HashMap<>();
        for(Map.Entry<String,Transporte> e:this.transporte.entrySet())
            if  (e.getValue() instanceof EmpresaTransportadora){
                EmpresaTransportadora ep = (EmpresaTransportadora) e.getValue();
                aux.put(e.getValue().getEmail(),ep.clone());
            }

        return aux;
    }

    //retorna os voluntarios
    public Map<String,Voluntario> getVoluntariosTransporte(){
        Map <String,Voluntario> aux = new HashMap<>();
        for(Map.Entry<String,Transporte> e:this.transporte.entrySet())
            if  (e.getValue() instanceof Voluntario){
                Voluntario ep = (Voluntario) e.getValue();
                aux.put(e.getKey(),ep.clone());
            }

        return aux;
    }

    public Map<String,Voluntario> getVoluntariosTransporteEmail(){
        Map <String,Voluntario> aux = new HashMap<>();
        for(Map.Entry<String,Transporte> e:this.transporte.entrySet())
            if  (e.getValue() instanceof Voluntario){
                Voluntario ep = (Voluntario) e.getValue();
                aux.put(e.getValue().getEmail(),ep.clone());
            }

        return aux;
    }



    //dispoe as encomendas ja efetuadas em lista por utilizador k: referencia user v:lista de encomendas do utlizador
    public Map<String, List<Encomenda>> getUtilizadorEncomendas(){
    Map<String,List<Encomenda>> aux = new HashMap<>();
    for(Map.Entry<String,Encomenda> e: listToMapEncomendasEfetuadas().entrySet())
        if(aux.containsKey(e.getValue().getComprador())) { //se o map ja tem encomendas guardadas de um utilizador
           List<Encomenda> lista = aux.get(e.getValue().getComprador()); //cria uma lista com as encomendas anteriormente guardadas para um utilizador
            lista.add(e.getValue().clone());            //adiciona a nova encomenda
            aux.put(e.getValue().getComprador().getReferencia(), lista); //adiciona a nova lista ao utilizador
        }
        else{                                               //se nao existirem ocorrencias no map
            List<Encomenda> lista2 = new ArrayList<>();      //cria se uma lista
            lista2.add(e.getValue().clone());                // adiciona-se o elemento a lista
            aux.put(e.getValue().getComprador().getReferencia(),lista2);     //adiciona se lista ao utilizador
        }
    return aux;
    }





    public void adicionaTransportador(Transporte t) {
        this.transporte.put(t.getReferencia(),t.clone());
    }
    public void adicionaUser(User t) {
        this.users.put(t.getReferencia(),t.clone());
    }
    public void adicionaLoja(Loja j) {
        this.lojas.put(j.getReferencia(), j.clone());
    }


    public void adicionaProdutoLoja(Produto j, String loja) {
        Loja a = this.lojas.get(loja).clone();
        a.adicionaProdutoLoja(j.clone());
        adicionaLoja(a);

    }
    public void adicionaEncomendaTransportador(EmpresaTransportadora a,Encomenda b ){
        EmpresaTransportadora x = this.getEmpresaTransporte().get(a.getReferencia());
        x.adicionaEncomendaTransporte(b);
        adicionaTransportador(x);
    }
    public void adicionaEncomendaVoluntario(Voluntario a,Encomenda b ){
        Voluntario x = this.getVoluntariosTransporte().get(a.getReferencia());
        x.adicionaEncomendaTransporte(b);
        adicionaTransportador(x);
    }



    public Transporte removeEncomendaTransportador(String ref,Encomenda e){
        if (this.transporte.get(ref) instanceof EmpresaTransportadora){
            EmpresaTransportadora et = (EmpresaTransportadora) this.transporte.get(ref);
            et.removeEncomendaTransportador(e);
            this.adicionaTransportador(et);
        }
        if (this.transporte.get(ref) instanceof Voluntario){
            Voluntario v = (Voluntario) this.transporte.get(ref);
            v.removeEncomendaTransportador(e);
            this.adicionaTransportador(v);
        }
        return this.transporte.get(ref);
    }

    public void removeEncomendaGeral(Encomenda a){
        Loja l = a.getLoja();
        User u = a.getComprador();
        if(a.getDistribuidor() instanceof EmpresaTransportadora) {
            EmpresaTransportadora e = (EmpresaTransportadora) a.getDistribuidor();
            this.removeEncomendaTransportador(e.getReferencia(),a);
        }
        if(a.getDistribuidor() instanceof Voluntario) {
            Voluntario e = (Voluntario) a.getDistribuidor();
            this.removeEncomendaTransportador(e.getReferencia(),a);

        }
        l.removeEncomendaLoja(a);
        u.removeEncomendaUser(a);
        this.adicionaUser(u);
        this.adicionaLoja(l);
        this.encomendas.remove(a.getReferencia());
    }

    public List<Encomenda> getEncomendasEfetuadas()
    {
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda v : this.encomendas.values())
            if (v.isEfetuada()==true){
                Encomenda enc = v.clone();
                aux.add(enc);
            }
        return aux;
    }

    public Map<String,Encomenda> listToMapEncomendasEfetuadas(){

            Map<String, Encomenda> map = getEncomendasEfetuadas().stream()
                    .collect(Collectors.toMap(Encomenda::getReferencia, encomenda -> encomenda.clone()));
            return map;
    }




    /**
     * encomenda
     */

    public void defUserEncomenda(){
        this.encomenda.setComprador(this.userIn);
    }

    public void adicionaEncomenda(Encomenda e) {
        this.encomendas.put(e.getReferencia(),e.clone());
    }


    public void removeEncomenda(Encomenda e) {this.encomendas.remove(e.getReferencia());}







    /**
     *
     * Adicao de encomendas, lojas e voluntarios a base de dados
     */



    public void RegistaLoja (Loja l) throws MailRegistadoException {
        if (this.getLojas().containsKey(l.getReferencia())) throw new MailRegistadoException("Loja já registada");
        this.lojas.put(l.getReferencia(), l.clone());
    }


    public void registarUtilizador(User c)throws MailRegistadoException
    {
        if (this.getUsers().containsKey(c.getReferencia())) throw new MailRegistadoException("Utilizador já registado");
        this.users.put(c.getReferencia(), c.clone());
    }

    public void registarVoluntario (Voluntario v) throws MailRegistadoException
    {
        if(this.getTransportador().containsKey(v.getReferencia())) throw new MailRegistadoException("Voluntario já registado");
        this.transporte.put(v.getReferencia(), v.clone());
    }

    public void registarEmpresa (EmpresaTransportadora e) throws MailRegistadoException
    {
        if(this.getTransportador().containsKey(e.getReferencia())) throw new MailRegistadoException("empresa já registado");
        this.transporte.put(e.getReferencia(), e.clone());
    }

    public void RegistaLojaEmail (Loja l) throws MailRegistadoException {
        if (this.getLojasEmail().containsKey(l.getEmail())) throw new MailRegistadoException("Email loja já registado");
        this.lojas.put(l.getReferencia(), l.clone());
    }


    public void registarUtilizadorEmail (User c)throws MailRegistadoException
    {
        if (this.getUsersEmail().containsKey(c.getEmail())) throw new MailRegistadoException("Email utilizador já registado");
        this.users.put(c.getReferencia(), c.clone());
    }

    public void registarVoluntarioEmail (Voluntario v) throws MailRegistadoException
    {
        if(this.getTransportadorEmail().containsKey(v.getEmail())) throw new MailRegistadoException("Email voluntario já registado");
        this.transporte.put(v.getReferencia(), v.clone());

    }

    public void registarEmpresaEmail (EmpresaTransportadora e) throws MailRegistadoException
    {
        if(this.getTransportadorEmail().containsKey(e.getEmail())) throw new MailRegistadoException("Email empresa já registado");
        this.transporte.put(e.getReferencia(), e.clone());
    }


    /**
     *
     * Inicio de sessao de clientes, lojas e transportadores
     */


    //inicio de sessao de cliente
    public void iniciaSessaoC(String email, String password) throws LoginErradoException
    {

        User u = this.getUsersEmail().get(email);
        if (u==null) throw new LoginErradoException("Email cliente não registado");
        if(u.getPassword().equals(password)) this.userIn = u;
        else  throw new LoginErradoException("Password errada");
    }

    //inicio de sessao de empresa
    public void iniciaSessaoE(String email, String password) throws LoginErradoException
    {
        EmpresaTransportadora e = this.getEmpresaTransporteEmail().get(email);
        if (e==null) throw new LoginErradoException("Email empresa não registado");
        if(e.getPassword().equals(password)) this.empresaIn = e;
        else  throw new LoginErradoException("Password errada");
    }

    //inicio de sessao de voluntario
    public void iniciaSessaoV(String email, String password) throws LoginErradoException
    {
        Voluntario v = this.getVoluntariosTransporteEmail().get(email);
        if (v==null) throw new LoginErradoException("Email voluntario não registado");
        if(v.getPassword().equals(password)) this.voluntarioIn = v;
        else  throw new LoginErradoException("Password errada");
    }

    //inicio de sessao de loja
    public void iniciaSessaoL(String email, String password) throws LoginErradoException
    {
        Loja l = this.getLojasEmail().get(email);

        if (l==null) throw new LoginErradoException("Email loja não registado");
        if(l.getPassword().equals(password)) this.lojaIn = l;
        else  throw new LoginErradoException("Password errada");
    }

    /**
     * metodos com Datas
     */


    //encomendas pedidas por ordem
    public List<Encomenda> encGlobaisData(){
        return this.encomendas.values().stream().sorted(new DataComparator()).collect(Collectors.toList());
    }

    public double totalFaturadoPeriodo(String referencia, LocalDate data){
        List<Encomenda> a = this.encomendas.values().stream().filter(e->e.getDistribuidor().getReferencia().equals(referencia) && e.getData().toLocalDate().isBefore(data)).collect(Collectors.toList());
        double res=0;
        for(Encomenda e : a){
            res+=e.getCustoTransporte();
        }
    return res;
    }


    public List<Encomenda> showEncomendasEmpresa(LocalDate data) {
        return this.encomendas.values().stream().filter(e -> e.getData().toLocalDate().isBefore(data) && e.getDistribuidor().getReferencia().equals(this.empresaIn.getReferencia())).sorted(new DataComparator()).collect(Collectors.toList());
    }

    public List<Encomenda> showEncomendasVoluntario(LocalDate data) {
        return this.encomendas.values().stream().filter(e -> e.getData().toLocalDate().isBefore(data) && e.getDistribuidor().getReferencia().equals(this.voluntarioIn.getReferencia())).sorted(new DataComparator()).collect(Collectors.toList());
    }

    public List<Encomenda> showEncomendaLoja(LocalDate data) {
        return this.encomendas.values().stream().filter(e -> e.getData().toLocalDate().isBefore(data) && e.getLoja().getReferencia().equals(this.lojaIn.getReferencia())).collect(Collectors.toList());
    }

    public List<Encomenda> showEncomendaUser(LocalDate data) {
        return this.encomendas.values().stream().filter(e -> e.getData().toLocalDate().isBefore(data) && e.getComprador().getReferencia().equals(this.userIn.getReferencia())).collect(Collectors.toList());
    }


    public List<EmpresaTransportadora> top10Kms(){
        return getEmpresaTransporte().values().stream()
                .sorted(new KmComparator())
                .limit(10).collect(Collectors.toList());
    }

    public List<User> topUsers(){
        List<User> users = this.users.values().stream().collect(Collectors.toList());
        Collections.sort(users, Collections.reverseOrder());
        return users.stream().limit(10).collect(Collectors.toList());


        }


    public List<Encomenda> getPedidosLoja(){
        Map<String,Encomenda> aux = this.encomendas;
        List <Encomenda> res = aux.values().stream().filter(e-> e.getLoja().getReferencia().equals(this.lojaIn.getReferencia()) && !e.isEfetuada()).collect(Collectors.toList());
        return res;
    }



    private Map<String, Transporte> EncomendaTransporte(Encomenda a) {
        Map<String, Transporte> aux = new HashMap<>();

        for (Map.Entry<String, Transporte> e : getTransportador().entrySet()) {

            if (e.getValue().isDisponivel() && e.getValue().distanciaValida(a))
                aux.put(e.getKey(), e.getValue().clone());
        }

        return aux;
    }

    //escolhe o transportador ( voluntario ou empresa) que percorre menos distancia ate à encomenda a
    public Transporte sortEncomendaTransporte(Encomenda a) {
        Map<String, Transporte> map1 = EncomendaTransporte(a);
        String aux = "";
        double distancia = 0;

        List<Double> l1;
        List<String> l2;
        l1 = map1.values().stream().mapToDouble(e-> e.distancia(a)).boxed().collect(Collectors.toList());
        l2 = map1.values().stream().map(e-> e.getReferencia()).collect(Collectors.toList());
        distancia = l1.get(1);

        for(int i =0;i<l1.size();i++) {
            if(distancia > l1.get(i))
                distancia = l1.get(i);
            }


        for (Map.Entry<String, Transporte> e : map1.entrySet()) {
            if (e.getValue().distancia(a) == distancia) {
                distancia = e.getValue().distancia(a);
                aux = e.getKey();

            }
        }

        return map1.get(aux);
    }

    public Transporte sortEncomendaTransporteExp(Encomenda a, String f){
        Map<String, Transporte> map1 = EncomendaTransporte(a);
        String aux = "";
        double distancia = 0;

        for (Map.Entry<String, Transporte> e : map1.entrySet()) {
            if (!e.getKey().equals(f)) {
                aux = e.getKey();
            }

            }
        return map1.get(aux);
    }






    public Encomenda geraReferenciaEncomenda(Encomenda a){
        StringBuilder sb = new StringBuilder();
        int sizeMap = this.getEncomendas().size();

        while(this.getEncomendas().containsKey((sb.append("e"+ sizeMap)).toString()))
            sizeMap++;
        a.setReferencia((sb.toString()));
        return a;
    }

    public User geraReferenciaUser(User e){
        StringBuilder sb = new StringBuilder();
        int sizeMap = this.getUsers().size();

        while(this.getUsers().containsKey((sb.append("u"+ sizeMap)).toString()))
            sizeMap++;
        e.setReferencia((sb.toString()));
        return e;
    }

    public Produto geraReferenciaProduto(Produto a,Loja b){
        StringBuilder sb = new StringBuilder();
        int sizeMap = b.getProdutos().size();

        while(b.getProdutos().containsKey((sb.append("p"+ sizeMap)).toString()))
            sizeMap++;
        a.setReferencia((sb.toString()));
        return a;
    }

    public Loja geraReferenciaLoja(Loja l){
        StringBuilder sb = new StringBuilder();
        int sizeMap = this.getLojas().size();

        while(this.getEncomendas().containsKey((sb.append("l"+ sizeMap)).toString()))
            sizeMap++;
        l.setReferencia((sb.toString()));

        return l;
    }

    public EmpresaTransportadora geraReferenciaTransportadorEmpresa(EmpresaTransportadora e){
        StringBuilder sb = new StringBuilder();
        int sizeMap = this.getEmpresaTransporte().size();

        while(this.getEncomendas().containsKey((sb.append("t"+ sizeMap)).toString()))
            sizeMap++;
        e.setReferencia((sb.toString()));
        return e;
    }
    public Voluntario geraReferenciaTransportadorVoluntario (Voluntario e){
        StringBuilder sb = new StringBuilder();
        int sizeMap = this.getVoluntariosTransporte().size();

        while(this.getEncomendas().containsKey((sb.append("v"+ sizeMap)).toString()))
            sizeMap++;
        e.setReferencia((sb.toString()));
        return e;
    }



    //basicamente trata da encomenda
    public EmpresaTransportadora addEncomendaEmpresa(){

        Encomenda e = this.getEncomenda();
        EmpresaTransportadora t = getEmpresaTransporte().get(e.getDistribuidor().getReferencia());
        t.aceitaEncomenda(e);
        e.setEfetuada(true);
        t.addKms(e);
        t.addFatura(e);
        e.setTempo(t.tempoViagem(e));
        adicionaTransportador(t);
        t.adicionaEncomendaTransporte(e);
        adicionaEncomenda(e);
        e.setCustoTransporte(t.defineCusto(e));
        this.setEmpresaIn(t);
        return t;
    }



    public void addEncomendaVoluntario(){
        Encomenda e = this.getEncomenda();
        Voluntario t = getVoluntariosTransporte().get(e.getDistribuidor().getReferencia());
        t.aceitaEncomenda(e);
        e.setEfetuada(true);
        t.addKms(e);
        e.setTempo(t.tempoViagem(e));
        t.adicionaEncomendaTransporte(e);
        adicionaTransportador(t);
        this.setVoluntarioIn(t);
    }


    /**
     * Guardar encomendas nos registos individuais
     */

    public void addRegistoC ()
    {

        String referencia=this.encomenda.getComprador().getReferencia();
        this.users.get(referencia).adicionaEncomendaUser(this.encomenda);
    }
    public void addRegistoT ()
    {
        String referencia=this.encomenda.getDistribuidor().getReferencia();
        this.transporte.get(referencia).adicionaEncomendaTransporte(this.encomenda);
    }
    public void addRegistoL ()
    {
        String referencia=this.encomenda.getLoja().getReferencia();
        this.lojas.get(referencia).adicionaEncomendaLoja(this.encomenda);
    }

    public void addRegistoPedidoTransportador (Transporte a)
    {
       String referencia = a.getReferencia();
        this.transporte.get(referencia).adicionaEncomendaTransporte(this.encomenda);
    }

    public User ShowDadosU()
    {
        return this.userIn.clone();
    }
    public EmpresaTransportadora ShowDadosE()
    {
        return this.empresaIn.clone();
    }

    public Voluntario ShowDadosV()
    {
        return this.voluntarioIn.clone();
    }

    public Loja ShowDadosL()
    {
        return this.lojaIn.clone();
    }

    /**
Carregamento de dados
 **/
    public static TrazAqui lerDados() throws IOException, ClassNotFoundException{

        ObjectInputStream ois = new ObjectInputStream(new FileInputStream("TrazAqui.data"));
        TrazAqui db = (TrazAqui) ois.readObject();
        ois.close();
        return db;
    }


    public void gravar() throws IOException {
        ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("TrazAqui.data"));
        oos.writeObject(this);

        oos.flush();
        oos.close();
    }





}

