
import java.util.*;
import java.util.stream.Collectors;
import java.io.*;
import java.lang.Math;

public class Dados implements Serializable{

    private Map<String, Utilizador> utilizadores;
    private Map<String, Transporte> transportes;
    private Map<String, Loja> lojas;
    private Map<String, Artigo> artigos;
    private Map<String, Encomenda> encomendasPendentes;
    private Map<String, Encomenda> encomendasAceites;
    private Map<Encomenda, List<Empresa>> encomendasTransportePendente;
    private int idEncomenda;

    public Dados(){
        this.utilizadores = new HashMap<String,Utilizador>();
        this.transportes = new HashMap<String,Transporte>();
        this.lojas = new HashMap<String, Loja>();
        this.artigos = new HashMap<String, Artigo>();
        this.idEncomenda = 0;
        this.encomendasPendentes = new HashMap<String, Encomenda>();
        this.encomendasTransportePendente = new HashMap<Encomenda, List<Empresa>>();
        this.encomendasAceites = new HashMap<String, Encomenda>();
    }

    public Dados(Map<String,Utilizador> utilizadores, Map<String,Transporte> transportes, Map<String, Loja> lojas, Map<String, Artigo> artigos, Map<String, Encomenda> encomendasPendentes, Map<String, Encomenda> encomendasAceites,Map<Encomenda,List<Empresa>> encomendasTransportePendente, int idEncomenda){
        this.utilizadores = utilizadores;
        this.transportes = transportes;
        this.lojas = lojas;
        this.artigos = artigos;
        this.encomendasPendentes = encomendasPendentes;
        this.encomendasTransportePendente = encomendasTransportePendente;
        this.encomendasAceites = encomendasAceites;
        this.idEncomenda = idEncomenda;
    }

    public Dados(Dados d){
        this.utilizadores = d.getUtilizadores();
        this.transportes = d.getTransportes();
        this.lojas = d.getLojas();
        this.artigos = d.getArtigos();
        this.encomendasPendentes = d.getEncomendasPendentes();
        this.encomendasAceites = d.getEncomendasAceites();
        this.encomendasTransportePendente=d.getEncomendasTransportePendente();
        this.idEncomenda = d.getIdEncomenda();
    }

    public Map<String,Utilizador> getUtilizadores(){
        return this.utilizadores;
    }

    public Map<String,Transporte> getTransportes(){
        return this.transportes;
    }

    public Map<String,Loja> getLojas(){
        return this.lojas;
    }

    public Map<String,Artigo> getArtigos(){
        return this.artigos;
    }

    public int getIdEncomenda(){ return this.idEncomenda; }

    public Map<String,Encomenda> getEncomendasPendentes(){ return this.encomendasPendentes; }

    public Map<String,Encomenda> getEncomendasAceites(){ return this.encomendasAceites; }
    
    public Map<Encomenda,List<Empresa>> getEncomendasTransportePendente(){ return this.encomendasTransportePendente; }

    public void setUtilizadores(Map<String,Utilizador> utilizadores){
        this.utilizadores = utilizadores;
    }

    public void setTransportes(Map<String,Transporte> transportes){
        this.transportes = transportes;
    }

    public void setLojas(Map<String,Loja> lojas){
        this.lojas = lojas;
    }

    public void setArtigos(Map<String,Artigo> artigos){
        this.artigos = artigos;
    }

    public void setEncomendasPendentes(Map<String,Encomenda> encomendasPendentes){
        this.encomendasPendentes = encomendasPendentes;
    }

    public void setEncomendasAceites(Map<String,Encomenda> encomendasAceites){
        this.encomendasAceites = encomendasAceites;
    }
    
    public void setEncomendasTransportePendente(Map<Encomenda,List<Empresa>> encomendasTransportePendente){
        this.encomendasTransportePendente = encomendasTransportePendente;
    }
    
    public void setIdEncomenda(int idEncomenda){ this.idEncomenda = idEncomenda; }

    public Dados clone(){
        return new Dados(this);
    }

    public void registarUtilizador(String email, String nome, String password, float x, float y) throws RegistarException{
        if(!utilizadores.containsKey(email)){
            Utilizador u = new Utilizador(email, nome, password, x, y, new HashMap<>());
            utilizadores.put(email,u);
        } else{
            throw new RegistarException("Utilizador já existe");
        }
    }
    
    public void registarVoluntario(String email, String nome, String password, float x, float y, double raio) throws RegistarException{
        if(!transportes.containsKey(email)){
            Transporte t = new Voluntario(email,nome,password,x,y,raio,false);
            transportes.put(email,t);
        }else{
            throw new RegistarException("Voluntário já existe");
        }
    }

    public void registarEmpresa(String email, String nome, String password, float x, float y, double raio, float custo) throws RegistarException{
        if(!transportes.containsKey(email)){
            Transporte t = new Empresa(email,nome,password,x,y,raio,false,custo);
            transportes.put(email,t);
        }else{
            throw new RegistarException("Empresa já existe");
        }
        
    }
    
    public void registarLoja(String email, String nome, String password, float x, float y) throws RegistarException{
        if(!lojas.containsKey(email)){
            Map m = new HashMap<>();
            Map a = new HashMap<>();
            Loja l = new Loja(email,nome,password,m, x, y,a);
            lojas.put(email,l);
        }else{
            throw new RegistarException("Loja já existe");
        }
    }
    
    public void logUtilizador(String email, String password) throws LogInException{
        if(utilizadores.containsKey(email)){
            Utilizador u = utilizadores.get(email);
            if(!u.getPassword().equals(password)){
                throw new LogInException();
            }
        }
        else{
            throw new LogInException();
        }
    }
    
    public int logTransporte(String email, String password) throws LogInException{
        if(transportes.containsKey(email)){
            Transporte t = transportes.get(email);
            if(!t.getPassword().equals(password)){
                throw new LogInException();
            }
            else{
                if(t instanceof Voluntario){
                    t.setDisp(true);
                    return 1;
                }
                else{
                    t.setDisp(true);
                    return 0;
                }
            }
        }
        else{
            throw new LogInException();
        }
    }
    
    public void logLoja(String email, String password) throws LogInException{
        if(lojas.containsKey(email)){
            Loja l = lojas.get(email);
            if(!l.getPassword().equals(password)){
                throw new LogInException();
            }
        
        }else{
            throw new LogInException();
        }
    }

    public void printLojas(){
        for(Loja l: this.lojas.values()){
            System.out.println(l.toString());
        }
    }
    
    public void printArtigos(String loja) throws Exception{
        if(!this.lojas.containsKey(loja)){ throw new LojaInvalidaException("Loja inexistente"); }
        if(this.lojas.get(loja).getArtigos().isEmpty()){
            throw new ArtigoInvalidoException("loja sem artigos");
        }
        else {
            for (Artigo a : this.lojas.get(loja).getArtigos().values()) {
                System.out.println(a.toString());
            }
        }
    }
    
    public void printEncomendas(int i, String email, int estado) throws EncomendaInvalidaException{
        if(i==0){
            if(this.utilizadores.get(email).getEncomendas().isEmpty()){
                throw new EncomendaInvalidaException("Nao existem encomendas");
            }
            else {
                Map<String, Encomenda> encUtil = this.utilizadores.get(email).getEncomendas();
                Map<String, Encomenda> encs = encUtil.entrySet().stream()
                        .filter(x -> estado == x.getValue().getEstado())
                        .collect(Collectors.toMap(x -> x.getKey(), x -> x.getValue()));
                if(encs.isEmpty()){
                    throw new EncomendaInvalidaException("Nao existem encomendas");
                }
                else {
                    for (Encomenda e : encs.values()) {
                        System.out.println(e.toString());
                    }
                }
            }
        }
        else if(i==1){
            if(this.lojas.get(email).getEncomendas().isEmpty()){
                throw new EncomendaInvalidaException("Nao existem encomendas");
            }
            else {
                Map<String, Encomenda> encLoja = this.lojas.get(email).getEncomendas();
                Map<String, Encomenda> encsLojaPend = encLoja.entrySet().stream()
                        .filter(x -> estado == x.getValue().getEstado())
                        .collect(Collectors.toMap(x -> x.getKey(), x -> x.getValue()));
                for (Encomenda e : encsLojaPend.values()) {
                    System.out.println(e.toString());
                }
            }
        }
        else if(i==2){
            if(this.encomendasPendentes.isEmpty()){
                throw new EncomendaInvalidaException("Nao existem encomendas");
            }
            else {
                Map<String, Encomenda> enc = this.encomendasPendentes.entrySet().stream()
                        .filter(x -> estado == x.getValue().getEstado())
                        .collect(Collectors.toMap(x -> x.getKey(), x -> x.getValue()));
                for (Encomenda e : enc.values()) {
                    double distl = this.distl(e, email);
                    double distu = this.distu(e, email);
                    System.out.println(distl + "," + distu);
                    if (distl <= this.transportes.get(email).getRaio() && distu <= this.transportes.get(email).getRaio()) {
                        System.out.println(e.toString());
                    } else {
                        System.out.println("Não existem encomendas no teu raio");
                    }
                }
            }
        }
    }

   
    public void adicionarArtigo(String id, String descricao, float peso, float valor, String loja){
        Artigo a = new Artigo(id,descricao,peso,valor);
        this.lojas.get(loja).adicionarArtigo(a);
    }


    public void adicionarEncomenda(String id, String email, String loja, List<String> lista) throws LojaInvalidaException{
        if(id.equals("")) {
            StringBuilder s = new StringBuilder();
            s.append("e" + this.idEncomenda);
            id = s.toString();

            while(this.encomendasPendentes.containsKey(id)){
                this.idEncomenda++;
                s = new StringBuilder();
                s.append("e" + this.idEncomenda);
                id = s.toString();
            }
        }

        String transporte = new String();

        int estado = 0;

        Encomenda e = new Encomenda(id, email, loja, transporte, new HashMap<>(), 0,0,estado);

        for(String idArtigo: lista){
            try {
                Artigo a = this.lojas.get(loja).getArtigos().get(idArtigo).clone();
                e.adicionarArtigo(a);
            }catch (ArtigoInvalidoException exception){
                System.out.println(exception.getMessage());
            }
        }

        if(this.lojas.containsKey(loja)) {
            this.encomendasPendentes.put(id, e);
            this.lojas.get(loja).adicionarEncomenda(e);
            this.utilizadores.get(email).adicionarEncomenda(e);
        }
        else throw new LojaInvalidaException();
    }
    
    public void aceitarEncomenda(String email, String idEncomenda) throws EncomendaInvalidaException{
        Map <String,Encomenda> encsLoja = this.encomendasPendentes.entrySet().stream()
                                            .filter(x->email.equals(x.getValue().getLoja()))
                                            .collect(Collectors.toMap(x->x.getKey(), x->x.getValue()));
        Map <String, Encomenda> enc = encsLoja.entrySet().stream()
                                         .filter(x->0==x.getValue().getEstado())
                                         .collect(Collectors.toMap(x->x.getKey(), x->x.getValue()));                    
        if(encsLoja.containsKey(idEncomenda)){
            Encomenda e = encsLoja.get(idEncomenda);
            e.setEstado(1);
        }
        else{throw new EncomendaInvalidaException();}   
    }
    public void aceitarEncomendaTransporte(String email, String idEncomenda) throws EncomendaInvalidaException{
       Map <String, Encomenda> enc = this.encomendasPendentes.entrySet().stream()
                                         .filter(x->1==x.getValue().getEstado())
                                         .collect(Collectors.toMap(x->x.getKey(), x->x.getValue()));
       if(enc.containsKey(idEncomenda)) {
           Encomenda e = enc.get(idEncomenda);
           double distl = this.distl(e, email);
           double distu = this.distu(e, email);
           double dist = distu + distl;
           if (distl <= this.transportes.get(email).getRaio() && distu <= this.transportes.get(email).getRaio()) {
               if (this.transportes.get(email) instanceof Voluntario) {
                   e.setEstado(3);
                   e.setTransporte(email);
                   this.encomendasAceites.put(idEncomenda, e);
                   this.encomendasPendentes.remove(idEncomenda);
                   Viagem v = new Viagem(this.utilizadores.get((e.getUtilizador())), this.lojas.get((e.getLoja())), e, distl + distu);
                   this.transportes.get(email).registarViagem(v);
               } else {
                   e.setEstado(2);
                   List<Empresa> trans;
                   if (!this.encomendasTransportePendente.containsKey(e)) {
                       trans = new ArrayList<Empresa>();
                       trans.add((Empresa) this.transportes.get(email));
                       this.encomendasTransportePendente.put(e, trans);
                   } else {
                       trans = this.encomendasTransportePendente.get(e);
                       if (!trans.contains(this.transportes.get(email))){
                           trans.add((Empresa) this.transportes.get(email));
                       }
                   }
               }
           } else {
               throw new EncomendaInvalidaException();
           }
       }
       else{ throw new EncomendaInvalidaException(); }
    }
    public void classificarEntrega(String email, String idEncomenda, int classificacao) throws EncomendaInvalidaException{
        if(this.encomendasAceites.containsKey(idEncomenda)){
            Encomenda e = this.encomendasAceites.get(idEncomenda);
            e.setEstado(4);
            this.transportes.get(e.getTransporte()).registarClassificacao(classificacao);
        }else{throw new EncomendaInvalidaException();}   
    }
    
    public void printTransporteAceitar(String email) throws Exception{
        if(this.encomendasTransportePendente.isEmpty()){
            throw new EncomendaInvalidaException("Nao existem encomendas pendentes");
        }
        else {
            Set<Encomenda> keys = this.encomendasTransportePendente.keySet();
            for (Encomenda e : keys) {
                if(e.getUtilizador().equals(email)) {
                    for (Empresa t : this.encomendasTransportePendente.get(e)) {
                        System.out.println(e.toString());
                        System.out.println(t.toString());
                        double dist = this.distl(e, t.getEmail()) + this.distu(e, t.getEmail());
                        double custo = t.getCusto() * dist;
                        System.out.println("Custo : " + custo);
                    }
                }
            }
        }
    }
    public void aceitarTransporte(String email,String idEncomenda ,String idTransporte) throws EncomendaInvalidaException,TransporteInvalidoException{
        Encomenda e = new Encomenda();
        if(this.encomendasPendentes.containsKey(idEncomenda)){
            e = this.encomendasPendentes.get(idEncomenda);
            if(this.encomendasTransportePendente.containsKey(e)){
                if(this.encomendasTransportePendente.get(e).contains((Empresa)this.transportes.get(idTransporte))){
                    double distl = this.distl(e,idTransporte);
                    double distu = this.distu(e,idTransporte);
                    double dist = distu+distl;
                    e.setEstado(3);
                    e.setTransporte(idTransporte);
                    this.encomendasAceites.put(idEncomenda, e);
                    this.encomendasPendentes.remove(idEncomenda);
                    this.encomendasTransportePendente.remove(e);
                    Viagem v = new Viagem(this.utilizadores.get((e.getUtilizador())),this.lojas.get((e.getLoja())),e,distl+distu);
                    this.transportes.get(idTransporte).registarViagem(v);
                }else throw new TransporteInvalidoException("Transporte invalido.");
            }else throw new EncomendaInvalidaException("Encomenda não é válida.");
        }else throw new EncomendaInvalidaException("Encomenda não é válida.");
    }
    public double distl(Encomenda e, String email){
        float lx = this.lojas.get(e.getLoja()).getPos_x();
        float ly = this.lojas.get(e.getLoja()).getPos_y();
        float tx = this.transportes.get(email).getPos_x();
        float ty = this.transportes.get(email).getPos_y();
        double distl = Math.sqrt(Math.pow((lx-tx),2) + Math.pow((ly-ty),2));
        return distl;
    }
    public double distu (Encomenda e, String email){
        float tx = this.transportes.get(email).getPos_x();
        float ty = this.transportes.get(email).getPos_y();
        float ux = this.utilizadores.get(e.getUtilizador()).getPos_x();
        float uy = this.utilizadores.get(e.getUtilizador()).getPos_y();        
        double distu = Math.sqrt(Math.pow((ux-tx),2) + Math.pow((uy-ty),2));
        return distu;

    }

    public void alteraCusto(String empresa, float custo){
        ((Empresa) this.transportes.get(empresa)).setCusto(custo);
    }

    public void printViagens(String transporte) throws ViagemInvalidaException{
        if(this.transportes.get(transporte).getViagens().isEmpty()){
            throw new ViagemInvalidaException("Nao existem viagens");
        }
        else {
            for (Viagem v : this.transportes.get(transporte).getViagens()) {
                System.out.println(v.toString());
            }
        }
    }
}









