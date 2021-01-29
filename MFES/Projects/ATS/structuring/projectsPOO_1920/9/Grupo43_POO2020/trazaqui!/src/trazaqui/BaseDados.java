package trazaqui;

import trazaqui.Exceptions.*;
import org.javatuples.Pair;

import java.io.*;
import java.lang.reflect.Array;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;


public class BaseDados implements Serializable {
    private Map <String,LogUtilizador> utilizadores;
    private Map <String,LogTransportadora> transportadoras;
    private Map <String,LogVoluntario> voluntarios;
    private Map <String,LogLoja> lojas;
    private Map <String,Encomenda> encomendas;
    private Map <String,Encomenda> encomendasdisponiveis;
    private Map <String,Historico> historico;
    private ArrayList<Historico> entregas;
    private ArrayList<Pair<String,String>> flags;
    private ArrayList<String> vols;
    private ArrayList<String> classifica;

    //getters
    public Map<String,LogUtilizador> getUtilizadores(){return new HashMap<>(this.utilizadores);}

    public Map<String,LogTransportadora> getTrasnportadoras(){return new HashMap<>(this.transportadoras);}

    public Map<String, LogVoluntario> getVoluntarios(){return new HashMap<>(this.voluntarios);}

    public Map<String, LogLoja> getLojas(){return new HashMap<>(this.lojas);}

    public Map<String, Encomenda> getEncomendas(){return new HashMap<>(this.encomendas);}

    public Map<String, Encomenda> getEncomendasdisponiveis(){return new HashMap<>(this.encomendasdisponiveis);}

    public Map<String,Historico> getHistorico(){return new HashMap<>(this.historico);}

    public ArrayList<Historico> getEntregas(){return new ArrayList<>(this.entregas);}

    public ArrayList<Pair<String,String>> getFlags(){return new ArrayList<>(this.flags);}

    public ArrayList<String> getVols(){return new ArrayList<>(this.vols);}

    public ArrayList<String> getClassifica(){return new ArrayList<>(this.classifica);}

    //setters
    public void setUtilizadores(Map<String,LogUtilizador> user){this.utilizadores=new HashMap<>(user);}

    public void setTransportadoras(Map<String,LogTransportadora> trans){this.transportadoras=new HashMap<>(trans);}

    public void setVoluntarios(Map<String,LogVoluntario> vol){this.voluntarios=new HashMap<>(vol);}

    public void setLojas(Map<String,LogLoja> loj){this.lojas=new HashMap<>(loj);}

    public void setEncomendas(Map<String,Encomenda> enc){this.encomendas=new HashMap<>(enc);}

    public void setEncomendasdisponiveis(Map<String,Encomenda> enc){this.encomendasdisponiveis=new HashMap<>(enc);}

    public void setHistorico(Map<String,Historico> hist){this.historico=new HashMap<>(hist);}

    public void setEntregas(ArrayList<Historico> ent){this.entregas=new ArrayList<>(ent);}

    public void setFlags(ArrayList<Pair<String,String>> f){this.flags=new ArrayList<>(f);}

    public void setVols(ArrayList<String> v){this.vols=new ArrayList<>(v);}

    public void setClassifica(ArrayList<String> c){this.classifica=new ArrayList<>(c);}
    //clone
    public BaseDados clone(){return new BaseDados(this);}

    //construtor vazio
    public BaseDados(){
        this.utilizadores=new HashMap<>();
        this.transportadoras=new HashMap<>();
        this.voluntarios=new HashMap<>();
        this.lojas=new HashMap<>();
        this.encomendas=new HashMap<>();
        this.encomendasdisponiveis=new HashMap<>();
        this.historico=new HashMap<>();
        this.entregas=new ArrayList<>();
        this.flags=new ArrayList<>();
        this.vols=new ArrayList<>();
        this.classifica=new ArrayList<>();
    }

    //construtor parametrizado
    public BaseDados(Map<String,LogUtilizador> user,Map<String,LogTransportadora> trans, Map<String,LogVoluntario> vol, Map<String,LogLoja> loj, Map<String,Encomenda> enc, Map<String,Encomenda> encdisp, Map<String,Historico> hist, ArrayList<Historico> ent, ArrayList<Pair<String,String>> f,ArrayList<String> v,ArrayList<String> c){
        setUtilizadores(user);
        setTransportadoras(trans);
        setVoluntarios(vol);
        setLojas(loj);
        setEncomendas(enc);
        setEncomendasdisponiveis(encdisp);
        setHistorico(hist);
        setEntregas(ent);
        setFlags(f);
        setVols(v);
        setClassifica(c);
    }

    //construtor por copia
    public BaseDados(BaseDados bd){
        setUtilizadores(bd.getUtilizadores());
        setTransportadoras(bd.getTrasnportadoras());
        setVoluntarios(bd.getVoluntarios());
        setLojas(bd.getLojas());
        setEncomendas(bd.getEncomendas());
        setEncomendasdisponiveis(bd.getEncomendasdisponiveis());
        setHistorico(bd.getHistorico());
        setEntregas(bd.getEntregas());
        setFlags(bd.getFlags());
        setVols(bd.getVols());
        setClassifica(bd.getClassifica());
    }

    //metodo equals
    public boolean equals (Object o){
        if (this==o) return true;
        if ( (o==null) || (o.getClass()!=this.getClass())) return false;

        BaseDados bd = (BaseDados) o;

        for (LogUtilizador lu : bd.utilizadores.values()){
            if(!this.utilizadores.containsValue(lu)){
                return false;
            }
        }
        for (LogTransportadora lt: bd.transportadoras.values()){
            if(!this.transportadoras.containsValue(lt)){
                return false;
            }
        }
        for (LogVoluntario lu: bd.voluntarios.values()){
            if(!this.voluntarios.containsValue(lu)){
                return false;
            }
        }
        for (LogLoja ls: bd.lojas.values() ){
            if(!this.lojas.containsValue(ls)){
                return false;
            }
        }
        for (Encomenda e:bd.encomendas.values()){
            if(!this.encomendas.containsValue(e)){
                return false;
            }
        }
        for (Encomenda e: bd.encomendasdisponiveis.values()){
            if(!this.encomendasdisponiveis.containsValue(e))
                return false;
        }
        for (Historico hist: bd.historico.values()){
            if(!this.historico.containsValue(hist))
                return false;
        }
        for (Historico hist: bd.entregas){
            if(!this.entregas.contains(hist))
                return false;
        }

        for (Pair<String,String> p: bd.flags){
            if(!this.flags.contains(p))
                return false;
        }

        for(String v:bd.vols){
            if(!this.vols.contains(v))
                return false;
        }

        for(String c:bd.classifica){
            if(!this.classifica.contains(c))
                return false;
        }
        return true;

    }

    //metodo para adicionar um utilizador
    // à lista de utilizadores
    public void addUtilizador(LogUtilizador user){
        this.utilizadores.put(user.getUsername(),user.clone());
    }

    //à lista de transportadoras
    public void addTransportadora(LogTransportadora trans){ this.transportadoras.put(trans.getUsername(),trans.clone()); }

    //à lista de voluntários
    public void addVoluntario(LogVoluntario vol){
        this.voluntarios.put(vol.getUsername(),vol.clone());
    }

    //à lista de lojas
    public void addLoja(LogLoja loj){
        this.lojas.put(loj.getUsername(),loj.clone());
    }

    //adiciona uma encomenda à lista de encomendas disponiveis
    public void addEncomendaDisponivel(Encomenda enc) {this.encomendasdisponiveis.put(enc.getcodEncomenda(),enc.clone());}

    //adiciona uma encomenda para uma transportadora no à lista de flags
    public void addFlag(String codTrans, String codEncomenda){this.flags.add(new Pair<>(codTrans,codEncomenda));}

    //adiciona um código a vols
    public void addVol(String cod){this.vols.add(cod);}

    //adiciona um codigo encomenda a lista de encomendas por classificar
    public void addClassifica(String cod){this.classifica.add(cod);}

    //adiciona um historico ao map historico
    public void addHistorico(Historico h){this.historico.put(h.getcodEncomenda(),h.clone());}

    //método que remove uma encomenda da lista de encomendas
    public void removeEncomenda(Encomenda enc){this.encomendas.remove(enc.getcodEncomenda(), enc.clone());}

    //método que remove uma encomenda da lista de encomendas disponiveis
    public void removeEncomendaDisponivel(Encomenda enc){this.encomendasdisponiveis.remove(enc.getcodEncomenda(),enc.clone());}

    //método que remove uma encomenda da lista de entregas
    public void removeEntrega(Historico h){this.entregas.remove(h.clone());}

    //método que remove um codigo encomenda da lista de encomendas por classificar
    public void removeClassifica(String cod){this.classifica.remove(cod);}

    //metodo que verifica se um determinado user existe
    //utilizadores
    public boolean ExisteUtilizador(String user){
        return utilizadores.containsKey(user);
    }

    //transportadoras
    public boolean ExisteTransportadora(String user){
        return transportadoras.containsKey(user);
    }

    //voluntarios
    public boolean ExisteVoluntario(String user){
        return voluntarios.containsKey(user);
    }

    //lojas
    public boolean ExisteLoja(String user){
        return lojas.containsKey(user);
    }


    //método que verifica se um determinado código existe
    //utilizador
    public boolean ExisteCodUser(String cod){
        for(LogUtilizador lu: this.utilizadores.values()){
            if(lu.getCodUtilizador().equals(cod)){
                return true;
            }
        }
        return false;
    }

    //transportadora
    public boolean ExisteCodTrans(String cod){
        for(LogTransportadora lt: this.transportadoras.values()){
            if(lt.getCodEmpresa().equals(cod))
                return true;
        }
        return false;
    }

    //voluntarios
    public boolean ExisteCodVoluntario(String cod){
        for(LogVoluntario lv: this.voluntarios.values()){
            if(lv.getCodVoluntario().equals(cod))
                return true;
        }
        return false;
    }

    //lojas
    public boolean ExisteCodLoja(String cod){
        for(LogLoja lt: this.lojas.values()){
            if(lt.getCodLoja().equals(cod))
                return true;
        }
        return false;
    }

    //encomendas
    public boolean ExisteEncomenda(String cod){
        return this.encomendas.containsKey(cod);
    }

    //encomendas disponiveis
    public boolean ExisteAceite(String cod){return this.encomendasdisponiveis.containsKey(cod);}

    //historico
    public boolean ExisteHistorico(String cod){return this.historico.containsKey(cod);}

    //método que verifica se existe uma encomenda com um código na lista de entregas
    public boolean ExisteEntrega(String cod){
        for(Historico h: this.entregas){
            if (h.getcodEncomenda().equals(cod))
                return true;
        }
        return false;
    }


    //metodo que verifica a correspondência do username e da password introduzidos
    //pelo utilizador
    public String checkUserPassUtil(String user, String pass){
        for(LogUtilizador lu : this.utilizadores.values()){
            if(lu.getUsername().equals(user)){
                if(lu.getPassword().equals(pass)) return lu.getUsername();
            }
        }
        return "NOK";
    }

    //pela transportadora
    public String checkUSerPassTrans(String user, String pass){
        for(LogTransportadora lt: this.transportadoras.values()){
            if(lt.getUsername().equals(user)){
                if(lt.getPassword().equals(pass)) return lt.getUsername();
            }
        }
        return "NOK";
    }

    //pelos voluntários
    public String checkUserPassVol(String user, String pass){
        for(LogVoluntario lv: this.voluntarios.values()){
            if(lv.getUsername().equals(user)){
                if(lv.getPassword().equals(pass)) return lv.getUsername();
            }
        }
        return "NOK";
    }

    //pelas lojas
    public String checkUserPassLoj(String user, String pass){
        for(LogLoja loj: this.lojas.values()){
            if(loj.getUsername().equals(user)){
                if(loj.getPassword().equals(pass)) return loj.getUsername();
            }
        }
        return "NOK";
    }

    //verifica se um username
    // já se encontra registado na base de dados

    public boolean userEmUso(String user){
        for(LogUtilizador lu: this.getUtilizadores().values()){
            if(lu.getUsername().equals(user)) return true;
        }

        for(LogTransportadora lt: this.getTrasnportadoras().values()){
            if(lt.getUsername().equals(user)) return true;
        }

        for(LogVoluntario lv: this.getVoluntarios().values()){
            if(lv.getUsername().equals(user)) return true;
        }

        for(LogLoja loj: this.getLojas().values()){
            if (loj.getUsername().equals(user)) return true;
        }
        return false;
    }

    //regista um novo user na base de dados
    //utilizador
    public void novoUtilizador(String nome, Localizacao pos, String user, String pass) throws UsernameJaEstaEmUsoException, UtilizadorExisteException{
        if(userEmUso(user))
            throw new UsernameJaEstaEmUsoException("Ja existe um registo com este username");
        String cod=novoCodUtilizador();
        LogUtilizador u=new LogUtilizador();
        u.setCodUtilizador(cod);
        u.setNome(nome);
        u.setGps(pos);
        u.setUsername(user);
        u.setPassword(pass);
        if(ExisteUtilizador(user))
            throw new UtilizadorExisteException("Já existe registo com este username");
        this.utilizadores.put(u.getUsername(), u.clone());
    }

    //transportadoras
    public void novaTransportadora(String nome, Localizacao gps, String nif, double raio, double precokm, String user, String password) throws UsernameJaEstaEmUsoException, TransportadoraExisteException{
        if(userEmUso(user))
            throw new UsernameJaEstaEmUsoException("Ja existe um registo com este username");

        String cod=novoCodTransportadora();
        LogTransportadora t=new LogTransportadora();
        t.setCodEmpresa(cod);
        t.setNome(nome);
        t.setGps(gps);
        t.setNif(nif);
        t.setRaio(raio);
        t.setPrecokm(precokm);
        t.setUsername(user);
        t.setPassword(password);
        t.setClassificacoes(new ArrayList<>());
        if(ExisteTransportadora(user))
            throw new TransportadoraExisteException("Já existe registo com este username");
        this.transportadoras.put(t.getUsername(),t.clone());
    }

    //voluntarios
    public void novoVoluntario(String nome, Localizacao gps, double raio, String user, String pass, boolean tf) throws UsernameJaEstaEmUsoException, VoluntarioExisteException {
        if(userEmUso(user))
            throw new UsernameJaEstaEmUsoException("Já existe um registo com este username");

        String cod=novoCodVoluntario();
        LogVoluntario v= new LogVoluntario();
        v.setCodVoluntario(cod);
        v.setNome(nome);
        v.setGps(gps);
        v.setRaio(raio);
        v.setUsername(user);
        v.setPassword(pass);
        v.setDisponibilidade(tf);
        v.setClassificacoes(new ArrayList<>());

        if(ExisteVoluntario(user))
            throw new VoluntarioExisteException("Já existe um registo com este username");
        this.voluntarios.put(v.getUsername(),v.clone());
    }

    //lojas
    public void novaLoja(String nome, Localizacao gps, String user, String pass) throws UsernameJaEstaEmUsoException, LojaExisteException{
        if (userEmUso(user))
            throw new UsernameJaEstaEmUsoException("Já existe um registo com este username");

        String cod=novoCodLoja();
        LogLoja l= new LogLoja();
        l.setCodLoja(cod);
        l.setNome(nome);
        l.setGps(gps);
        l.setUsername(user);
        l.setPassword(pass);
        if(ExisteLoja(user))
            throw new LojaExisteException("Já existe um registo com este username");
        this.lojas.put(l.getUsername(),l.clone());
    }

    //encomenda
    public Encomenda novaEncomenda(String codUtilizador, String codLoja, double peso, ArrayList<LinhaEncomenda> linhas){
        String cod=novoCodEncomenda();
        Encomenda e= new Encomenda();
        e.setCodEncomenda(cod);
        e.setCodUtilizador(codUtilizador);
        e.setCodLoja(codLoja);
        e.setPeso(peso);
        e.setLinhas(linhas);
        this.encomendas.put(e.getcodEncomenda(),e.clone());
        return e;
    }

    //produto
    public Produto novoProduto(String username,String descricao, double preco, double stock){
        String cod = novoCodProduto(username);
        Produto p= new Produto();
        p.setCodProd(cod);
        p.setDescricao(descricao);
        p.setPreco(preco);
        p.setStock(stock);
        return p;
    }

    //entrega
    public void novaEntrega(String cod, String nome,String codEncomenda,String codUtilizador, String codLoja, double peso, ArrayList<LinhaEncomenda> linhas,double kms){
        Historico h= new Historico();
        h.setCod(cod);
        h.setNome(nome);
        h.setCodEncomenda(codEncomenda);
        h.setCodUtilizador(codUtilizador);
        h.setCodLoja(codLoja);
        h.setPeso(peso);
        h.setLinhas(linhas);
        h.setKmspercorridos(kms);
        this.entregas.add(h.clone());
    }

    /**
     * ASSOCIAR CONTA CLIENTE, LOJA, TRANSPORTADORA, VOLUNTARIO,ENCOMENDA,HISTORICO
     */
    public void associaEncomenda(String codEncomenda, String codUtilizador, String codLoja, double peso, ArrayList<LinhaEncomenda> linha) throws EncomendaExisteException{
        if(ExisteEncomenda(codEncomenda))
            throw new EncomendaExisteException("Já existe um registo com este código de encomenda");
        Encomenda e= new Encomenda();
        e.setCodEncomenda(codEncomenda);
        e.setCodUtilizador(codUtilizador);
        e.setCodLoja(codLoja);
        e.setPeso(peso);
        e.setLinhas(linha);
        this.encomendas.put(e.getcodEncomenda(),e.clone());
    }

    public void associaAceite(String codEncomenda, String codUtilizador, String codLoja, double peso, ArrayList<LinhaEncomenda> linha)throws EncomendaExisteException{
       if(ExisteAceite(codEncomenda))
           throw new EncomendaExisteException("Já existe um registo com este código de encomenda");
       Encomenda e=new Encomenda();
       e.setCodEncomenda(codEncomenda);
       e.setCodUtilizador(codUtilizador);
       e.setCodLoja(codLoja);
       e.setPeso(peso);
       e.setLinhas(linha);
       this.encomendasdisponiveis.put(e.getcodEncomenda(),e.clone());
    }

    public void associaLoja(String cod, String nome, Localizacao gps, String user, String pass, CatalogoProdutos cp) throws LojaExisteException,UsernameJaEstaEmUsoException {
        if (userEmUso(user))
            throw new UsernameJaEstaEmUsoException("Já existe um registo com este username");

        LogLoja l = new LogLoja();
        l.setCodLoja(cod);
        l.setNome(nome);
        l.setGps(gps);
        l.setUsername(user);
        l.setPassword(pass);
        l.setCatalogoProdutos(cp);
        if (ExisteLoja(user)) throw new LojaExisteException("Já existe um registo para esse username");
        this.lojas.put(l.getUsername(), l.clone());
    }

    public void associaVoluntario(String cod,String nome, Localizacao gps, double raio, String user, String pass, boolean tf) throws VoluntarioExisteException,UsernameJaEstaEmUsoException {
        if(userEmUso(user))
            throw new UsernameJaEstaEmUsoException("Já existe um registo com este username");

        LogVoluntario v= new LogVoluntario();
        v.setCodVoluntario(cod);
        v.setNome(nome);
        v.setGps(gps);
        v.setRaio(raio);
        v.setUsername(user);
        v.setPassword(pass);
        v.setDisponibilidade(tf);
        v.setClassificacoes(new ArrayList<>());
        if(ExisteVoluntario(user)) throw new VoluntarioExisteException("Já existe um registo para esse username");
        this.voluntarios.put(v.getUsername(),v.clone());
    }

    public void associaUtilizador(String cod,String nome, Localizacao pos, String username, String pass) throws UtilizadorExisteException,UsernameJaEstaEmUsoException{
        if(userEmUso(username))
            throw new UsernameJaEstaEmUsoException("Ja existe um registo com este username");

        LogUtilizador u=new LogUtilizador();
        u.setCodUtilizador(cod);
        u.setNome(nome);
        u.setGps(pos);
        u.setUsername(username);
        u.setPassword(pass);
        if(ExisteUtilizador(username)) throw new UtilizadorExisteException("Já existe um registo para esse username");
        this.utilizadores.put(u.getUsername(), u.clone());
    }

    public void associaTransportadora(String cod,String nome, Localizacao gps, String nif, double raio, double precokm, String user, String password) throws TransportadoraExisteException,UsernameJaEstaEmUsoException{
        if(userEmUso(user))
            throw new UsernameJaEstaEmUsoException("Ja existe um registo com este username");

        LogTransportadora t=new LogTransportadora();
        t.setCodEmpresa(cod);
        t.setNome(nome);
        t.setGps(gps);
        t.setNif(nif);
        t.setRaio(raio);
        t.setPrecokm(precokm);
        t.setUsername(user);
        t.setPassword(password);
        if(ExisteTransportadora(user)) throw new TransportadoraExisteException("Já existe um registo para esse username");
        this.transportadoras.put(t.getUsername(),t.clone());
    }

    //método que cria um novo código para
    //utilizador
    public String novoCodUtilizador(){
        int count=0;
        String cod;
        for(LogUtilizador lu: this.utilizadores.values()){
            cod=lu.getCodUtilizador().substring(1);
            if (Integer.parseInt(cod)>count) count=Integer.parseInt(cod);
        }
        Random rand= new Random();
        String confirma="u"+count;
        while(ExisteCodUser(confirma)){
            confirma="u"+ rand.nextInt(count + 2);
        }

        return confirma;
    }

    //transportadora
    public String novoCodTransportadora(){
        int count=0;
        String cod;
        for(LogTransportadora lt: this.transportadoras.values()){
            cod=lt.getCodEmpresa().substring(1);
            if(Integer.parseInt(cod)>count) count=Integer.parseInt(cod);
        }
        Random rand=new Random();
        String confirma="t"+count;
        while(ExisteCodTrans(confirma)){
            confirma="t"+rand.nextInt(count+2);
        }
        return confirma;
    }

    //voluntário
    public String novoCodVoluntario(){
        int count=0;
        String cod;
        for(LogVoluntario lt: this.voluntarios.values()){
            cod=lt.getCodVoluntario().substring(1);
            if(Integer.parseInt(cod)>count) count=Integer.parseInt(cod);
        }
        Random rand=new Random();
        String confirma="v"+count;
        while(ExisteCodTrans(confirma)){
            confirma="v"+rand.nextInt(count+2);
        }
        return confirma;
    }

    //encomenda
    public String novoCodEncomenda(){
        int count=0;
        String cod;
        for(Encomenda e: this.encomendas.values()){
            cod=e.getcodEncomenda().substring(1);
            if(Integer.parseInt(cod)>count) count=Integer.parseInt(cod);
        }
        for(Encomenda e: this.encomendasdisponiveis.values()){
            cod=e.getcodEncomenda().substring(1);
            if(Integer.parseInt(cod)>count) count=Integer.parseInt(cod);
        }
        for(Historico h: this.historico.values()){
            cod=h.getcodEncomenda().substring(1);
            if(Integer.parseInt(cod)>count) count=Integer.parseInt(cod);
        }
        for(Historico h: this.entregas){
            cod=h.getcodEncomenda().substring(1);
            if(Integer.parseInt(cod)>count) count=Integer.parseInt(cod);
        }
        Random rand=new Random();
        String confirma="e"+count;
        while(ExisteEncomenda(confirma)||ExisteAceite(confirma)||ExisteHistorico(confirma)){
            confirma="e"+rand.nextInt(count+2);
        }
        return confirma;
    }

    //loja
    public String novoCodLoja(){
        int count=0;
        String cod;
        for(LogLoja lt: this.lojas.values()){
            cod=lt.getCodLoja().substring(1);
            if(Integer.parseInt(cod)>count) count=Integer.parseInt(cod);
        }
        Random rand=new Random();
        String confirma="l"+count;
        while(ExisteCodLoja(confirma)){
            confirma="l"+rand.nextInt(count+2);
        }
        return confirma;
    }

    //produto
    public String novoCodProduto(String username){
        int count=0;
        String cod;
        LogLoja l=this.lojas.get(username);
        for(Produto p: l.getCatalogoProdutos().getProdutos()){
            cod=p.getCodProd().substring(1);
            if(Integer.parseInt(cod)>count) count=Integer.parseInt(cod);
        }
        Random rand=new Random();
        String confirma="p"+count;
        while(produtoExiste(username,confirma)){
            confirma="p"+rand.nextInt(count+2);
        }
        return confirma;
    }

    //mudar a localização
    //de um utilizador
    public void setLocalizacaoUtilizador(String username, double x, double y){
        Localizacao loc=new Localizacao(x,y);
        this.utilizadores.get(username).setGps(loc);
    }

    //de uma transportadora
    public void setLocalizacaotransportadora(String username,double x, double y){
        Localizacao loc=new Localizacao(x,y);
        this.transportadoras.get(username).setGps(loc);
    }

    //de um voluntário
    public void setLocalizacaovoluntario(String username, double x, double y){
        Localizacao loc=new Localizacao(x,y);
        this.voluntarios.get(username).setGps(loc);
    }

    //de uma Loja
    public void setLocalizacaoloja(String username, double x,double y){
        Localizacao loc = new Localizacao(x,y);
        this.lojas.get(username).setGps(loc);
    }

    //metodo que verifica se um deteminado produto existe
    public boolean produtoExiste(String user, String CodProd){
        LogLoja log=this.lojas.get(user);
        List<Produto> p=log.getCatalogoProdutos().getProdutos();
        for (Produto pr: p){
            if (pr.getCodProd().equals(CodProd))
                return true;
        }
        return false;
    }

    //método que adiciona um novo produto ao catalogo de uma determinada loja
    public void addProduto(String user,Produto p){
        LogLoja log=this.lojas.get(user);
        CatalogoProdutos cp=log.getCatalogoProdutos();
        ArrayList<Produto> pr=cp.getProdutos();
        pr.add(p);
        cp.setProdutos(pr);
        log.setCatalogoProdutos(cp);
    }

    //método que atualiza o stock de um produto
    public void updatestock(String user, double stock, String cod){
        LogLoja l=this.lojas.get(user);
        CatalogoProdutos cp = l.getCatalogoProdutos();
        ArrayList<Produto> pr=cp.getProdutos();
        for(Produto prod: pr){
            if(prod.getCodProd().equals(cod)){
                prod.setStock(stock);
            }
        }
        cp.setProdutos(pr);
        l.setCatalogoProdutos(cp);
    }

    //método que remove um produto de uma determinada loja
    public void removeProduto(String user, Produto x) {
        LogLoja log=this.lojas.get(user);
        ArrayList<Produto> p=log.getCatalogoProdutos().getProdutos();
        p.removeIf(pr -> pr.equals(x));
        CatalogoProdutos c= new CatalogoProdutos(log.getCodLoja(),p);
        log.setCatalogoProdutos(c);
    }

    //método que reduz o stock de um produto de uma loja
    public void reduzStock(String user, String codprod, double stock){
        LogLoja log=this.lojas.get(user);
        List<Produto> pr=log.getCatalogoProdutos().getProdutos();
        for(Produto p: pr){
            if(p.getCodProd().equals(codprod)){
                p.setStock(p.getStock()-stock);
            }
        }
    }

    public Produto buscaProduto(String user,String cod) {
        LogLoja log=this.lojas.get(user);
        ArrayList<Produto> pr= log.getCatalogoProdutos().getProdutos();
        for(Produto p: pr){
            if (p.getCodProd().equals(cod)){
                return p;
            }
        }
        return null;
    }

    //método que adicona uma classificação
    //a uma transportadora
    public void classifTrans(String cod, Classificacao classif) throws TransportadoraNaoExisteException{
        if(!ExisteCodTrans(cod))
            throw new TransportadoraNaoExisteException("A empresa transportadora não existe!");
        ArrayList<Classificacao> cl= getTrans(cod).getClassificacoes();
        cl.add(classif);
        getTrans(cod).setClassificacoes(cl);
    }

    //a um voluntario
    public void classifVol(String cod, Classificacao classif) throws VoluntarioNaoExisteException{
        if(!ExisteCodVoluntario(cod))
            throw new VoluntarioNaoExisteException("O voluntário não existe!");
        ArrayList<Classificacao> cl= getVoluntario(cod).getClassificacoes();
        cl.add(classif);
        getTrans(cod).setClassificacoes(cl);
    }

    //método que determina a classificacao média
    //das transportadoras
    public void classifMediaTrans(LogTransportadora lt){
        ArrayList<Classificacao> cl =lt.getClassificacoes();
        if (cl.isEmpty()){
            System.out.println("Esta transportadora ainda não recebeu nenhuma classificação");
        }
        else {
            double count = 0;
            for (Classificacao c : cl) {
                count += c.getClassificacao();
            }
            System.out.println(count/cl.size());
        }

    }

    //dos voluntarios
    public void classifMediaVol(String user){
        ArrayList<Classificacao> cl=this.voluntarios.get(user).getClassificacoes();
        if (cl.isEmpty())
            System.out.println("Este voluntário ainda não foi classificado");
        else {
            double count = 0;
            for (Classificacao c : cl) {
                count += c.getClassificacao();
            }
            System.out.println(count/cl.size());
        }

    }

    //método para uma loja poder alterar o preço de um produto
    public void mudaPreco(String user,String cod, double preco) throws LojaNaoExisteException{
        if(!ExisteLoja(user)){
            throw new LojaNaoExisteException("Loja não existe");
        }
        LogLoja l=this.lojas.get(user);
        for(Produto p:l.getCatalogoProdutos().getProdutos()){
            if(p.getCodProd().equals(cod)){
                p.setPreco(preco);
            }
        }
    }

    //método para obter a localizacao
    //utilizador
    public Localizacao getLocalizacaoUtilizador(String username){
        Localizacao b = new Localizacao();
        for(LogUtilizador c : this.getUtilizadores().values()){
            if(c.getUsername().equals(username))
                b =  c.getGps();
        }
        return b;
    }

    //Loja
    public Localizacao getLocalizacaoLoja(String username){
        Localizacao b = new Localizacao();
        for(LogLoja c : this.getLojas().values()){
            if(c.getUsername().equals(username))
                b =  c.getGps();
        }
        return b;
    }

    //Transportadora
    public Localizacao getLocalizacaoTransportadora(String username){
        Localizacao b = new Localizacao();
        for(LogTransportadora c : this.getTrasnportadoras().values()){
            if(c.getUsername().equals(username))
                b =  c.getGps();
        }
        return b;
    }

    //Voluntário
    public Localizacao getLocalizacaoVoluntario(String username){
        Localizacao b = new Localizacao();
        for(LogVoluntario c : this.getVoluntarios().values()){
            if(c.getUsername().equals(username))
                b =  c.getGps();
        }
        return b;
    }

    //Método que ordena as lojas por ordem alfabética
    public Set<String> lojasOrdemAlfabetica() throws NaoExisteLojasRegistadasException{
        if(this.getLojas().isEmpty()){
            throw new NaoExisteLojasRegistadasException("Nao existem lojas registadas!");
        }
        Set<String> s = new TreeSet<>();
        for(LogLoja a : this.getLojas().values()){
            s.add(a.getNome());
        }
        return s;
    }

    //Método que retorna uma lista de encomendas recebida por uma loja
    public ArrayList<Encomenda> buscaEncomendas(String cod) throws LojaNaoExisteException {
        if (!ExisteCodLoja(cod))
            throw new LojaNaoExisteException("Esta Loja Não Existe!");
        ArrayList<Encomenda> e = new ArrayList<>();

        for(Encomenda enc: this.encomendas.values()){
            if(enc.getcodLoja().equals(cod)){
                e.add(enc);
            }
        }
        return e;
    }

    //Método imprime as encomendas que uma loja recebeu
    public void buscaEncomendasDisplay(ArrayList<Encomenda> enc){
        for (Encomenda encomenda : enc) {
            System.out.println(encomenda);
        }
    }

    //Método que retorna uma lista com todas as encomendas aceites por uma loja
    public ArrayList<Historico> buscaHistoricoLoja(String cod) throws LojaNaoExisteException {
        if (!ExisteCodLoja(cod))
            throw new LojaNaoExisteException("Esta Loja Não Existe!");
        ArrayList<Historico> e = new ArrayList<>();

        for(Historico enc: this.historico.values()){
            if(enc.getcodLoja().equals(cod)){
                e.add(enc);
            }
        }
        return e;
    }

    //Método que retorna uma lista com todas encomendas entregues por uma transportadora
    public ArrayList<Historico> buscaHistoricoTransportadora(String cod) throws TransportadoraNaoExisteException{
        if(!ExisteCodTrans(cod))
            throw new TransportadoraNaoExisteException("Esta Transportadora Não Existe!");
        ArrayList<Historico> enc=new ArrayList<>();

        for(Historico e: this.historico.values()){
            if(e.getCod().equals(cod)){
                enc.add(e);
            }
        }
        return enc;
    }

    //Método que retorna uma lista com todas as encomendas entregues por um voluntário
    public ArrayList<Historico> buscaHistoricoVoluntario(String cod) throws VoluntarioNaoExisteException{
        if(!ExisteCodVoluntario(cod))
            throw new VoluntarioNaoExisteException("Este Voluntário Não Existe!");
        ArrayList<Historico> enc= new ArrayList<>();

        for(Historico e: this.historico.values()){
            if(e.getCod().equals(cod)){
                enc.add(e);
            }
        }
        return enc;
    }

    //Método que retorna uma lista com todas as encomendas feitas por um utilizador
    public ArrayList<Historico> buscaHistoricoUtilizador(String cod) throws UtilizadorNaoExisteException{
        if(!ExisteCodUser(cod))
            throw new UtilizadorNaoExisteException("Este Utilizador Não Existe!");
        ArrayList<Historico> enc= new ArrayList<>();

        for(Historico e: this.historico.values()){
            if(e.getcodUtilizador().equals(cod)){
                enc.add(e);
            }
        }
        return enc;
    }

    //Método que faz print de uma lista de históricos de encomenda
    public void buscaHistoricoDisplay(ArrayList<Historico> hist){
        if (hist.isEmpty()){
            System.out.println("O seu histórico está vazio!\n");
        }
        else {
            for (Historico h : hist) {
                System.out.println(h.toString());
            }
        }
    }

    //método para devolver um utilizador através do seu código
    public LogUtilizador getUtilizador(String cod){
        for(LogUtilizador c : this.utilizadores.values()){
            if(c.getCodUtilizador().equals(cod))
                return c;
        }
        return null;
    }

    //método para devolver uma loja através do seu código
    public LogLoja getLoja(String cod){
        for(LogLoja c : this.lojas.values()){
            if(c.getCodLoja().equals(cod))
                return c;
        }
        return null;
    }

    //método para devolver uma transportadora através do seu código
    public LogTransportadora getTrans(String cod){
        for(LogTransportadora c : this.transportadoras.values()){
            if(c.getCodEmpresa().equals(cod))
                return c;
        }
        return null;
    }

    //método para devolver um voluntário através do seu código
    public LogVoluntario getVoluntario(String cod){
        for(LogVoluntario c : this.voluntarios.values()){
            if(c.getCodVoluntario().equals(cod))
                return c;
        }
        return null;
    }

    //método que busca todas as transportadoras cujo o raio alcança o cliente e a loja em questão
    public ArrayList<LogTransportadora> buscatransportadoras(String codutilizador, String codloja) {
        ArrayList<LogTransportadora> lt= new ArrayList<>();
        for(LogTransportadora t: this.transportadoras.values()){
            if ( t.getGps().distLocalizacao(getUtilizador(codutilizador).getGps())<t.getRaio() && t.getGps().distLocalizacao(getLoja(codloja).getGps())<t.getRaio() ){
                lt.add(t);
            }
        }
        return lt;
    }

    //método que dá display das transportadoras
    public void transportadoraDisplay(ArrayList<LogTransportadora> lt){
        for(LogTransportadora t: lt){
            System.out.println(t.getCodEmpresa());
            System.out.println(t.getNome());
            System.out.println(t.getPrecokm()+"€/km");
            classifMediaTrans(t);
            System.out.print("\n");
        }
    }

    //método que printa o estado de uma encomenda
    public void estadodeEncomenda(String codEncomenda) throws EncomendaNaoExisteException{
        if(!ExisteEncomenda(codEncomenda) && !ExisteAceite(codEncomenda) && !ExisteHistorico(codEncomenda) && !ExisteEntrega(codEncomenda))
            throw new EncomendaNaoExisteException("Não existe nenhuma encomenda com este registo");
        for(Encomenda e: this.encomendas.values()){
            if (e.getcodEncomenda().equals(codEncomenda)) {
                System.out.println("A encomenda " + codEncomenda + " ainda não foi aceite");
                break;
            }
        }
        for(Encomenda e: this.encomendasdisponiveis.values()){
            int c=0;
            if(e.getcodEncomenda().equals(codEncomenda)) {
                for (Pair<String, String> p : this.flags) {
                    if (p.getValue1().equals(codEncomenda)) {
                        System.out.println("A encomenda " + codEncomenda + " está a espera pela transportadora " + p.getValue0());
                        c=1;
                    }
                }
                if (c==0){
                    System.out.println("A encomenda " + codEncomenda + " está a espera por ser aceite por um voluntário");
                }
            }
        }
        for(Historico h: this.entregas){
            if(h.getcodEncomenda().equals(codEncomenda)){
                if(h.getCod().charAt(0)=='v') {
                    System.out.println("A encomenda " + codEncomenda + " está em caminho pelo voluntário " + h.getCod());
                    LocalDateTime date=h.getDate();
                    Pair<Integer,Integer> p=calculaTempoVol(h.getCod(),h.getcodLoja(),h.getcodUtilizador());
                    int horas=p.getValue0();
                    int min=p.getValue1();
                    System.out.println("O voluntário foi buscar a sua encomenda às: " +date);
                    System.out.println("O tempo de espera estimado é: "+horas+"horas e"+min+"minutos");
                    System.out.println("Entrega prevista pras: " +LocalDateTime.of(date.getYear(),date.getMonth(),date.getDayOfMonth(),date.getHour()+horas,date.getMinute()+min));
                }
                else if (h.getCod().charAt(0)=='t') {
                    System.out.println("A encomenda " + codEncomenda + " está em caminho pela transportadora " + h.getCod());
                    LocalDateTime date=h.getDate();
                    Pair<Integer,Integer> p=calculaTempoTrans(h.getCod(),h.getcodLoja(),h.getcodUtilizador());
                    int horas=p.getValue0();
                    int min=p.getValue1();
                    System.out.println("A empresa enviou um estafeta às: " +date.getHour()+":"+date.getMinute());
                    System.out.println("O tempo de espera estimado é: "+horas+" horas e "+min+" minutos");
                    LocalDateTime prevista=date.plusHours(horas);
                    prevista=prevista.plusMinutes(min);
                    System.out.println("Entrega prevista pras: " +prevista.getHour()+":"+prevista.getMinute()+" do dia "+prevista.getDayOfMonth());
                }
            }
        }
        for(Historico h: this.historico.values()){
            if(h.getcodEncomenda().equals(codEncomenda)){
                System.out.println("A encomenda "+ codEncomenda + "já foi entregue");
            }
        }
    }

    //método que vai buscar todas as transportadoras e voluntários associados a um cliente que ainda não foram classificados
    public ArrayList<String> buscapraClassificar(String username){
        ArrayList<String> encs=new ArrayList<>();
        for(String cod: this.classifica){
            if(this.historico.get(cod).getcodUtilizador().equals(this.utilizadores.get(username).getCodUtilizador())){
                encs.add(cod);
            }
        }
        return encs;
    }

    //método para dar display das transportadoras e voluntários que fizeram uma entrega e ainda não foram calssificados
    public void displaypraClassificar(ArrayList<String> encs){
        for(String s: encs){
            System.out.println(this.historico.get(s).getCod());
            System.out.println(this.historico.get(s).getNome());
            System.out.println(s);
        }
    }

    //método que devolve uma lista de encomendas disponiveis para um empresa transportadora entregar
    public ArrayList<Encomenda> buscaEncomendasTransportadora(String username){
        LogTransportadora t=this.transportadoras.get(username);
        ArrayList<Encomenda> encs=new ArrayList<>();
        ArrayList<String> cods=new ArrayList<>();

        for(Pair<String,String> flag: this.flags){
            if(t.getCodEmpresa().equals(flag.getValue0()))
                cods.add(flag.getValue1());
        }

        for(Encomenda e:this.encomendasdisponiveis.values()){
            LogLoja l=getLoja(e.getcodLoja());
            LogUtilizador u=getUtilizador(e.getcodUtilizador());
            if((cods.contains(e.getcodEncomenda())) && (t.getGps().distLocalizacao(u.getGps())<t.getRaio()) && (t.getGps().distLocalizacao(l.getGps())<t.getRaio()))
                encs.add(e);
        }

        return encs;
    }

    //método que devolve uma lista de encomendas disponiveis para os voluntários
    public ArrayList<Encomenda> buscaEncomendaVoluntario(String username){
        ArrayList<Encomenda> encs=new ArrayList<>();
        LogVoluntario v=this.voluntarios.get(username);

        for(Encomenda e: this.encomendasdisponiveis.values()){
            LogLoja l=getLoja(e.getcodLoja());
            LogUtilizador u=getUtilizador(e.getcodUtilizador());
            if(this.vols.contains(e.getcodEncomenda()) && v.getGps().distLocalizacao(u.getGps())<v.getRaio() && v.getGps().distLocalizacao(l.getGps())<v.getRaio()){
                encs.add(e);
            }
        }
        return encs;
    }

    //metodo para calcular o tempo estimado de entrega feita por voluntario
    public Pair<Integer,Integer> calculaTempoVol(String codVoluntario, String codLoja, String codUtilizador){
        int horas =0;
        int minutos = 0;
        double velocidade = 50;
        double distVoluntarioLoja = getVoluntario(codVoluntario).getGps().distLocalizacao(getLoja(codLoja).getGps());
        double distLojaCliente = getLoja(codLoja).getGps().distLocalizacao(getUtilizador(codUtilizador).getGps());
        double distanciaTotal = distLojaCliente + distVoluntarioLoja;
        double tempo = distanciaTotal/velocidade;
        while(tempo!=0) {
            if (tempo > 1) {
                tempo -= 1;
                horas += 1;
            }
            else{
                minutos += (int) (tempo *60);
                tempo-=tempo;
            }
        }
        Pair<Integer,Integer> p = new Pair<>(horas,minutos);
        return p;
    }

    //metodo para calcular o tempo estimado de entrega feita por transportadora
    public Pair<Integer,Integer> calculaTempoTrans(String codTransportadora, String codLoja, String codUtilizador){
        int horas =0;
        int minutos = 0;
        double velocidade = 50;
        double distTransLoja = getTrans(codTransportadora).getGps().distLocalizacao(getLoja(codLoja).getGps());
        double distLojaCliente = getLoja(codLoja).getGps().distLocalizacao(getUtilizador(codUtilizador).getGps());
        double distanciaTotal = distLojaCliente + distTransLoja;
        double tempo = distanciaTotal/velocidade;
        while(tempo!=0) {
            if (tempo > 1) {
                tempo -= 1;
                horas += 1;
            }
            else{
                minutos += (int) (tempo *60);
                tempo-=tempo;
            }
        }
        Pair<Integer,Integer> p = new Pair<>(horas,minutos);
        return p;
    }

    //método para calcular uma lista de encomendas de uma determinada empresa na lista de entregas
    public ArrayList<Historico> buscaEncomendaEntregueTrans(String username){
        ArrayList<Historico> encs=new ArrayList<>();

        for(Historico h:this.entregas){
            if(h.getCod().equals(this.transportadoras.get(username).getCodEmpresa()))
                encs.add(h);
        }

        return encs;
    }

    //método para retornar a encomenda que um voluntário está a entregar
    public Historico buscaEncomendaEntregueVol(String username){
        Historico hist=new Historico();
        for(Historico h:this.entregas){
            if(h.getCod().equals(this.voluntarios.get(username).getCodVoluntario()))
                hist=h;
        }
        return hist;
    }

    //método para que conta o número de encomendas que um utilizador fez
    public int contaHistoricoUser(String cod) {
        ArrayList<Historico> hist = new ArrayList<>();
        for (Historico h: this.historico.values()){
            if(h.getcodUtilizador().equals(cod))
                hist.add(h);
        }
        return hist.size();
    }

    //listagem do top10 de utilizadores com mais encomendas feitas
    public List<LogUtilizador> top10utilizadores(){
        List<LogUtilizador> res= new ArrayList<>();
        Comparator<LogUtilizador> cmpr= (u1,u2)-> Integer.compare(contaHistoricoUser(u2.getCodUtilizador()), contaHistoricoUser(u1.getCodUtilizador()));
        Set<LogUtilizador> ret=new TreeSet<>(cmpr);
        double counter=0;
        for(LogUtilizador u: this.utilizadores.values()){
            ret.add(u.clone());
        }
        Iterator it =ret.iterator();
        while(it.hasNext() && counter <10){
            LogUtilizador lu= (LogUtilizador) it.next();
            res.add(lu.clone());
            counter++;
        }
        return res;
    }

    //método que conta o número total de kms percorridos que uma empresa transportadora fez
    public double contaKmsTrans(String cod){
        List<Historico> hist= new ArrayList<>();
        for(Historico h: this.historico.values()){
            if(h.getCod().equals(cod)){
                hist.add(h);
            }
        }
        double count=0;
        for(Historico h: hist){
            count+=h.getKmspercorridos();
        }
        return count;
    }

    public List<LogTransportadora> top10transportadoras(){
        List<LogTransportadora> res=new ArrayList<>();
        Comparator<LogTransportadora> cmpr= Comparator.comparingDouble(t -> contaKmsTrans(t.getCodEmpresa()));
        Set<LogTransportadora> ret= new TreeSet<>(cmpr);
        double count=0;
        for(LogTransportadora t: this.transportadoras.values()){
            ret.add(t.clone());
        }
        Iterator it= ret.iterator();
        while (it.hasNext()&& count<10){
            LogTransportadora t=(LogTransportadora) it.next();
            res.add(t.clone());
            count++;
        }
        return res;
    }

    public void displaytopusers(List<LogUtilizador> users){
        if(users.isEmpty())
            System.out.println("Ainda não foi pedida nenhuma encomenda usando o TrazAqui!");
        for(LogUtilizador u: users){
            System.out.println("Utilizador:");
            System.out.println(u.getCodUtilizador());
            System.out.println(u.getUsername());
        }
    }

    public void displaytoptrans(List<LogTransportadora> trans){
        if(trans.isEmpty())
            System.out.println("Ainda não foi realizada nenhuma encomenda por uma transportadora");
        for(LogTransportadora t: trans){
            System.out.println("Transportadora:");
            System.out.println(t.getCodEmpresa());
            System.out.println(t.getNome());
            classifMediaTrans(t);
        }
    }

    public void atualizaQuantidade(String codLoja, String opcao, double q) throws ProdutoNaoExisteException{
        Produto p = getLoja(codLoja).getCatalogoProdutos().getProduto(opcao);
        if (produtoExiste(getLoja(codLoja).getUsername(), opcao)) {
            double quantidade = getLoja(codLoja).getCatalogoProdutos().getProduto(opcao).getStock();
            if(q!=quantidade) {
                reduzStock(getLoja(codLoja).getUsername(),opcao,q);
            }
            else{
                removeProduto(getLoja(codLoja).getUsername(),p);
            }
        } else {
            throw new ProdutoNaoExisteException("Este Produto Não Existe!");
        }
    }

    //método para dar display a encomendas que ainda nao foram entregues
    public void displayencsnaoentregues(String username){
        LogUtilizador u=this.utilizadores.get(username);
        for(Encomenda e:this.encomendas.values()){
            if(e.getcodUtilizador().equals(u.getCodUtilizador()))
                System.out.println(e.getcodEncomenda());
        }
        for(Encomenda e:this.encomendasdisponiveis.values()){
            if(e.getcodUtilizador().equals(u.getCodUtilizador()))
                System.out.println(e.getcodEncomenda());
        }
        for(Historico h:this.entregas){
            if(h.getcodUtilizador().equals(u.getCodUtilizador())) {
                System.out.println(h.getcodEncomenda());

            }
        }
    }
}

