package trazaqui;

import trazaqui.Exceptions.*;

import java.io.Serializable;
import java.util.*;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class Armazena implements Serializable {
    private Map<String,Utilizador> utilizadores;
    private Map <String,Loja> lojas;
    private Map <String,Transportadora> transportadoras;
    private Map <String,Voluntario> voluntarios;
    private Map <String,Encomenda> encomendas;
    private ArrayList<CatalogoProdutos> produtos;
    private ArrayList<String> aceites;
    /**
     * Construtor vazio
     */
    public Armazena(){
        this.utilizadores = new HashMap<>();
        this.lojas = new HashMap<>();
        this.transportadoras = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.encomendas = new HashMap<>();
        this.produtos=new ArrayList<>();
        this.aceites=new ArrayList<>();
    }

    /**
     * Metodo construtor parameterizado
     */
    public Armazena( Map<String,Utilizador> u, Map<String,Loja> l, Map<String,Transportadora> t, Map<String,Voluntario> v,Map<String,Encomenda> e,ArrayList<CatalogoProdutos> produtos, ArrayList<String> aceites, Map<String,Encomenda> h){
        setUtilizadores(u);
        setLojas(l);
        setTransportadoras(t);
        setVoluntarios(v);
        setEncomendas(e);
        setCatalogos(produtos);
        setAceites(aceites);
    }

    /**
     * Metodo construtor por copia
     * @param list Objeto da classe BaseDados
     */
    public Armazena(Armazena list){
        setUtilizadores(list.getUtilizadores());
        setLojas(list.getLojas());
        setTransportadoras(list.getTransportadoras());
        setVoluntarios(list.getVoluntarios());
        setEncomendas(list.getEncomendas());
        setCatalogos(list.getCatalogos());
        setAceites(list.getAceites());
    }

    public ArrayList<CatalogoProdutos> getCatalogos(){
        return new ArrayList<>(this.produtos);
    }

    public void setCatalogos(ArrayList<CatalogoProdutos> produtos){
        this.produtos=new ArrayList<>(produtos);
    }

    public ArrayList<String> getAceites(){return new ArrayList<>(this.aceites);}

    public void  setAceites(ArrayList<String> aceites){this.aceites=new ArrayList<>(aceites);}

    /**
     * Retorna um cliente especifico com base no seu Username
     * @param cod O username do cliente
     * @return Um utilizador
     */
    public Utilizador getUtilizador(String cod){
        for(Utilizador c : this.utilizadores.values()){
            if(c.getCodUtilizador().equals(cod))
                return c;        }
        return null;
    }

    public Loja getLoja(String cod){
        for(Loja c : this.lojas.values()){
            if(c.getCodLoja().equals(cod))
                return c;        }
        return null;
    }

    public Transportadora getTransportadora(String cod){
        for(Transportadora c : this.transportadoras.values()){
            if(c.getCodEmpresa().equals(cod))
                return c;        }
        return null;
    }

    public Voluntario getVoluntario(String cod){
        for(Voluntario c : this.voluntarios.values()){
            if(c.getCodVoluntario().equals(cod))
                return c;        }
        return null;
    }

    public Encomenda getEncomenda(String cod){
        for(Encomenda c : this.encomendas.values()){
            if(c.getcodEncomenda().equals(cod))
                return c;        }
        return null;
    }

    public Produto getProduto(String cod){
        Produto produto = new Produto();
        for(CatalogoProdutos c : this.produtos) {
            try {
                produto= c.getProduto(cod);
            } catch (ProdutoNaoExisteException e) {
                System.out.println(e.getMessage());
            }
        }
        return produto;
    }

    /**
     * Metodo para obter os utilizadores
     * @return Lista dos utilizadores
     */
    public Map<String,Utilizador> getUtilizadores(){
        Map<String,Utilizador> ret = new HashMap<>();
        for(Utilizador p : this.utilizadores.values()){
            ret.put(p.getCodUtilizador(),p.clone());
        }
        return ret;
    }

    /**
     * Metodo para obter as lojas
     * @return Lista das lojas
     */
    public Map<String,Loja> getLojas(){
        Map<String,Loja> ret = new HashMap<>();
        for(Loja p : this.lojas.values()){
            ret.put(p.getCodLoja(),p.clone());
        }
        return ret;
    }

    /**
     * Metodo para obter as transportadoras
     * @return Lista das transportadoras
     */
    public Map<String,Transportadora> getTransportadoras(){
        Map<String,Transportadora> ret = new HashMap<>();
        for(Transportadora p : this.transportadoras.values()){
            ret.put(p.getCodEmpresa(),p.clone());
        }
        return ret;
    }

    /**
     * Metodo para obter os voluntarios
     * @return Lista dos voluntarios
     */
    public Map<String,Voluntario> getVoluntarios(){
        Map<String,Voluntario> ret = new HashMap<>();
        for(Voluntario p : this.voluntarios.values()){
            ret.put(p.getCodVoluntario(),p.clone());
        }
        return ret;
    }

    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> ret = new HashMap<>();
        for(Encomenda p : this.encomendas.values()){
            ret.put(p.getcodEncomenda(),p.clone());
        }
        return ret;
    }

    /**
     * Adicionar um utilizador a lista de utilizadores
     * @param u um utilizador
     *
     */
    public void addUtilizador(Utilizador u){
        this.utilizadores.put(u.getCodUtilizador(),u.clone());
    }

    /**
     * Adicionar uma loja a lista de lojas
     * @param l uma loja
     *
     */
    public void addLoja(Loja l){
        this.lojas.put(l.getCodLoja(),l.clone());
    }

    /**
     * Adicionar um transportadora a lista de transportadoras
     * @param t uma transportadora
     *
     */
    public void addTransportadora(Transportadora t){
        this.transportadoras.put(t.getCodEmpresa(),t.clone());
    }

    /**
     * Adicionar um voluntario a lista de voluntarios
     * @param v um voluntario
     *
     */
    public void addVoluntario(Voluntario v){
        this.voluntarios.put(v.getCodVoluntario(),v.clone());
    }

    /**
     * Metodo para definir os utilizadores
     * @param u Lista de utilizadores
     */
    public void setUtilizadores(Map<String,Utilizador> u){
        this.utilizadores = new HashMap<>();
        for(Utilizador  p : u.values()){
            this.utilizadores.put(p.getCodUtilizador(),p.clone());
        }
    }

    /**
     * Metodo para definir as lojas
     * @param l Lista de lojas
     */
    public void setLojas(Map<String,Loja> l){
        this.lojas = new HashMap<>();
        for(Loja  p : l.values()){
            this.lojas.put(p.getCodLoja(),p.clone());
        }
    }

    /**
     * Metodo para definir as transportadoras
     * @param t Lista de transportadoras
     */
    public void setTransportadoras(Map<String,Transportadora> t){
        this.transportadoras = new HashMap<>();
        for(Transportadora p : t.values()){
            this.transportadoras.put(p.getCodEmpresa(),p.clone());
        }
    }

    /**
     * Metodo para definir os voluntarios
     * @param v Lista de voluntarios
     */
    public void setVoluntarios(Map<String,Voluntario> v){
        this.voluntarios = new HashMap<>();
        for(Voluntario  p : v.values()){
            this.voluntarios.put(p.getCodVoluntario(),p.clone());
        }
    }

    public void setEncomendas(Map<String,Encomenda> e){
        this.encomendas = new HashMap<>();
        for(Encomenda  p : e.values()){
            this.encomendas.put(p.getcodEncomenda(),p.clone());
        }
    }

    /**
     * Clonar um objecto da classe BaseDados
     */
    public Armazena clone(){
        return new Armazena(this);
    }



    /**
     * Metodo equals
     * @param o Objeto de uma classe qualquer
     * @return Boolean
     */
    public boolean equals(Object o){
        if(this == o) return true;
        if((o == null) || (o.getClass() != this.getClass())) return false;

        Armazena util = (Armazena) o;

        for(Utilizador u : util.utilizadores.values())
            if(!this.utilizadores.containsValue(u))
                return false;

        for(Loja l : util.lojas.values())
            if(!this.lojas.containsValue(l))
                return false;

        for(Transportadora t : util.transportadoras.values())
            if(!this.transportadoras.containsValue(t))
                return false;

        for(Voluntario v : util.voluntarios.values())
            if(!this.voluntarios.containsValue(v))
                return false;

        return true;
    }

    /**
     * Regista um novo proprietário na base de dados
     */
    public void novoUtilizador(Utilizador u) throws UtilizadorExisteException, CodigoJaEstaEmUsoException{
        if(codEmUso(u.getCodUtilizador()))
            throw new CodigoJaEstaEmUsoException("Ja existe um registo com este codigo");
        Utilizador p = new Utilizador();
        p.setCodUtilizador(u.getCodUtilizador());
        p.setNome(u.getNome());
        p.setGps(u.getGps());
        this.utilizadores.put(p.getCodUtilizador(),p.clone());
    }

    /**
     * Regista um novo cliente na base de dados
     */
    public void novaLoja(Loja l) throws LojaExisteException, CodigoJaEstaEmUsoException{
        if(codEmUso(l.getCodLoja()))
            throw new CodigoJaEstaEmUsoException("Ja existe um registo com este código");
        Loja p = new Loja();
        p.setCodLoja(l.getCodLoja());
        p.setNome(l.getNome());
        p.setGps(l.getGps());
        this.lojas.put(p.getCodLoja(),p.clone());
    }


    public void novaTransportadora(Transportadora t) throws TransportadoraExisteException, CodigoJaEstaEmUsoException{
        if(this.codEmUso(t.getCodEmpresa()))
            throw new CodigoJaEstaEmUsoException("Ja existe um registo com este codigo");
        Transportadora p = new Transportadora();
        p.setCodEmpresa(t.getCodEmpresa());
        p.setNome(t.getNome());
        p.setGps(t.getGps());
        p.setNif(t.getNif());
        p.setRaio(t.getRaio());
        p.setPrecokm(t.getPrecokm());
        this.transportadoras.put(p.getCodEmpresa(),p.clone());
    }

    public void novoVoluntario(Voluntario v) throws VoluntarioExisteException, CodigoJaEstaEmUsoException{
        if(this.codEmUso(v.getCodVoluntario()))
            throw new CodigoJaEstaEmUsoException("Ja existe um registo com este codigo");
        Voluntario p = new Voluntario();
        p.setCodVoluntario(v.getCodVoluntario());
        p.setNome(v.getNome());
        p.setGps(v.getGps());

        this.voluntarios.put(p.getCodVoluntario(),p.clone());
    }

    public void novaEncomenda(Encomenda e) throws EncomendaExisteException, CodigoJaEstaEmUsoException{
        if(this.codEmUso(e.getcodEncomenda()))
            throw new CodigoJaEstaEmUsoException("Ja existe um registo com este codigo");
        Encomenda p = new Encomenda();
        p.setCodEncomenda(e.getcodEncomenda());
        p.setCodUtilizador(e.getcodUtilizador());
        p.setCodLoja(e.getcodLoja());
        p.setPeso(e.getPeso());
        p.setLinhas(e.getLinhas());
        this.encomendas.put(p.getcodEncomenda(),p.clone());
    }

    public void novoCatalogoProdutos(CatalogoProdutos l){
        CatalogoProdutos p = new CatalogoProdutos();
        p.setCodLoja(l.getCodLoja());
        p.setProdutos(l.getProdutos());
        this.produtos.add(p.clone());
    }

    public boolean codEmUso(String cod){
        for(Utilizador u : this.getUtilizadores().values())
            if(u.getCodUtilizador().equals(cod))
                return true;
        for(Loja l : this.getLojas().values())
            if(l.getCodLoja().equals(cod))
                return true;
        for(Transportadora t : this.getTransportadoras().values())
            if(t.getCodEmpresa().equals(cod))
                return true;
        for(Voluntario v : this.getVoluntarios().values())
            if(v.getCodVoluntario().equals(cod))
                return true;
        return false;
    }

    public String toStringUtilizadores(){
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizadores:").append(this.utilizadores);
        return sb.toString();
    }

    public String toStringLojas(){
        StringBuilder sb = new StringBuilder();
        sb.append("Lojas:").append(this.lojas);
        return sb.toString();
    }

    public String toStringTransportadoras(){
        StringBuilder sb = new StringBuilder();
        sb.append(":").append(this.lojas);
        return sb.toString();
    }

    public Set<String> lojasOrdemAlfabetica(){
        Set<String> s = new TreeSet<>();
        for(Loja a : this.getLojas().values()){
            s.add(a.getNome());
        }
        return s;
    }

    //método para juntar vários catalogos da mesma loja
    public void juntaCatalogos(){
        ArrayList<CatalogoProdutos> cp= new ArrayList<>(this.produtos);
        ArrayList<CatalogoProdutos> res= new ArrayList<>();
        ArrayList<CatalogoProdutos> check= new ArrayList<>();
        while(!cp.isEmpty()){
            CatalogoProdutos cat= cp.get(0);
            cp.remove(0);
            for(CatalogoProdutos c: cp){
                if(cat.getCodLoja().equals(c.getCodLoja())){
                        ArrayList<Produto> p=cat.getProdutos();
                        p.addAll(c.getProdutos());
                        cat.setProdutos(p);
                        check.add(c);
                    }
                }
            res.add(cat);
            cp.removeAll(check);
            check=new ArrayList<>();
        }
        setCatalogos(res);
   }

   //método que junta produtos com o mesmo código de um catálogoo
   public void JuntaProdutos(){
        ArrayList<CatalogoProdutos> cp= new ArrayList<>();
        for(CatalogoProdutos c: this.produtos){
            ArrayList<Produto> pr=new ArrayList<>(c.getProdutos());
            ArrayList<Produto> check= new ArrayList<>();
            ArrayList<Produto> res= new ArrayList<>();
            while(!pr.isEmpty()){
                Produto pro=pr.get(0);
                pr.remove(0);
                for(Produto p: pr){
                    if (p.getCodProd().equals(pro.getCodProd())){
                        check.add(p);
                        double s=p.getStock()+pro.getStock();
                        double preco=p.getPreco()+pro.getStock();
                        pro.setStock(s);
                        pro.setPreco(preco);
                    }
                }
                res.add(pro);
                pr.removeAll(check);
                check=new ArrayList<>();
            }
            c.setProdutos(res);
            cp.add(c);
        }
        setCatalogos(cp);
   }

   public boolean checkAceite(Encomenda e){
        for(String c: this.aceites){
            if(e.getcodEncomenda().equals(c)){
                return true;
            }
        }
        return false;
   }

   public void adicionaCodigo(String cod){
        this.aceites.add(cod);
   }

   public boolean existeCodUser(String cod){ return this.utilizadores.containsKey(cod);}

   public boolean existeCodLoja(String cod){return this.lojas.containsKey(cod);}

   public boolean existeCodTrans(String cod){return this.transportadoras.containsKey(cod);}

   public boolean existeCodVol(String cod){return this.voluntarios.containsKey(cod);}
}