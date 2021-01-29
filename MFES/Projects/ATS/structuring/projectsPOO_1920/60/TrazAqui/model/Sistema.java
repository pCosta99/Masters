package model;

import interfaces.*;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que implementa o sistema
 */
public class Sistema implements ISistema, Serializable {
    private Map<String, IEmpresa> transportadoras;
    private Map<String, IVoluntario> voluntarios;
    private Map<String, ILoja> lojas;
    private Map<String, IUser> utilizadores;
    private Map<String, IEncomenda> encomendas;
    private List<String> encomendasAceites;

    /**
     * Construtor vazio do sistema
     */
    public Sistema() {
        this.transportadoras = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.lojas = new HashMap<>();
        this.utilizadores = new HashMap<>();
        this.encomendas = new HashMap<>();
        this.encomendasAceites = new ArrayList<>();
    }

    /**
     * Construtor do sistema
     * @param t empresas
     * @param v voluntários
     * @param l lojas
     * @param u users
     * @param e encomendas
     * @param a encomendas aceites
     */
    public Sistema(Map<String, IEmpresa> t, Map<String, IVoluntario> v, Map<String, ILoja > l,
                   Map<String, IUser> u, Map<String, IEncomenda> e, List<String> a) {
        setTransportadoras(t);
        setVoluntarios(v);
        setLojas(l);
        setUtilizadores(u);
        setEncomendas(e);
        setEncomendasAceites(a);
    }

    /**
     * Construtor do sistema
     * @param s sistema
     */
    public Sistema(Sistema s) {
        setTransportadoras(s.getTransportadoras());
        setVoluntarios(s.getVoluntarios());
        setLojas(s.getLojas());
        setUtilizadores(s.getUtilizadores());
        setEncomendas(s.getEncomendas());
        setEncomendasAceites(s.getEncomendasAceites());
    }

    /**
     * Devolve as transportadoras
     * @return empresas transportadoras
     */
    public Map<String, IEmpresa> getTransportadoras() {
        Map<String, IEmpresa> aux = new HashMap<>();

        this.transportadoras.forEach(aux::put);
        return aux;
    }

    /**
     * Substitui as transportadoras
     * @param t transportadoras
     */
    public void setTransportadoras(Map<String, IEmpresa> t) {
        this.transportadoras = new HashMap<>();

        t.forEach((key, value) -> this.transportadoras.put(key,
                value.clone()));
    }

    /**
     * Devolve os voluntários
     * @return voluntários
     */
    public Map<String, IVoluntario> getVoluntarios() {
        Map<String, IVoluntario> aux = new HashMap<>();

        this.voluntarios.forEach(aux::put);
        return aux;
    }

    /**
     * Substitui os voluntarios
     * @param t voluntarios
     */
    public void setVoluntarios(Map<String, IVoluntario> t) {
        this.voluntarios = new HashMap<>();

        t.forEach((key, value) -> this.voluntarios.put(key,value));
    }

    /**
     * Devolve as lojas
     * @return lojas
     */
    public Map<String,ILoja> getLojas() {
        Map<String, ILoja > aux = new HashMap<>();

        this.lojas.forEach(aux::put);
        return aux;
    }

    /**
     * Substitui as lojas
     * @param l lojas
     */
    public void setLojas(Map<String, ILoja > l) {
        this.lojas = new HashMap<>();

        l.forEach((key, value) -> this.lojas.put(key,
                value));
    }

    /**
     * Devolve os utilizadores
     * @return utilizadores
     */
    public Map<String, IUser> getUtilizadores() {
        Map<String, IUser> aux = new HashMap<>();

        this.utilizadores.forEach(aux::put);
        return aux;
    }

    /**
     * Substitui os utilizadores
     * @param u utilizadores
     */
    public void setUtilizadores(Map<String, IUser> u) {
        this.utilizadores = new HashMap<>();

        u.forEach((key, value) -> this.utilizadores.put(key,
                value));
    }

    /**
     * Devolve as encomendas
     * @return encomendas
     */
    public Map<String, IEncomenda> getEncomendas() {
        Map<String,IEncomenda> aux = new HashMap<>();

        this.encomendas.forEach(aux::put);
        return aux;
    }

    /**
     * Substitui as encomendas
     * @param t encomendas
     */
    public void setEncomendas(Map<String, IEncomenda> t) {
        this.encomendas = new HashMap<>();

        t.forEach((key, value) -> this.encomendas.put(key,value));
    }

    /**
     * Devolve as encomendas aceites
     * @return encomendas aceites
     */
    public List<String> getEncomendasAceites() {

        return new ArrayList<>(this.encomendasAceites);
    }

    /**
     * Substitui as encomendas aceites
     * @param t encomendas aceites
     */
    public void setEncomendasAceites(List<String> t) {
        this.encomendasAceites = new ArrayList<>();

        this.encomendasAceites.addAll(t);
    }

    /**
     * Adiciona um voluntário ao sistema
     * @param v voluntário
     */
    public void addVoluntario(IVoluntario v) {
        this.voluntarios.put(v.getCode(), v.clone());
    }

    /**
     * Adiciona uma empresa ao sistema
     * @param e empresa
     */
    public void addEmpresa(IEmpresa e) {
        this.transportadoras.put(e.getCode(), e.clone());
    }

    /**
     * Adiciona uma loja ao sistema
     * @param l loja
     */
    public void addLoja(ILoja l) {
        this.lojas.put(l.getCode(), l.clone());
    }

    /**
     * Adiciona um utilizador ao sistema
     * @param u user
     */
    public void addUser(IUser u) {
        this.utilizadores.put(u.getCode(), u.clone());
    }

    /**
     * Adiciona uma encomenda ao sistema
     * @param e encomenda
     */
    public void addEncomenda(IEncomenda e) {
        this.encomendas.put(e.getCode(), e.clone());
    }

    /**
     * Adiciona uma encomenda aceite ao sistema
     * @param e encomenda aceite
     */
    public void addEncomendaAceite(String e) {
        this.encomendasAceites.add(e);
    }

    /**
     * Adiciona uma encomenda a um cliente
     */
    public void addEncomendaACliente() {

        for (IEncomenda e : this.encomendas.values()) {
            IUser user = this.utilizadores.get(e.getComprador());

            user.addEncomenda(e);
        }
    }

    /**
     * Adiciona produtos a uma loja
     */
    public void addProdutosALoja() {

        for (IEncomenda e : this.encomendas.values()) {
            ILoja loja = this.lojas.get(e.getVendedor());

            loja.setProdutos(e.getProdutos());
        }
    }

    /**
     * Adiciona uma encomenda aceite
     * @param e encomenda
     */
    public void aceitaEnc(String e) {
        this.encomendasAceites.add(e);
    }

    /**
     * Remove encomenda aceite
     * @param e encomenda
     */
    public void removeEnc(String e) {
        this.encomendasAceites.remove(e);
    }

    /**
     * @return representa um sistema em string
     */
    public String toString() {
        StringBuilder s = new StringBuilder();

        s.append("Empresas transportadoras:\n").append(this.transportadoras);
        s.append("\nVoluntários:\n").append(this.voluntarios);
        s.append("\nLojas:\n").append(this.lojas);
        s.append("\nUtilizadores:\n").append(this.utilizadores);
        s.append("\nEncomendas no sistema:\n").append(this.encomendas);
        s.append("\nEncomendas aceites:\n").append(this.encomendasAceites);
        s.append("\n\n");

        return s.toString();
    }

    /**
     * @return clone de um sistema
     */
    public Sistema clone() {
        return new Sistema(this);
    }

    /**
     * Devolve os 10 utilizadores que mais usaram o sistema
     * @return 10 utilizadores que mais utilizaram o sistema
     */
    public List<AbstractMap.SimpleEntry<IUser, Integer>> top10Users(){
        Map<IUser,Integer> mapa = new HashMap<>();

        for(IUser u : this.utilizadores.values())
            mapa.put(u.clone(),u.getEncomendas().size());

        return mapa.entrySet().stream().map(e -> new AbstractMap.SimpleEntry<>(e.getKey().clone(), e.getValue()))
                                        .sorted(new ComparatorQuantidadeEncomendas())
                                        .limit(10)
                                        .collect(Collectors.toList());
    }

    /**
     * Devolve as 10 empresas que mais usaram o sistema
     * @return 10 empresas que mais utilizaram o sistema
     */
    public List<AbstractMap.SimpleEntry<IEmpresa, Double>> top10Empresas(){
        Map<IEmpresa,Double> mapa = new HashMap<>();

        for(IEmpresa e : this.transportadoras.values()){
            double kms = 0.0;
            for(IEncomenda enc : e.getEncomendas().values()) {
                ILoja  l = this.lojas.get(enc.getVendedor());
                IUser u = this.utilizadores.get(enc.getComprador());
                kms += e.distancia(l) + l.distancia(u);
            }
            mapa.put(e.clone(),kms);
        }
        return mapa.entrySet().stream().map(e -> new AbstractMap.SimpleEntry<>(e.getKey().clone(), e.getValue()))
                                        .sorted(new ComparatorQuantidadeKms())
                                        .limit(10)
                                        .collect(Collectors.toList());
    }

    /**
     * Grava os dados para um ficheiro
     */
    public void gravarDados() throws IOException {
        ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream("./dados/sistema.obj"));

        o.writeObject(this);
        o.flush();
        o.close();
    }

    /**
     * Lê os dados de um sistema
     * @return sistema
     */
    public ISistema lerDados() throws IOException, ClassNotFoundException {
        ObjectInputStream o = new ObjectInputStream(new FileInputStream("./dados/sistema.obj"));
        ISistema t = (ISistema) o.readObject();
        o.close();

        return t;
    }
}
