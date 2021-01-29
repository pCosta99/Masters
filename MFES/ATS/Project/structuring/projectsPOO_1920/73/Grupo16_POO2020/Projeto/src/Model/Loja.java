package Model;

import Helpers.Triplet;

import java.io.Serializable;
import java.util.*;

public class Loja implements Observable, Comparable<Loja>, SystemUser, Serializable {

    private String codLoja;
    private String nomeLoja;
    private double tempoHabitual;
    private GPS gps;
    private List<Encomenda> encsEmEspera;
    private List<Observer> observers;

    private String loginID;
    private String loginPass;

    private Map<String, Triplet<String,Double,Double>> produtos;


    public void registarProd(Produto prod, double quantidade, double preco) {
        produtos.put(prod.getCodProduto(),new Triplet<>(prod.getDescricao(),quantidade,preco));
    }


    public Loja(String codLoja, String nomeLoja, GPS gps) {
        this.codLoja = codLoja;
        this.nomeLoja = nomeLoja;
        this.gps = gps;
        this.encsEmEspera = new ArrayList<>();
        this.observers = new ArrayList<>();
        this.produtos = new LinkedHashMap<>();
        this.tempoHabitual = 0;
        this.loginID = codLoja;
        this.loginPass = codLoja;
    }

    public Loja(String codLoja, String nomeLoja, GPS gps, double tempo, List<Observer> obs, String loginID, String loginPass) {
        this.codLoja = codLoja;
        this.nomeLoja = nomeLoja;
        this.gps = gps;
        this.encsEmEspera = new ArrayList<>();
        this.observers = new ArrayList<>(obs);
        this.produtos = new LinkedHashMap<>();
        this.tempoHabitual = tempo;
        this.loginID = loginID;
        this.loginPass = loginPass;
    }

    public Loja(Loja umaLoja){
        this.codLoja=umaLoja.getCodLoja();
        this.nomeLoja=umaLoja.getNomeLoja();
        this.gps=umaLoja.getGps();
        this.encsEmEspera = umaLoja.getEncsEmEspera();
        this.produtos = umaLoja.getProdutos();
        this.observers=umaLoja.getObservers();
        this.loginID = umaLoja.getLoginID();
        this.loginPass = umaLoja.getLoginPass();
    }

    public String getLoginID() {
        return loginID;
    }

    public void setLoginID(String loginID) {
        this.loginID = loginID;
    }

    public String getLoginPass() {
        return loginPass;
    }

    public void setLoginPass(String loginPass) {
        this.loginPass = loginPass;
    }

    public void addEncEmEspera(Encomenda enc) {
        this.encsEmEspera.add(enc);
        this.removeFromStock(enc);

    }

    private Encomenda getEncEmEspera(String cod) {
        return this.encsEmEspera.stream().filter(enc -> enc.getCodEncomenda().equals(cod)).findFirst().orElse(null);
    }

    public void aceitarEncomeda(Encomenda enc) {
        this.encsEmEspera.remove(getEncEmEspera(enc.getCodEncomenda()));
        this.notifyObserver(this.observers.get(0),"EncomendaAceite", Arrays.asList(enc.getCodEncomenda()));
    }

    private void removeFromStock(Encomenda enc) {
        for(LinhaEncomenda l : enc.getLinhasEncomenda()) {
            double previousQuant = this.produtos.get(l.getProduto().getCodProduto()).getSecond();
            this.produtos.get(l.getProduto().getCodProduto()).setSecond(previousQuant-l.getQuantidade());
        }
    }


    public Map<String, Triplet<String, Double, Double>> getProdutos() {
        return produtos;
    }

    public List<Observer> getObservers() {
        return observers;
    }

    public void addObserver(Observer e) {
        this.observers.add(e);
    }

    public  void notifyObserver(Observer obs, String label, List<Object> data) {
        obs.update(this,label,data);
    }



    public double getTempoHabitual() {
        return tempoHabitual;
    }

    public void setTempoHabitual(double tempoHabitual) {
        this.tempoHabitual = tempoHabitual;
    }

    public String getCodLoja() {
        return codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public String getNomeLoja() {
        return nomeLoja;
    }

    public void setNomeLoja(String nomeLoja) {
        this.nomeLoja = nomeLoja;
    }

    public GPS getGps() {
        return gps;
    }

    public void setGps(GPS gps) {
        this.gps = gps;
    }

    public List<Encomenda> getEncsEmEspera() {
        return encsEmEspera;
    }

    public void setEncsEmEspera(List<Encomenda> encsEmEspera) {
        this.encsEmEspera = encsEmEspera;
    }

    public void encomendaPronta(Encomenda enc){
        List<Object> aux=new ArrayList<>();
        aux.add(enc);
        this.notifyObserver(this.observers.get(0),"EncPronta",aux);
    }

    @Override
    public String toString() {
        return "Loja{" +
                "codLoja='" + codLoja + '\'' +
                ", nomeLoja='" + nomeLoja + '\'' +
                ", tempoHabitual=" + tempoHabitual +
                ", gps=" + gps +
                ", encsEmEspera=" + encsEmEspera +
                ", produtos=" + produtos +
                '}';
    }



    @Override
    public boolean checkPassword(String pass) {
        return this.loginPass.equals(pass);
    }

    @Override
    public int compareTo(Loja o) {
        return Integer.compare(Integer.parseInt(this.codLoja.substring(1)),Integer.parseInt(o.getCodLoja().substring(1)));
    }

/*
    public double calculaTempo (Object c,Encomenda encomenda) {
        if (c instanceof Transportadora) {
            return ((Transportadora) c).calculoDistancia(encomenda) / ((Transportadora) c).getVelocidade() + tempoHabitual;
        }
           else {
            return 0;
            //preciso codigo dos Voluntario;
           }
    }

     */

    //como tratar o tempo
    // coisas dos gps
    //sinalizaçao


}
