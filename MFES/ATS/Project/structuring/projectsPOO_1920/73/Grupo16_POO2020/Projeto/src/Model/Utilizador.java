package Model;

import Helpers.Pair;
import View.Panel;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class Utilizador implements Observable, SystemUser, Comparable<Utilizador>, Serializable {

    private String codUtilizador;
    private String nome;
    private GPS gps;
    private List<Observer> observers;
    private String loginID;
    private String loginPass;

    private Pair<Encomenda,Pair<String,Double>> notification;



    public Utilizador(String codUtilizador, String nome, GPS gps){
        this.codUtilizador=codUtilizador;
        this.nome=nome;
        this.gps=gps;
        this.observers=new ArrayList<>();
        this.loginID = codUtilizador;
        this.loginPass = codUtilizador;
        this.notification = null;

    }

    public Utilizador(){
        this.codUtilizador="";
        this.nome="";
        this.gps= new GPS();
        this.observers=new ArrayList<>();
        this.notification = null;
    }

    public Utilizador(String codUtilizador, String nome, GPS gps, List<Observer> observers){
        this.codUtilizador=codUtilizador;
        this.nome=nome;
        this.gps=gps;
        this.observers=observers;
        this.notification = null;
    }

    public Utilizador(String codUtilizador, String nome, GPS gps, List<Observer> observers, String loginID, String loginPass) {
        this.codUtilizador = codUtilizador;
        this.nome = nome;
        this.gps = gps;
        this.observers = observers;
        this.loginID = loginID;
        this.loginPass = loginPass;
        this.notification = null;
    }

    public Utilizador(Utilizador umUtilizador){
        this.codUtilizador=umUtilizador.getCodUtilizador();
        this.nome=umUtilizador.getNome();
        this.gps=umUtilizador.getGps();
        this.observers=umUtilizador.getObservers();
        this.loginID = umUtilizador.getLoginID();
        this.loginPass = umUtilizador.getLoginPass();
        this.notification = umUtilizador.getNotification();
    }

    public void setNotif(Pair<Encomenda,Pair<String,Double>> notif) {
        this.notification = notif;
    }

    public void acceptTransp() {
        this.notifyObserver(observers.get(0),"AceitarTransp", Arrays.asList(this.notification));
        this.notification = null;
    }

    public Pair<Encomenda,Pair<String,Double>> getNotification() {
        return notification;
    }

    public void setNotification(Pair<Encomenda,Pair<String,Double>> notification) {
        this.notification = notification;
    }

    public String getCodUtilizador() {
        return codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public GPS getGps() {
        return gps;
    }

    public void setGps(GPS gps) {
        this.gps = gps;
    }

    public void setObservers(List<Observer> observers) {
        this.observers = observers;
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

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador that = (Utilizador) o;
        return Objects.equals(codUtilizador, that.codUtilizador) &&
                Objects.equals(nome, that.nome) &&
                Objects.equals(gps, that.gps);
    }

    @Override
    public String toString() {
        return "Utilizador{" +
                "codUtilizador='" + codUtilizador + '\'' +
                ", nome='" + nome + '\'' +
                ", gps=" + gps +
                ", loginID='" + loginID + '\'' +
                ", loginPass='" + loginPass + '\'' +
                '}';
    }

    public void realizarPedido(Encomenda enc){
    }
    public void aceitarServico(String cod, Encomenda enc){}
    public void negarServico(String cod, Encomenda enc){}

    public void classificarVoluntario(Voluntario voluntario, int classificacao){
        voluntario.getClassificacoes().add(classificacao);
    }
    public void classificarTransportadora(Transportadora transportadora, int classificacao){
        transportadora.getClassificacoes().add(classificacao);
    }

    @Override
    public List<Observer> getObservers() {
        return observers;
    }

    @Override
    public void addObserver(Observer e) {
        this.observers.add(e);
    }

    @Override
    public void notifyObserver(Observer obs, String label, List<Object> data) {
        obs.update(this,label,data);
    }

    @Override
    public boolean checkPassword(String pass) {
        return this.loginPass.equals(pass);
    }

    @Override
    public int compareTo(Utilizador o) {
        return Integer.compare(Integer.parseInt(this.codUtilizador.substring(1)),Integer.parseInt(o.getCodUtilizador().substring(1)));
    }
}
