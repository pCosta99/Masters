package Model;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

public class Voluntario implements Observable, SystemUser, Serializable, Comparable<Voluntario> {

    private String codVoluntario;
    private String nome;
    private GPS gps;
    private double raio;
    private List<Integer> classificacoes;
    private List<RegistoEnc> registosEnc;
    private boolean aceitaTransporteMedico;
    private List<Observer> observers;

    private double velPerKm;

    private String loginID;
    private String loginPass;

    // 0 - livre, 1 - ocupado
    private int STATUS;



    public Voluntario(String codVoluntario, String nome, GPS gps, double raio) {
        this.codVoluntario = codVoluntario;
        this.nome = nome;
        this.gps = gps;
        this.raio = raio;
        this.classificacoes = new ArrayList<>();
        this.registosEnc = new ArrayList<>();
        this.aceitaTransporteMedico = false;
        this.velPerKm = randomVel();

        this.loginID = codVoluntario;
        this.loginPass = codVoluntario;


        this.STATUS = 0;
    }

    public Voluntario(String codVoluntario, String nome, GPS gps, double raio, double velPerKm, String loginID, String loginPass) {
        this.codVoluntario = codVoluntario;
        this.nome = nome;
        this.gps = gps;
        this.raio = raio;
        this.classificacoes = new ArrayList<>();
        this.registosEnc = new ArrayList<>();
        this.aceitaTransporteMedico = false;
        this.velPerKm = velPerKm;

        this.loginID = loginID;
        this.loginPass = loginPass;


        this.STATUS = 0;
    }

    private double randomVel() {
        double min = 15;
        double max = 25;
        Random random = new Random();
        return min + (max - min) * random.nextDouble();
    }

    public Voluntario(){
        this.codVoluntario = "";
        this.nome = "";
        this.gps = new GPS();
        this.raio = 0;
        this.classificacoes = new ArrayList<>();
        this.registosEnc = new ArrayList<>();
        this.aceitaTransporteMedico = false;
        this.observers = new ArrayList<>();
    }

    public Voluntario(String codVoluntario, String nome, GPS gps, double raio, List<Integer> classificacoes, List<RegistoEnc> registosEnc, boolean aceitaTransporteMedico, List<Observer> observers) {
        this.codVoluntario = codVoluntario;
        this.nome = nome;
        this.gps = gps;
        this.raio = raio;
        this.classificacoes = classificacoes;
        this.registosEnc = registosEnc;
        this.aceitaTransporteMedico = aceitaTransporteMedico;
        this.observers=observers;
    }

    public Voluntario(Voluntario umVol) {
        this.codVoluntario = umVol.getCodVoluntario();
        this.nome = umVol.getNome();
        this.gps = umVol.getGps();
        this.raio = umVol.getRaio();
        this.classificacoes = umVol.getClassificacoes();
        this.registosEnc = umVol.getRegistosEnc();
        this.aceitaTransporteMedico = umVol.isAceitaTransporteMedico();

        this.loginID = umVol.getLoginID();
        this.loginPass = umVol.getLoginPass();


        this.STATUS = umVol.getSTATUS();
    }

    //FIXME - tempo de espera
    public double timeToTransport(Encomenda enc) {
        GPS gpsLoja = enc.getLojaGPS();
        GPS gpsUser = enc.getUserGPS();
        double distToStore = dist(this.gps,gpsLoja);
        double distStoreToCli = dist(gpsLoja,gpsUser);
        double totalDist = distToStore + distStoreToCli;
        return totalDist / this.velPerKm;

    }

    private double dist(GPS gps1, GPS gps2) {
        return Math.sqrt(Math.pow(gps1.getX() - gps2.getX(), 2) + Math.pow(gps1.getY() - gps2.getY(), 2));
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

    public int getSTATUS() {
        return STATUS;
    }

    public void setSTATUS(int STATUS) {
        this.STATUS = STATUS;
    }

    public String getCodVoluntario() {
        return codVoluntario;
    }

    public void setCodVoluntario(String codVoluntario) {
        this.codVoluntario = codVoluntario;
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

    public double getRaio() {
        return raio;
    }

    public void setRaio(float raio) {
        this.raio = raio;
    }

    public List<Integer> getClassificacoes() {
        return classificacoes;
    }

    public void setClassificacoes(List<Integer> classificacoes) {
        this.classificacoes = classificacoes;
    }

    public List<RegistoEnc> getRegistosEnc() {
        return registosEnc;
    }

    public void setRegistosEnc(List<RegistoEnc> registosEnc) {
        this.registosEnc = registosEnc;
    }

    public boolean isAceitaTransporteMedico() {
        return aceitaTransporteMedico;
    }

    public void setAceitaTransporteMedico(boolean aceitaTransporteMedico) {
        this.aceitaTransporteMedico = aceitaTransporteMedico;
    }



    public List<Encomenda> dentroRaio(List<Encomenda> encomendas) {
        return encomendas.stream().
                filter(e -> (Math.sqrt(Math.pow(this.gps.x - e.getLojaGPS().getX(), 2)) + Math.pow(this.gps.y - e.getLojaGPS().getY(), 2)) <= this.raio).
                collect(Collectors.toList());
    }

    public Encomenda maisProxima(List<Encomenda> encomendas) {
        Encomenda res = encomendas.get(0);
        for (Encomenda e : encomendas)
            if ((Math.sqrt(Math.pow(this.gps.x - e.getLojaGPS().getX(), 2)) + Math.pow(this.gps.y - e.getLojaGPS().getY(), 2)) <=
                    (Math.sqrt(Math.pow(this.gps.x - res.getLojaGPS().getX(), 2)) + Math.pow(this.gps.y - res.getLojaGPS().getY(), 2))) {
                res = e;
            }
        return res;
    }

    public int mediaClassificacao(){
        int sum = classificacoes.stream().mapToInt(i -> i).sum();
        return sum/classificacoes.size();
    }

    public boolean aceitoTransporteMedicamentos () {
        return this.aceitaTransporteMedico;
    }
    public void aceitaMedicamentos ( boolean state){
        this.aceitaTransporteMedico = state;
    }
    private void realizarTransporte () { }
    public int pedirParaEntregar (Encomenda enc){
        return 0;
        }

    @Override
    public String toString() {
        return "Voluntario{" +
                "codVoluntario='" + codVoluntario + '\'' +
                ", nome='" + nome + '\'' +
                ", gps=" + gps +
                ", raio=" + raio +
                ", classificacoes=" + classificacoes +
                ", registosEnc=" + registosEnc +
                ", aceitaTransporteMedico=" + aceitaTransporteMedico +
                ", velPerKm=" + velPerKm +
                ", loginID='" + loginID + '\'' +
                ", loginPass='" + loginPass + '\'' +
                ", STATUS=" + STATUS +
                '}';
    }

    @Override
    public List<Observer> getObservers() {
        return observers;
    }

    @Override
    public void addObserver(Observer o) {
        this.observers.add(o);
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
    public int compareTo(Voluntario o) {
        return Integer.compare(Integer.parseInt(this.getCodVoluntario().substring(1)),Integer.parseInt(o.getCodVoluntario().substring(1)));
    }
}
