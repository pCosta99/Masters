package Model;

import Helpers.Pair;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

public class Transportadora implements Observable, SystemUser, Serializable, Comparable<Transportadora> {

    private String codEmpresa;
    private String nomeEmpresa;
    private GPS gps;
    private double raio;
    private String nif;
    private double precoPorKm;
    private double taxaPeso;
    private int nEncomendas;
    private List<Integer> classificacoes;
    private List<RegistoTempoCustoEncomenda> registosEnc;
    private List<Observer> observers;
    private boolean aceitaTransporteMedico;
    private double velocidade;

    private List<Pair<Encomenda,Double>> encomendasPorAceitar;

    private String loginID;
    private String loginPass;

    private int STATUS;


    public Transportadora(String codEmpresa, String nomeEmpresa, GPS gps ,double raio,String nif,
                          double precoPorKm) {
        this.codEmpresa=codEmpresa;
        this.nomeEmpresa=nomeEmpresa;
        this.gps=gps;
        this.raio=raio;
        this.nif=nif;
        this.precoPorKm=precoPorKm;

        this.nEncomendas=1;
        this.velocidade = 180;
        this.precoPorKm = 1;
        this.taxaPeso = 1;

        this.classificacoes =new ArrayList<>();
        this.registosEnc =new ArrayList<>();
        this.aceitaTransporteMedico=false;

        this.loginID = this.codEmpresa;
        this.loginPass = this.codEmpresa;

        this.encomendasPorAceitar = new ArrayList<>();
        this.STATUS = 0;
    }

    public Transportadora () {
        this.codEmpresa="";
        this.nomeEmpresa="";
        this.gps= new GPS(0,0);
        this.raio=0;
        this.nif="";
        this.precoPorKm=0;
        this.taxaPeso=0;
        this.nEncomendas=1;
        this.observers=new ArrayList<>();
        this.classificacoes= new ArrayList<>();
        this.registosEnc= new ArrayList<>();
        this.aceitaTransporteMedico=false;
        this.velocidade=0;
    }

    public Transportadora (String codEmpresa, String nomeEmpresa, GPS gps ,double raio,String nif,
                           double precoPorKm,double taxaPeso,double velocidade,String loginID, String loginPass) {
        this.codEmpresa=codEmpresa;
        this.nomeEmpresa=nomeEmpresa;
        this.gps=gps;
        this.raio=raio;
        this.taxaPeso=taxaPeso;
        this.nif=nif;
        this.precoPorKm=precoPorKm;
        this.observers=new ArrayList<>();
        this.velocidade=velocidade;
        this.nEncomendas=1;
        this.classificacoes =new ArrayList<>();
        this.classificacoes= new ArrayList<>();
        this.registosEnc =new ArrayList<>();
        this.registosEnc= new ArrayList<>();
        this.aceitaTransporteMedico=false;
        this.loginID = loginID;
        this.loginPass = loginPass;
    }

    public Transportadora (String codEmpresa, String nomeEmpresa, GPS gps ,float raio,String nif,
                           float precoPorKm, List<Integer> classificacoes,List<Observer> observers,
                           List<RegistoTempoCustoEncomenda> LinhaEncomenda,double velocidade,boolean aceitaTransporteMedico,double taxaPeso,int nEncomendas) {
        this.codEmpresa=codEmpresa;
        this.nomeEmpresa=nomeEmpresa;
        this.gps=gps;
        this.raio=raio;
        this.taxaPeso=taxaPeso;
        this.nif=nif;
        this.precoPorKm=precoPorKm;
        this.observers=observers;
        this.velocidade=velocidade;
        this.nEncomendas=nEncomendas;
        this.classificacoes =new ArrayList<>();
        this.classificacoes.addAll(classificacoes);
        this.registosEnc =new ArrayList<>();
        this.registosEnc.addAll(LinhaEncomenda);
        this.aceitaTransporteMedico=aceitaTransporteMedico;
    }

    public Transportadora (Transportadora t) {
        this.codEmpresa=t.getCodEmpresa();
        this.nomeEmpresa=t.getNomeEmpresa();
        this.gps=t.getGps();
        this.raio=t.getRaio();
        this.nif=t.getNif();
        this.precoPorKm=t.getPrecoPorKm();
        this.nEncomendas=t.getnEncomendas();
        this.classificacoes=t.getClassificacoes();
        this.registosEnc=t.getRegistosEnc();
        this.aceitaTransporteMedico=t.isAceitaTransporteMedico();
        this.observers=t.getObservers();
        this.taxaPeso=t.getTaxaPeso();
        this.velocidade=t.getVelocidade();
        this.encomendasPorAceitar = t.getEncomendasPorAceitar();

        this.loginID = t.getLoginID();
        this.loginPass = t.getLoginPass();
        this.STATUS = t.getSTATUS();
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

    public double timeToTransport(Encomenda enc) {
        GPS gpsLoja = enc.getLojaGPS();
        GPS gpsUser = enc.getUserGPS();
        double distToStore = dist(this.gps,gpsLoja);
        double distStoreToCli = dist(gpsLoja,gpsUser);
        double totalDist = distToStore + distStoreToCli;
        return totalDist / this.velocidade;

    }

    private double dist(GPS gps1, GPS gps2) {
        return Math.sqrt(Math.pow(gps1.getX() - gps2.getX(), 2) + Math.pow(gps1.getY() - gps2.getY(), 2));
    }

    public double addEncomenda(Encomenda enc) {
        double custo = calculoCustoEncomenda(enc);
        this.encomendasPorAceitar.add(new Pair<>(enc,custo));
        return custo;
    }

    public void removeEncomenda() {
        this.encomendasPorAceitar.remove(0);
    }

    public int getSTATUS() {
        return STATUS;
    }

    public void setSTATUS(int STATUS) {
        this.STATUS = STATUS;
    }

    public List<Pair<Encomenda, Double>> getEncomendasPorAceitar() {
        return encomendasPorAceitar;
    }

    public void setEncomendasPorAceitar(List<Pair<Encomenda, Double>> encomendasPorAceitar) {
        this.encomendasPorAceitar = encomendasPorAceitar;
    }

    public double getVelocidade() {
        return velocidade;
    }

    public void setVelocidade(double velocidade) {
        this.velocidade = velocidade;
    }

    public double getTaxaPeso() {
        return taxaPeso;
    }

    public void setTaxaPeso(double taxaPeso) {
        this.taxaPeso = taxaPeso;
    }

    public int getnEncomendas() {
        return nEncomendas;
    }

    public void setnEncomendas(int nEncomendas) {
        this.nEncomendas = nEncomendas;
    }

    public String getCodEmpresa() {
        return codEmpresa;
    }

    public void setCodEmpresa(String codEmpresa) {
        this.codEmpresa = codEmpresa;
    }

    public String getNomeEmpresa() {
        return nomeEmpresa;
    }

    public void setNomeEmpresa(String nomeEmpresa) {
        this.nomeEmpresa = nomeEmpresa;
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

    public String getNif() {
        return nif;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public double getPrecoPorKm() {
        return precoPorKm;
    }

    public void setPrecoPorKm(float precoPorKm) {
        this.precoPorKm = precoPorKm;
    }


    public List<Integer> getClassificacoes() {
        return classificacoes;
    }

    public void setClassificacoes(List<Integer> classificacoes) {
        this.classificacoes = classificacoes;
    }

    public List<RegistoTempoCustoEncomenda> getRegistosEnc() {
        return registosEnc;
    }

    public void setRegistosEnc(List<RegistoTempoCustoEncomenda> registosEnc) {
        this.registosEnc = registosEnc;
    }

    public boolean isAceitaTransporteMedico() {
        return aceitaTransporteMedico;
    }

    public void setAceitaTransporteMedico(boolean aceitaTransporteMedico) {
        this.aceitaTransporteMedico = aceitaTransporteMedico;
    }

    public boolean aceitoTransporteMedicamentos(){return this.aceitaTransporteMedico;}
    public void aceitaMedicamentos(boolean state){this.aceitaTransporteMedico = state;}

    private void realizarTransporte(){}
    public int pedirParaEntregar(Encomenda enc) {return 0;}


    @Override
    public String toString() {
        return "Transportadora{" +
                "codEmpresa='" + codEmpresa + '\'' +
                ", nomeEmpresa='" + nomeEmpresa + '\'' +
                ", gps=" + gps +
                ", raio=" + raio +
                ", nif='" + nif + '\'' +
                ", precoPorKm=" + precoPorKm +
                ", taxaPeso=" + taxaPeso +
                ", nEncomendas=" + nEncomendas +
                ", classificacoes=" + classificacoes +
                ", registosEnc=" + registosEnc +
                ", aceitaTransporteMedico=" + aceitaTransporteMedico +
                ", velocidade=" + velocidade +
                ", encomendasPorAceitar=" + encomendasPorAceitar +
                ", loginID='" + loginID + '\'' +
                ", loginPass='" + loginPass + '\'' +
                ", STATUS=" + STATUS +
                '}' + "\n";
    }

    public Transportadora clone () {
        return new Transportadora(this);
    }

    public double calculoDistancia (Encomenda a) {
        return (Math.sqrt(Math.pow(this.gps.x - a.getLojaGPS().getX(), 2) + Math.pow(this.gps.y - a.getLojaGPS().getY(), 2)))+
                (Math.sqrt(Math.pow(a.getLojaGPS().getX() - a.getUserGPS().getX(), 2) + Math.pow(a.getLojaGPS().getY() - a.getUserGPS().getY(), 2)));
    }

    public double calculoCustoEncomenda (Encomenda a) {
        return this.calculoDistancia(a)*this.precoPorKm*this.taxaPeso;
    }

    @Override
    public List<Observer> getObservers() {
        return observers;
    }
//empresas que aceitam mais de uma encomenda
    public boolean aceitaEncomendas (List<Encomenda> a) {
        return nEncomendas > a.size();
    }

    public void addObserver(Observer e) {
        this.observers.add(e);

    }
    public void notifyObserver(Observer obs, String label, List<Object> data) {
        obs.update(this,label,data);
    }

    //funcao igual a do ze
    List<Encomenda> dentroRaio(List<Encomenda> encomendas) {
        return encomendas.stream().
                filter(e -> (Math.sqrt(Math.pow(this.gps.getX() - e.getLojaGPS().getX(), 2)) + Math.pow(this.gps.getY() - e.getLojaGPS().getY(), 2)) < this.raio).
                collect(Collectors.toList());
    }

    @Override
    public boolean checkPassword(String pass) {
        System.out.println(this);
        return this.loginPass.equals(pass);
    }

    @Override
    public int compareTo(Transportadora o) {
        return Integer.compare(Integer.parseInt(this.codEmpresa.substring(1)),Integer.parseInt(o.getCodEmpresa().substring(1)));
    }

/*

    //adicionar um registo de encomenda ,o tempo nao sei como por
    public void addRegistoEnc (Encomenda a) {
        RegistoTempoCustoEncomenda aux = new RegistoTempoCustoEncomenda(a.getCodEncomenda(),
                this.calculoCustoEncomenda(a),this.calculoDistancia(a)/velocidade); //tempo provisorio
        this.getRegistosEnc().add(aux);
    }

//forma de selecao da encomenda
    public Encomenda getEncomendaMaisRentavel (List<Encomenda> enc) {
        int i = 0;
        double j = 0;
        Encomenda aux = new Encomenda();
        for (Encomenda a : enc) {
            for (i = 0; i < a.getLinhasEncomenda().size(); i++) {
                if (a.getLinhasEncomenda().get(i).getValorUnitario()+calculoCustoEncomenda(a) > j) aux = a;
            }

        }
        return aux;
    }






//custo da encomenda baseado na distancia e no peso



 */



}
