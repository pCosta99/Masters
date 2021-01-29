package model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

//FALTAM ALGUNS METODOS

public class Voluntario extends User implements Serializable {

    private Coordenadas coor; //posicao exata
    private int raio; //raio de geográfico no qual se movimenta em kms
    private boolean special; //estatuto especial para encomendas medicas
    private boolean status; //estado: ocupado ou nao MAIS PARA FRENTE PARA O VOLUNTARIO MUDAR O SEU ESTADO
    private List<Viagem> viagens; //Registo de viagens efetuadas
    private int totalKms; //total de viagens
    private double tempoMedio; //tempo medio que este voluntario faz por km
    private double velocidadeM;
    private List<Encomenda> pendentes;
    private List<Encomenda> historico;


    public Voluntario() {
        super();
        this.coor = new Coordenadas();
        this.raio = 0;
        this.special = false;
        this.status = false;
        this.viagens = new ArrayList<Viagem>();
        this.totalKms = 0;
        this.tempoMedio = 0.0;
        this.velocidadeM = 0.0;
        this.historico = new ArrayList<>();
        this.pendentes = new ArrayList<>();
    }

    public Voluntario(String id, String nome, String email, String password, String morada,
                      Coordenadas c, int raio, boolean special, boolean status, List<Viagem> v, int totalKms, double tempoMedio,
                      double velocidadeM,List<Encomenda> reg ,List<Encomenda> espera)
    {
        super(id, nome, email, password, morada);
        setCoor(c);
        this.raio = raio;
        this.special = special;
        this.status = status;
        this.tempoMedio = tempoMedio;
        this.totalKms = totalKms;
        setViagens(v);
        this.velocidadeM = velocidadeM;
        this.historico = reg;
        this.pendentes = espera;
    }


    public Voluntario(Voluntario v) {
        super(v);
        setCoor(v.getCoor());
        setRaio(v.getRaio());
        setSpecial(v.isSpecial());
        setStatus(v.isStatus());
        setTempoMedio(v.getTempoMedio());
        setTotalKms(v.getTotalKms());
        setViagens(v.getViagens());
        setVelocidadeM(v.getVelocidadeM());
    }


    public Coordenadas getCoor() {
        return this.coor;
    }

    public void setCoor(Coordenadas coor) {
        this.coor = coor;
    }

    public int getRaio() {
        return this.raio;
    }

    public void setRaio(int raio) {
        this.raio = raio;
    }

    public boolean isSpecial() {
        return this.special;
    }

    public void setSpecial(boolean special) {
        this.special = special;
    }

    public boolean isStatus() {
        return this.status;
    }

    public void setStatus(boolean status) { this.status = status;}

    public double getTempoMedio() {
        return this.tempoMedio;
    }

    public void setTempoMedio(double tempoMedio) {
        this.tempoMedio = tempoMedio;
    }

    public int getTotalKms() {
        int total = 0;

        for (Viagem v : this.viagens) {
            total += v.distanciaPercorrida(v.getInicio(), v.getFim());
        }
        return total;
    }

    public void setTotalKms(int totalKms) {
        this.totalKms = totalKms;
    }

    public ArrayList<Viagem> getViagens() {
        ArrayList<Viagem> res = new ArrayList<Viagem>();

        for (Viagem v : this.viagens)
            res.add(v.clone());

        return res;
    }

    public void setViagens(List<Viagem> list) {
        this.viagens = new ArrayList<Viagem>();

        for (Viagem v : list)
            this.viagens.add(v.clone());
    }


    public double getVelocidadeM() {
        return velocidadeM;
    }

    public void setVelocidadeM(double velocidadeM) {
        this.velocidadeM = velocidadeM;
    }

    public Voluntario clone() {
        return new Voluntario(this);
    }


    public List<Encomenda> getRegistoEncomendasPending() {
        List<Encomenda> res = new ArrayList<Encomenda>();

        for (Encomenda e : this.pendentes)
            res.add(e.clone());

        return res;
    }

    public void setRegistoEncomendasPendentes(List<Encomenda> r) {
        this.pendentes = new ArrayList<Encomenda>();

        for (Encomenda e : r)
            this.pendentes.add(e.clone());

    }




    public List<Encomenda> getRegistoEncomendasHistorico() {
        List<Encomenda> res = new ArrayList<Encomenda>();

        for (Encomenda e : this.historico)
            res.add(e.clone());

        return res;
    }


    public void setRegistoEncomendasHistorico(List<Encomenda> r) {
        this.historico = new ArrayList<Encomenda>();

        for (Encomenda e : r)
            this.historico.add(e.clone());
    }


    public void recusaEncomenda(Encomenda r) {
        this.getRegistoEncomendasPending().remove(r);
    }

    public void aceitaEncomenda(Encomenda r) {
        recusaEncomenda(r);
        this.getRegistoEncomendasHistorico().add(r);
        Viagem nova = new Viagem(this.getCoor(),r.getDestino(),r.getIdEncomenda());
        this.viagens.add(nova);
    }




    public void addEncomendaPending(Encomenda e){
        this.pendentes.add(e);
    }

    public void removeEncomendaPending(Encomenda encomenda){this.pendentes.remove(encomenda);}

    public void addEncomendaRegisto(Encomenda e){
        this.historico.add(e);
    }

    public void removeEncomendaRegisto(Encomenda e){
        this.historico.remove(e);
    }


    public Viagem getViagemdeEncomenda(Encomenda e) throws Exception {

        for (Viagem v : this.viagens) {
            if (v.getIdEnc().equals(e.getIdEncomenda())) return v;
        }

         throw new Exception("Viagem de Encomenda nao encontrada");
    }

    public double tempoDemoradoEncomenda(Encomenda e) throws Exception{
        if (this.getRegistoEncomendasHistorico().contains(e)) {
            return this.tempoMedio * this.getViagemdeEncomenda(e).distanciaPercorrida(getViagemdeEncomenda(e).getInicio(), getViagemdeEncomenda(e).getFim());

        } else throw new Exception("Viagem de Encomenda nao encontrada");

    }

    public List<Encomenda> registoEncomendaemPeriodo(LocalDateTime inicio, LocalDateTime fim){
        return this.getRegistoEncomendasHistorico().stream().filter(encomenda -> encomenda.getData().isEqual(inicio) && encomenda.getData().isAfter(inicio)
                && encomenda.getData().isEqual(fim) && encomenda.getData().isBefore(fim)).collect(Collectors.toList());
    }



    /*
    Funções que alteram o estado para  ocupado - false ||  desocupado-true
     */

    public void ocupado(){
        this.status = false;
    }

    public void desocupado(){
        this.status=true;
    }


    @Override
    public String toString() {
        return new StringBuilder()
        .append(this.getNome()).append("\n")
        .append(this.getId()).append("\n")
        .append(this.getEmail()).append("\n")
        .append(this.getMorada()).append("\n")
        .append(this.getCoor()).append("\n")
        .append(this.getRaio()).append("\n")
        .append(this.isStatus()).append("\n")
        .append(this.getTempoMedio()).append("\n")
        .append(this.getVelocidadeM()).append("\n").toString();


    }

    public int numeroEncomendas(){
        return this.historico.size();
    }


}
