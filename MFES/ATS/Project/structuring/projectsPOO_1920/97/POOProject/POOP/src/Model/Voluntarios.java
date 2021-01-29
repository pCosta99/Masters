package Model;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import static java.lang.Math.*;
import java.io.Serializable;

/**
 * Classe que representa os voluntários 
 */

public class Voluntarios implements Serializable{
    private static final long serialVersionUID = 5583135800979722684L;
    private String password;
    private String codVoluntario;
    private String nome;
    private double latitude;
    private double longitude;
    private double raio;
    private int classificacoes; // 0 a 5
    private int nrClassif;
    private double tempoMedporKm;

    public boolean livre;
    private boolean transportaMed;
    private List<Encomendas> enc;


    private Map<Encomendas, Double> regTempo;


    public Voluntarios(){
        this.password = new String();
        this.codVoluntario = new String();
        this.nome = new String();
        this.latitude = 0.0;
        this.longitude = 0.0;
        this.raio = 0.0;
        this.livre = true;
        this.transportaMed = false;
        this.enc = new ArrayList<Encomendas>();
        this.classificacoes=0;
        this.nrClassif=0;
        this.tempoMedporKm = 0.0;

        this.regTempo= new HashMap<>();
    }


    public Voluntarios(String password, String cdV, String no, double latitude, double longitude, double raio, boolean trMed, double tpM){
        this.password=password;
        this.codVoluntario=cdV;
        this.nome=no;
        this.latitude=latitude;
        this.longitude=longitude;
        this.raio=raio;
        this.livre=true;
        this.transportaMed=trMed;
        this.enc = new ArrayList<Encomendas>();
        this.classificacoes=0;
        this.nrClassif=0;
        this.tempoMedporKm=tpM;
        this.regTempo= new HashMap<Encomendas,Double>();
    }

    public Voluntarios(Voluntarios v){
        this.password=v.getPassword();
        this.codVoluntario=v.getCodVoluntario();
        this.nome=v.getNome();
        this.latitude=v.getLatitude();
        this.longitude=v.getLongitude();
        this.raio=v.getRaio();
        this.livre=v.isLivre();
        this.transportaMed=v.isTransportaMed();
        setEnc(v.getEnc());
        this.classificacoes=v.getClassi();
        this.nrClassif=v.getNrC();
        this.tempoMedporKm = v.getTempoMedio();
        setRegTempo(v.getRegTempo());
    }

    /**
    * Método que retorna a password do voluntario 
    */

    public String getPassword() { return password; }

    /**
    * Método que retorna o codigo do voluntario 
    */
    
    public String getCodVoluntario() { return codVoluntario; }

    /**
    * Método que retorna o nome do voluntario 
    */

    public String getNome() { return nome; }

    /**
    * Método que retorna a latitude do voluntario 
    */

    public double getLatitude() { return this.latitude; }

    /**
    * Método que retorna a longitude do voluntario 
    */

    public double getLongitude() { return this.longitude; }

    /**
    * Método que retorna o raio do voluntario 
    */

    public double getRaio() { return this.raio; }

    /**
    * Método que indica se o voluntario esta livre ou nao 
    */

    public boolean isLivre() { return this.livre; }

    /**
    * Método que indica se o voluntario transporta encomendas médicas
    */

    public boolean isTransportaMed() { return this.transportaMed; }

    /**
    * Método que retorna a lista de encomendas transportadas pelo voluntario
    */

    public List<Encomendas> getEnc(){
        ArrayList<Encomendas> en =new ArrayList<>();
        for(Encomendas e : this.enc )
            en.add(e.clone());
        return en;
    }

    /**
    * Método que retorna a classificacao do voluntario
    */

    public int getClassi() {
        return (this.nrClassif == 0)? 5 : (this.classificacoes / this.nrClassif);
    }

    /**
    * Método que retorna o numero de classificacoes 
    */

    private int getNrC() {
        return this.nrClassif;
    }

    /**
    * Método que retorna o tempo medio por km do voluntario
    */

    public double getTempoMedio(){return this.tempoMedporKm;}

    /**
    * Método que associa a encomenda ao tempo que demorou no transporte
    */

    public Map<Encomendas, Double> getRegTempo(){
        Map<Encomendas, Double> reg = new HashMap<>();
        for(Map.Entry<Encomendas, Double> entry : this.regTempo.entrySet())
            reg.put(entry.getKey().clone(), entry.getValue());
        return reg; 
       
    }

   /**
    * Setter do codigo do voluntario
    */

    public void setCodVoluntario(String codVoluntario) { this.codVoluntario = codVoluntario; }

    /**
    * Setter do nome do voluntario
    */

    public void setNome(String nome) { this.nome = nome; }

    /**
    * Setter da latitude do voluntario
    */

    public void setLatitude(double latitude) { this.latitude = latitude; }

    /**
    * Setter da longitude do voluntario
    */

    public void setLongitude(double longitude) { this.longitude = longitude; }

    /**
    * Setter do raio do voluntario
    */

    public void setRaio(double raio) { this.raio = raio; }

    /**
    * Setter da disponibilidade do voluntario
    */

    public void setLivre(boolean livre) { this.livre = livre; }

    /**
    * Setter de se o voluntario tranporta encomendas médicas ou não
    */

    public void setTransportaMed(boolean transportaMed) { this.transportaMed = transportaMed; }

    /**
    * Setter do tempo medio do voluntario por km 
    */

    public void setTempoMedio(double tempoMedporKm) { this.tempoMedporKm = tempoMedporKm;}

    /**
    * Setter de uma encomenda na lista de encomendas
    */

    public void setEnc(List<Encomendas> enc){
        this.enc = new ArrayList<>();
        for(Encomendas e: enc)
            this.enc.add(e);
    }

    /**
    * Setter do registo de tempo 
    */

    public void setRegTempo(Map<Encomendas, Double> reg){
        this.regTempo = new HashMap<>();
        reg.entrySet().forEach(r -> this.regTempo.put(r.getKey().clone(), r.getValue()));
    }
    

    /**
    * Método que gera uma certa aleatoriedade
    */

    void rent() {
        double meteorologia = new Metereologia().getEstacaoAtraso();
        double transito = new Transito().getAtrasoTransito(meteorologia);
        double atraso = (meteorologia % 0.5)+ (transito % 0.5);
        this.tempoMedporKm= this.tempoMedporKm* (1 + atraso);
    }

    /**
    * Método que dado uma classificacao atualiza a calssificacao do voluntario
    */

    public void rate(int Volrating) {
        this.nrClassif++;
        this.classificacoes+= Volrating;
    }

    /**
    * Método que altera a disponibilidade de um voluntario
    */

    void swapState() {
        this.livre = !this.livre;
    }

    /**
     * Método que altera a categoria(medica/nao medica) de um voluntario
     */
    void swapStateMed(){ this.transportaMed = !this.isTransportaMed();}


   /* the average radius for a
	 * spherical approximation of the figure of the Earth is approximately
	 * 6371.01 kilometers.*/
    
     /**
     * Metodo que calcula a distancia a um ponto
     */
    public double distanceTo(double lat1, double lon1) {
        double lati1= Math.toRadians(lat1);
        double long1=Math.toRadians(lon1);
        double lati2= Math.toRadians(this.latitude);
        double long2=Math.toRadians(this.longitude);

        double dist = acos(sin(lati1)* sin(lati2) + cos(lati1) * cos(lati2) * cos(long1 - long2)) * 6371;

        return dist;
    }

    /**
     * Estima o tempo total de transporte do voluntario
     */
    public double estimaTempoVLU(User u, Lojas l){
        rent();
        double tempoVoluntarioL = distanceTo(l.getLat(),l.getLon()) * this.getTempoMedio();
        l.rent();
        double tempoLoja = l.getTpAtend();
        double tempoLU = l.distanceLojaU(u);
        if (l.filaEspera){
            tempoLoja*=l.encA.size();
            return (tempoVoluntarioL + tempoLoja + tempoLU);
        }
        else return (tempoVoluntarioL + tempoLoja + tempoLU);
    }

    /**
    * Método que retorna a lista de encomendas de um voluntario dado um código de utilizador
    */
    

    List<Encomendas> getListencV(String clientID) {
        return this.enc
                .stream()
                .filter(e -> e.getCodUtilizador().equals(clientID))
                .collect(Collectors.toList());
    }

    /**
    * Método que retorna a lista de encomendas de um voluntario dado um determinado user 
    */
    
    List<Encomendas> getListencV(User c) {
        String clientID = c.getCodUtilizador();
        return this.enc
                .stream()
                .filter(e -> e.getCodUtilizador().equals(clientID))
                .collect(Collectors.toList());
    }

    /**
    * Método que retorna a lista de encomendas de um voluntario dado um determinado espaco de tempo
    */

    List<Encomendas> getListencVDH(LocalDateTime init, LocalDateTime end) {

        return this.enc.stream()
                .filter(e ->  e.getDataHora().isBefore(end) && e.getDataHora().isAfter(init))
                .collect(Collectors.toList());
    }

    /**
     * Método que calcula a distancia de um oluntario a uma loja e a distancia de uma loja a um utilizador
     * */
    
    //calcula a distancia de um Voluntario a uma loja + de uma loja a um utilizador
    public double calculaDistanciaEnc( Lojas l, User u, Encomendas e){
        double distVL = distanceTo( l.getLat(), l.getLon());
        double distLU = l.distanceLojaU(u);
        double distTotal = distVL + distLU;
        return distTotal;
    } //permite obter kms precorridos
    /**
    * Método que vai buscar uma encomenda dado uma loja, empresa e utilizador 
    */


    public void buscarEncomenda(Lojas l,Encomendas e,User u){ //repara que os voluntarios sao aceites automaticamente
        //swapState();
        //sleep (estimaTempoVLU)

        this.enc.add(e);
        this.latitude=u.getLatitude();
        this.longitude=u.getLongitude();
        this.regTempo.putIfAbsent(e, estimaTempoVLU(u, l));
        swapState();
    }

    public Voluntarios clone(){return new Voluntarios(this);}

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Voluntarios that = (Voluntarios) o;
        return this.codVoluntario.equals(that.codVoluntario)
                && this.nome.equals(that.nome)
                && this.latitude == that.latitude
                && this.longitude == that.longitude
                && this.raio == that.raio
                && this.livre == that.livre
                && this.transportaMed == that.transportaMed
                && this.enc.equals(that.enc);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Voluntario:").append(this.codVoluntario).append(",")
                                     .append(this.nome).append(",")
                                     .append(this.latitude).append(",")
                                     .append(this.longitude).append(",")
                                     .append(this.raio).append(",")
                                     .append(this.classificacoes).append(",")
                                     .append(this.nrClassif).append(",")
                                     .append(this.tempoMedporKm).append(",")
                                     .append(this.livre).append(",")
                                     .append(this.transportaMed).append(",")
                                     .append(this.enc).append(",")
                                     .append(this.regTempo).append(",");                                   
        return sb.toString();
    }

    @Override
    public int hashCode() {
        return 1;
    }

}
