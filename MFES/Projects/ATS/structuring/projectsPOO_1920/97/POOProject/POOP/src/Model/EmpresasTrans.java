package Model;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static java.lang.Math.*;
import static java.lang.Math.cos;

import java.io.Serializable;

/**
 * Classe que representa cada Empresa Transportadora
 */

public class EmpresasTrans implements Serializable{
    private static final long serialVersionUID = -1585902623816836810L;
    private String password;
    private String codEmpresa;
    private String nome;
    private double latitude;
    private double longitude;
    private String nif;
    private double raioA;
    private double taxa; //preco por km
    public boolean livre;
    public boolean isMed;
    private double tempoMedioporKm;
    public int nrEnc;
    private int classificacoes; // 0 a 5
    private int nrClassif;
    private double nrKmsfeitos;
    private double precoPeso;
    private double precoTempo;
    private Map<Encomendas,custoTempo> regCustoTempo;

    public EmpresasTrans(){
        this.password="";
        this.codEmpresa="";
        this.nome="";
        this.latitude=0.0;
        this.longitude=0.0;
        this.nif="";
        this.raioA=0.0;
        this.taxa=0.0;
        this.livre=true;
        this.isMed=false;
        this.nrEnc=0;
        this.tempoMedioporKm=0.0;
        this.classificacoes = 0;
        this.nrClassif = 0;
        this.nrKmsfeitos=0.0;
        this.precoPeso=0.0;
        this.precoTempo=0.0;
        this.regCustoTempo = new HashMap<>();

    }

    public EmpresasTrans(String password, String cdE ,String n,double la,double lo,String nif, double r,double t, boolean isMed,double tmpKm,double pp,double pt) {
        this.password = password;
        this.codEmpresa=cdE;
        this.nome=n;
        this.latitude = la;
        this.longitude = lo;
        this.nif=nif;
        this.raioA = r;
        this.taxa = t;
        this.livre = true;
        this.isMed=isMed;
        this.nrEnc = 0;
        this.tempoMedioporKm=tmpKm;
        this.classificacoes = 0;
        this.nrClassif = 0;
        this.nrKmsfeitos=0;
        this.precoPeso = pp;
        this.precoTempo = pt;
        this.regCustoTempo = new HashMap<Encomendas,custoTempo>();     
    }


    public EmpresasTrans(EmpresasTrans e) {
        this.password = e.getPassword();
        this.codEmpresa=e.getCodEmpresa();
        this.nome=e.getNome();
        this.latitude =e.getLatitude();
        this.longitude =e.getLongitude();
        this.nif=e.getNif();
        this.raioA = e.getRaioA();
        this.taxa = e.getTaxa();
        this.livre =e.isLivre();
        this.isMed = e.isMED();
        this.nrEnc =e.getNrEnc();
        this.tempoMedioporKm= e.getTempoMedioporKm();
        this.classificacoes = e.getClassificacoes();
        this.nrClassif= e.getNrClassificacoes();
        this.nrKmsfeitos=e.getNrKmsFeitos();
        this.precoPeso = e.getPrecoPeso();
        this.precoTempo = e.getPrecoTempo();
        this.regCustoTempo = e.getRegCustoTempo();
    }

   /**
    * Getter da Password da empresa
    */

    public String getPassword(){ return password; }

    /**
    * Getter do Codigo da empresa
    */

    public String getCodEmpresa() { return codEmpresa; }

    /**
    * Getter do Nome da empresa
    */

    public String getNome() { return nome; }

    /**
    * Getter da Latitude da empresa
    */

    public double getLatitude() { return latitude; }

    /**
    * Getter da Longitude da empresa
    */

    public double getLongitude() { return longitude; }

    /**
    * Getter do Nif da empresa
    */

    public String getNif() { return nif; }

    /**
    * Getter da Taxa da empresa
    */

    public double getTaxa() { return taxa; }

    /**
    * Getter do raio da empresa
    */

    public double getRaioA() { return raioA; }

    /**
    * Getter da disponibilidade da empresa
    */

    public boolean isLivre() { return livre; }

    /**
    * Getter de se a encomenda transportada é médica ou não
    */

    public boolean isMED(){ return isMed;}

    /**
    * Getter do número de encomendas da empresa
    */

    public int getNrEnc() { return nrEnc; }

    /**
    * Getter do tempo médio por km da empresa
    */

    public double getTempoMedioporKm() { return tempoMedioporKm; }

    /**
    * Getter do numero de classificacoes da empresa
    */

    public int getNrClassificacoes(){return this.nrClassif;}

    /**
    * Getter das classificacoes da empresa
    */

    public int getClassificacoes(){return (this.nrClassif == 0)? 0 : (this.classificacoes / this.nrClassif);}

    /**
    * Getter dos numeros de kms feitos da empresa
    */

    public double getNrKmsFeitos () {return this.nrKmsfeitos;}

    /**
    * Getter do preco por unidade de peso 
    */

    public double getPrecoPeso(){return this.precoPeso;}

    /**
    * Getter do preco por unidade de tempo
    */

    public double getPrecoTempo(){return this.precoTempo;}

    /**
    * Getter do registo do Custo de Tempo da empresa
    */

    public Map<Encomendas, custoTempo> getRegCustoTempo(){
        Map<Encomendas, custoTempo> reg = new HashMap<>();
        for(Map.Entry<Encomendas, custoTempo> entry : this.regCustoTempo.entrySet())
            reg.put(entry.getKey().clone(), entry.getValue());
        return reg; 
    }

     /**
    * Setter do Codigo da empresa
    */
    
    public void setCodEmpresa(String codEmpresa) { this.codEmpresa = codEmpresa; }

    /**
    * Setter do Nome da empresa
    */

    public void setNome(String nome) { this.nome = nome; }

    /**
    * Setter do Nif da empresa
    */
    public void setNif(String nif) { this.nif = nif; }

    /**
    * Setter da Latitude da empresa
    */

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
    * Setter da Longitude da empresa
    */

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
    * Setter do Raio da empresa
    */

    public void setRaioA(double raioA) {
        this.raioA = raioA;
    }

    /**
    * Setter da Taxa da empresa
    */

    public void setTaxa(double taxa) {
        this.taxa = taxa;
    }

    /**
    * Setter do numero de encomendas da empresa
    */

    public void setNrEnc(int nrEnc) {
        this.nrEnc = nrEnc;
    }

    /**
    * Setter da disponibilidade da empresa
    */

    public void setLivre(boolean livre) {
        this.livre = livre;
    }

    /**
    * Setter de se a encomenda transportada é médica ou não
    */

    public void setMed(boolean med) { isMed = med; }

    /**
    * Setter do tempo medio por km
    */

    public void setTempoMedioporKm(double tempoMedioporKm) { this.tempoMedioporKm = tempoMedioporKm; }

    /**
    * Setter do numero de kms feitos da empresa
    */

    public void settNrKmsFeitos(double kms) {this.nrKmsfeitos = kms; }

    /**
    * Setter do registo do Custo de Tempo da empresa
    */

    public void setRegCustoTempo(Map<Encomendas, custoTempo> reg){
        this.regCustoTempo = new HashMap<>();
        reg.entrySet().forEach(r -> this.regCustoTempo.put(r.getKey().clone(), r.getValue()));
    }

    /**
     * Método para influenciar o tempo devido a aleatoriedades
     */
    
    public void rent() {
        double meteorologia = new Metereologia().getEstacaoAtraso();
        double transito = new Transito().getAtrasoTransito(meteorologia);
        double atraso = (meteorologia % 0.5)+ (transito % 0.5);
        this.tempoMedioporKm= this.tempoMedioporKm* (1 + atraso);
    }
 
    /**
     * Método que adiciona uma encomenda á empresa 
     */
    public void adicionaEncomenda( Encomendas e, custoTempo ct){
        this.regCustoTempo.putIfAbsent(e, ct);
    }

    /**
     * Método que altera o estado de disponibilidade de uma empresa
     */

    public void swapState() {
        this.livre = !this.livre;
    }

    /**
    * Método que altera se a transporta encomendas medicas ou nao 
    */

    public void swapStateMed(){ this.isMed = !this.isMed;}

    /**
    * Método que serce para atualizar a classificação da empresa
    */

    public void rate(int rating) {
        this.nrClassif++;
        this.classificacoes += rating;
    }

    /**
     * Método que determina a distancia a um ponto 
     */
    public double distanceTo(double lat1, double lon1) {
        double lati1= Math.toRadians(lat1);
        double long1=Math.toRadians(lon1);
        double lati2= Math.toRadians(this.latitude);
        double long2=Math.toRadians(this.longitude);

        double dist = acos(sin(lati1)* sin(lati2) + cos(lati1) * cos(lati2) * cos(long1 - long2)) * 6371;

        //supostamnte estamos a calcular a distancia entre o voluntario e um ponto
        //retorna em kms
        return dist;
    }

    /**
    * Método que obtém as encomendas da empresa
    */

    public Set<Encomendas> getEncomendas(){
        return this.regCustoTempo.keySet();
    }

    /**
     * Método que devolve a lista de encomendas de um dado cliente
     */

    List<Encomendas> getListClient(String clientID) {
        return this.regCustoTempo.keySet()
                                 .stream()
                                 .filter(e -> e.getCodUtilizador().equals(clientID))
                                 .collect(Collectors.toList());
    }

    
    /**
    * Método que devolve a lista de encomendas entre datas e horas
    */
    List<Encomendas> getListencEDH( LocalDateTime init, LocalDateTime end) {
        return this.regCustoTempo.keySet().stream()
                                          .filter(e -> e.getDataHora().isBefore(end) && e.getDataHora().isAfter(init))
                                          .collect(Collectors.toList());
    }

    /**
    * Método que calcula a distancia total da encomenda
    */   

    public double calculaDistanciaEnc( Lojas l, User u){
        double distEL = distanceTo( l.getLat(), l.getLon());
        double distLU = l.distanceLojaU(u);
        double distTotal = distEL + distLU;
        return distTotal;
    }

    /**
     * Método que calcula o preco do transporte de uma encomenda em funcao da distancia,taxa e peso da encomenda
     */

    public double calculaPrecoDist( double dist){ 
        return dist * this.getTaxa();
    }

    /**
     * Método que calcula o preco relacionado com o peso da encomenda
     */

    public double calculaPrecoPeso( Encomendas e){ 
        return e.getPeso() * this.getPrecoPeso();
    }

    /**
     * Método que calcula o preco relacionado com o tempo da fila de espera
     */

    public double calculaPrecoTempo( Lojas l){ 
        double tempo = 0.0;
        if(l.filaEspera)
            tempo = l.getTpAtend() *this.getPrecoTempo() * l.getEncR().size();
        return tempo;
    }

    /**
     * Método que calcula o preco total do transporte da encomenda
     */
    public double calculaPrecoTotal( User u, Encomendas e, Lojas l){
        return calculaPrecoDist(calculaDistanciaEnc(l, u)) + calculaPrecoPeso(e) + calculaPrecoTempo(l) + e.custoEncomenda();
    }

    /**
     * Método que estima o tempo necessario para o transporte de uma encomenda
     */
    public double estimaTempo( User u, Lojas l){
        rent();
        double tempoEmpresa = distanceTo(l.getLat(),l.getLon()) * this.getTempoMedioporKm();
        l.rent();
        double tempoLoja = l.getTpAtend();
        double tempoLU = l.distanceLojaU(u);
        if (l.filaEspera){
            tempoLoja*=l.encA.size();
            return (tempoEmpresa +tempoLoja + tempoLU);
        }
        else return (tempoEmpresa + tempoLoja + tempoLU);
    }

    /**
     * Método que retorna o custo e o tempo resultante do ato de ir buscar a encomenda á loja
     */

    public custoTempo buscarenc(User u, Lojas l,double dist,Encomendas e){
        double tempo = estimaTempo(u,l);
        double custo = calculaPrecoTotal(u,e,l);
        custoTempo ct = new custoTempo(custo,tempo);
        this.nrKmsfeitos += dist;
        swapState();
        return ct;
    }


    
    public EmpresasTrans clone(){ return  new EmpresasTrans(this);}

    public String toString()
    { StringBuilder sb = new StringBuilder();
        sb.append("Transportadora:").append(this.codEmpresa).append(",")
                                     .append(this.nome).append(",")
                                     .append(this.latitude).append(",")
                                     .append(this.longitude).append(",")
                                     .append(this.nif).append(",")
                                     .append(this.raioA).append(",")
                                     .append(this.taxa).append(",")
                                     .append(this.livre).append(",")
                                     .append(this.isMed).append(",")
                                     .append(this.classificacoes).append(",")
                                     .append(this.nrClassif).append(",")
                                     .append(this.nrKmsfeitos).append(",")
                                     .append(this.precoPeso).append(",")
                                     .append(this.precoTempo).append(",")
                                     .append(this.regCustoTempo).append(",");
        return sb.toString();
    }
 
}
