package Model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.lang.Math;


/**
 * Write a description of class Model.Transportadora here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

/**
 * Classe que guarda toda a informação respetiva a uma transportadora
 */
public class Transportadora implements Serializable
{
    private String pass;
    private String cod;
    private String nome;
    private GPS gps;
    private int nif;
    private double raio;
    private double taxa;
    private double taxaPeso;
    private int pesoLimite;
    public double kms;
    private List<Encomenda> listenc;
    private List<Encomenda> pedidos;
    private List<Encomenda> espera;
    private boolean medico;

    /**
     * Construtor sem parametros
     */
    public Transportadora()
    {
        this.pass = new String();
        this.cod = new String();
        this.nome = new String();
        this.gps = new GPS();
        this.nif = 0;
        this.raio = 0;
        this.taxa = 0;
        this.taxaPeso = 0;
        this.kms = 0;
        this.pesoLimite = 100;
        this.listenc = new ArrayList<>();
        this.pedidos = new ArrayList<>();
        this.espera = new ArrayList<>();
        this.medico = false;
    }

    /**
     * Construtor parametrizado
     * @param s      String com codigo de transportadora
     * @param nome   String com nome
     * @param gps    GPS com localização
     * @param n      int com nif
     * @param r      double com raio de ação
     * @param p      double com taxa sobre a distancia
     * @param tp     double com taxa sobre o peso
     * @param peso   double com peso limite
     * @param lista  Lista com todas as encomendas entregues
     * @param medico boolean que verifica se esta apto a fazer entregas de encomendas medicas
     */
    public Transportadora (String s, String nome, GPS gps,int n, double r, double p,double tp, int peso, ArrayList<Encomenda> lista , boolean medico)
    {
        this.pass = s;
        this.cod = s;
        this.nome = nome;
        this.gps = gps.clone();
        this.nif = n;
        this.raio =r;
        this.taxa = p;
        this.taxaPeso = tp;
        this.pesoLimite = peso;
        this.kms = 0;
        this.setList(lista);
        this.pedidos = new ArrayList<>();
        this.espera = new ArrayList<>();
        this.medico = medico;
    }

    /**
     * Construtor parametrizado
     * @param pass   String com a pass
     * @param s      String com codigo de transportadora
     * @param nome   String com nome
     * @param gps    GPS com localização
     * @param n      int com nif
     * @param r      double com raio de ação
     * @param p      double com taxa sobre a distancia
     * @param tp     double com taxa sobre o peso
     * @param peso   double com peso limite
     * @param med    boolean que verifica se esta apto a fazer entregas de encomendas medicas
     */
    public Transportadora (String pass, String s, String nome, GPS gps,int n, double r, double p,double tp, int peso, boolean med)
    {
        this.pass = pass;
        this.cod = s;
        this.nome = nome;
        this.gps = gps.clone();
        this.nif = n;
        this.raio =r;
        this.taxa = p;
        this.taxaPeso = tp;
        this.kms = 0;
        this.pesoLimite = peso;
        this.listenc = new ArrayList<>();
        this.pedidos = new ArrayList<>();
        this.espera = new ArrayList<>();
        this.medico = med;

    }

    /**
     * Construtor por copia
     * @param u     Transportadora a copiar
     */
    public Transportadora (Transportadora u)
    {
        this.pass = u.getPass();
        this.cod = u.getCod();
        this.nome = u.getNome();
        this.gps = new GPS(u.getGPS());
        this.nif = u.getNif();
        this.raio = u.getRaio();
        this.taxa = u.getTaxa();
        this.taxaPeso = u.getTaxaPeso();
        this.pesoLimite = u.getPesoLimite();
        this.kms = u.getKms();
        this.setList(u.getList());
        this.setPedidos(u.getPedidos());
        this.setEspera(u.getEspera());
        this.medico = u.aceitoTransporteMedicamentos();

    }

    /**
     * Get da variavel medico do objeto
     * @return      boolean
     */
    public boolean aceitoTransporteMedicamentos() {
        return medico;
    }

    /**
     * Set da variavel medico do objeto
     * @param medico    boolean
     */
    public void aceitaMedicamentos(boolean medico) {
        this.medico = medico;
    }

    /**
     * Get da variavel kms do objeto
     * @return      double com os kms
     */
    public double getKms() {
        return kms;
    }

    /**
     * Set da variavel kms do objeto
     * @param kms    double com os kms
     */
    public void setKms(double kms) {
        this.kms = kms;
    }

    /**
     * Set da variavel pass do objeto
     * @param p     String com pass
     */
    public void setPass(String p){this.pass = p;}

    /**
     * Get da variavel pass do objeto
     * @return      String com pass
     */

    public String getPass(){return this.pass;}

    /**
     * Get da variavel cod do objeto
     * @return      String com cod
     */
    public String getCod()
    {
        return this.cod;
    }

    /**
     * Get da variavel nome do objeto
     * @return      String com nome
     */
    public String getNome()
    {
        return this.nome;
    }

    /**
     * Get da variavel gps do objeto
     * @return      GPS
     */
    public GPS getGPS()
    {
        return this.gps;
    }

    /**
     * Get da variavel nif do objeto
     * @return      int com nif
     */
    public int getNif()
    {
        return this.nif;
    }

    /**
     * Get da variavel raio do objero
     * @return      double com raio
     */
    public double getRaio()
    {
        return this.raio;
    }

    /**
     * Get da variavel taxa do objeto
     * @return      double com taxa
     */
    public double getTaxa()
    {
        return this.taxa;
    }

    /**
     * Get da variavel taxaPeso do objeto
     * @return      double com taxaPeso
     */
    public double getTaxaPeso() {
        return taxaPeso;
    }

    /**
     * Set da variavel taxaPeso do objeto
     * @param taxaPeso      double com taxa Peso
     */
    public void setTaxaPeso(double taxaPeso) {
        this.taxaPeso = taxaPeso;
    }

    /**
     * Get da variavel pesoLimite do objeto
     * @return      double com o peso limite
     */
    public int getPesoLimite() {
        return pesoLimite;
    }

    /**
     * Set da variavel pesoLimite do objeto
     * @param pesoLimite   double com peso limite
     */
    public void setPesoLimite(int pesoLimite) {
        this.pesoLimite = pesoLimite;
    }

    /**
     * Get da variavel lisenc do objeto
     * @return      Lista com todas as encomendas
     */
    public ArrayList<Encomenda> getList() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for (Encomenda l : this.listenc)
            aux.add(l);
        return aux;
    }

    /**
     * Get da variavel pedidos do objeto
     * @return      List com todos os pedidos
     */
    public ArrayList<Encomenda> getPedidos() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for (Encomenda l : this.pedidos)
            aux.add(l);
        return aux;
    }

    /**
     * Get da variavel espera do objeto
     * @return      Lista com todas as encomendas em espera para serem aceites pelo utilizador
     */
    public ArrayList<Encomenda> getEspera() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for (Encomenda l : this.espera)
            aux.add(l);
        return aux;
    }

    /**
     * Set da variavel listenc do objeto
     * @param l     Lista com todas encomendas
     */
    public void setList (ArrayList<Encomenda> l)
    {
        this.listenc = new ArrayList<>();
        for(Encomenda li : l)
            this.listenc.add(li);
    }
    /**
     * Set da variavel pedidos do objeto
     * @param l     Lista com todos os pedidos
     */
    public void setPedidos (ArrayList<Encomenda> l)
    {
        this.pedidos = new ArrayList<>();
        for(Encomenda li : l)
            this.pedidos.add(li);
    }
    /**
     * Set da variavel listenc do objeto
     * @param l     Lista com todas encomendas em espera
     */
    public void setEspera (ArrayList<Encomenda> l)
    {
        this.espera = new ArrayList<>();
        for(Encomenda li : l)
            this.espera.add(li);
    }

    /**
     * Set da variavel cod do objeto
     * @param s     String com codigo
     */
    public void setCod(String s)
    {
        this.cod = s;
    }

    /**
     * Set da variavel nome do objeto
     * @param n     String com nome
     */
    public void setNome(String n)
    {
        this.nome = n;
    }

    /**
     * Set da variavel gps do objeto
     * @param l     double com latitude
     * @param lo    double com longitude
     */
    public void setGPS (double l, double lo)
    {
        this.gps.setGPS(l,lo);
    }

    /**
     * Set da variavel nif do objeto
     * @param n     int com nif
     */
    public void setNif (int n)
    {
        this.nif = n;
    }

    /**
     * Set da variavel raio do objeto
     * @param r     doouble com raio
     */
    public void setRaio (double r)
    {
        this.raio = r;
    }

    /**
     * Set da variavel taxa do objeto
     * @param p     doouble com taxa
     */
    public void setTaxa(double p)
    {
        this.taxa = p;
    }

    /**
     * Método que clona este objeto
     * @return      clone do objeto
     */
    public Transportadora clone()
    {
        return new Transportadora(this);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Model.Transportadora{");
        sb.append("cod='").append(cod).append('\'');
        sb.append(", nome='").append(nome).append('\'');
        sb.append(", gps=").append(gps);
        sb.append(", nif=").append(nif);
        sb.append(", raio=").append(raio);
        sb.append(", taxa=").append(taxa);
        sb.append(", taxaPeso=").append(taxaPeso);
        sb.append(", pesoLimite=").append(pesoLimite);
        sb.append(", listenc=").append(listenc);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Método equals deste objeto
     * @param o     Objeto a comparar
     * @return      boolean
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Transportadora)) return false;
        Transportadora that = (Transportadora) o;
        return nif == that.nif &&
                Double.compare(that.raio, raio) == 0 &&
                Double.compare(that.taxa, taxa) == 0 &&
                Double.compare(that.taxaPeso, taxaPeso) == 0 &&
                pesoLimite == that.pesoLimite &&
                Objects.equals(cod, that.cod) &&
                Objects.equals(nome, that.nome) &&
                Objects.equals(gps, that.gps) &&
                Objects.equals(listenc, that.listenc);
    }


    /**
     * Método que calcula o preço de entrega de uma encomenda
     * @param loja      GPS da loja
     * @param user      GOS do utilizador
     * @param e         Encomenda
     * @return          double com preço
     */
    public double getPreço(GPS loja, GPS user, Encomenda e){
        double d1 = this.gps.distancia(loja);
        double d2 = loja.distancia(user);
        return taxa * (d1 + d2) + (taxaPeso * e.getPeso()) ;

    }

    /**
     * Método que calcula um possivel tempo de viagem no Verão
     * @param loja      GPS loja
     * @param user      GPS utilizador
     * @return          double com tempo
     */
    public double getTempoV(GPS loja, GPS user){
        double d1 = this.gps.distancia(loja);
        double d2 = loja.distancia(user);
        double v = Math.random()*((100 - 80)+1);
        return (d1+d2)/v;
    }


        /**
     * Método que calcula um possivel tempo de viagem no Verão
     * @param loja      GPS loja
     * @param user      GPS utilizador
     * @param fila      GPS utilizador
     * @return          double com tempo
     */
    public double getTempoVF(GPS loja, GPS user, int fila){
        double d1 = this.gps.distancia(loja);
        double d2 = loja.distancia(user);
        double timeF = Math.random()*((5 - 30)+1);
        double v = Math.random()*((100 - 80)+1);
        return timeF * fila + (d1+d2)/v;
    }

    /**
     * Método que calcula um possivel tempo de viagem no Inverno
     * @param loja      GPS loja
     * @param user      GPS utilizador
     * @return          double com tempo
     */
    public double getTempoI(GPS loja, GPS user){
        double d1 = this.gps.distancia(loja);
        double d2 = loja.distancia(user);
        double v = Math.random()*((60 - 40)+1);
        return (d1+d2)/v;
    }

        /**
     * Método que calcula um possivel tempo de viagem no Inverno
     * @param loja      GPS loja
     * @param user      GPS utilizador
     * @param fila      GPS utilizador
     * @return          double com tempo
     */
    public double getTempoIF(GPS loja, GPS user, int fila){
        double d1 = this.gps.distancia(loja);
        double d2 = loja.distancia(user);
        double timeF = Math.random()*((5 - 30)+1);
        double v = Math.random()*((60 - 40)+1);
        return timeF * fila + (d1+d2)/v;
    }

    /**
     * Método que calcula um possivel tempo de viagem na Primavera e Outono
     * @param loja      GPS loja
     * @param user      GPS utilizador
     * @return          double com tempo
     */

    public double getTempoPO(GPS loja, GPS user){
        double d1 = this.gps.distancia(loja);
        double d2 = loja.distancia(user);
        double v = Math.random()*((90 - 65)+1);
        return (d1+d2)/v;
    }

    /**
     * Método que calcula um possivel tempo de viagem na Primavera e Outono
     * @param loja      GPS loja
     * @param user      GPS utilizador
     * @param fila      GPS utilizador
     * @return          double com tempo
     */

    public double getTempoPOF(GPS loja, GPS user, int fila){
        double d1 = this.gps.distancia(loja);
        double d2 = loja.distancia(user);
        double timeF = Math.random()*((5 - 30)+1);
        double v = Math.random()*((90 - 65)+1);
        return timeF * fila + (d1+d2)/v;
    }

    /**
     * Método que verifica se duas pass sao iguais
     * @param p     String com pass
     * @return      boolean
     */
    public boolean validaPass(String p){
        return this.pass.equals(p);
    }

    /**
     * Método que coloca uma encomenda na lista de espera e retira dos pedidos
     * @param e     Encomenda aceite
     */
    public void aceitaPedido(Encomenda e){
        this.pedidos.remove(e);
        this.espera.add(e);
    }

    /**
     * Método que retira dos pedidos uma encomenda recusada
     * @param e     Encomenda
     */
    public void rejeitaPedido(Encomenda e){ this.pedidos.remove(e);}


    /**
     * Método que adiciona uma encomenda a variavel pedidos
     * @param e     Encomenda a adicionar
     */
    public void addPedido(Encomenda e){
        this.pedidos.add(e);
    }

    /**
     * Método que remove uma encomenda da variavel pedidos
     * @param e     Encomenda a remover
     */
    public void rmPedido(Encomenda e){
        this.pedidos.remove(e);
    }

    /**
     * Método que adiciona uma encomenda a variavel listenc
     * @param e     Encomenda a adicionar
     */
    public void addEncT(Encomenda e){
        this.listenc.add(e);
    }

    /**
     * Método que adicionar mais kms á variavel kms
     * @param kms       double com kms
     */
    public void addKms(double kms){this.kms+=kms;}

    /**
     * Método que devolve a média das classificações das encomendas entregues
     * @return      double com média
     */
    public double getClGeral(){
        double r=0;
        double size = 0;
        for(Encomenda e : this.listenc){
            if(e.getClassificacao()!=-1 && e.getEntregue()){
                r+=e.getClassificacao();
                size++;
            }
        }
        if(size==0) return 0;
        else return r/size;
    }

    /**
     * Set da variavel classificação de uma encomenda
     * @param e     Encomenda a classificar
     * @param cl    int com classificação
     */
    public void setCl(Encomenda e, int cl){
        Iterator<Encomenda> it = pedidos.iterator();
        boolean f = false;
        while(it.hasNext() && !f){
            Encomenda enc = it.next();
            if(enc.equals(e)){
                enc.setClassificacao(cl);
                f=true;
            }
        }
    }


    /**
     * Método que faz todas as alterações respetivas a entregar ume encomenda
     * @param e     Encomenda entregue
     * @param fi    LocalDateTime com data e hora da entrega
     */
    public void encEntregue(Encomenda e, LocalDateTime fi) {
        Iterator<Encomenda> it = this.listenc.iterator();
        boolean f = false;
        while (it.hasNext() && !f) {
            Encomenda e1 = it.next();
            if (e1.equals(e)) {
                e1.setDataf(fi);
                e1.setEntregue(true);
                f = true;
            }
        }
    }
}