import java.time.LocalDate;
import java.io.Serializable;
import java.util.*;
/**
 * Escreva a descrição da classe Entregas aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Entregas implements Serializable{
    private String codeUser;
    private String codeLoja;
    private String codeVoluntario;
    private String codeEmpresa;
    private String codEncomenda;
    private double pendente; 
    private double km;
    private double gpsX;
    private double gpsY;
    private int estado;
    
    /**
     * Construtor para objetos da classe Entregas (por omissao)
     */
    public Entregas(){
        this.codeUser = new String();
        this.codeLoja = new String();
        this.codeVoluntario = new String();
        this.codeEmpresa = new String();
        this.codEncomenda = new String();
        this.pendente = 0;
        this.km = 0;
        this.gpsX = 0;
        this.gpsY = 0;
    }
    
    /**
     * Construtor para objetos da classe AluguerCarro (parametrizado).
     * 
     * @param  eu   o code do utilizador
     * @param  el   o code da loja
     * @param  vol   o code do voluntario
     * @param  emp   o code da empresa
     * @param  cod  o codigo da encomenda
     * @param  p   o estado da encomenda
     * @param  k   os quilometros percorridos
     * @param  x   a coordenada x de destino
     * @param  y   a coordenada y de destino
     */
    
    public Entregas (String eu, String el,String vol, String emp, String cod, double p,double k, double x, double y){
        this.setCodeUser(eu);
        this.setCodeLoja(el);
        this.setCodeVoluntario(vol);
        this.setCodeEmpresa(emp);
        this.setCodEncomenda(cod);
        this.setPendente(p);
        this.setKm(k);
        this.setGpsX(x);
        this.setGpsY(y);
    }
    /**
     * Construtor para objetos da classe Entregas (de copia)
     * 
     * @param  e   a encomenda
     */
    public Entregas(Entregas e){
        this.codeUser = e.getCodeUser();
        this.codeLoja = e.getCodeLoja();
        this.codeVoluntario = e.getCodeVoluntario();
        this.codeEmpresa = e.getCodeEmpresa();
        this.codEncomenda = e.getCodEncomenda();
        this.pendente = e.getPendente();
        this.km = e.getKm();
        this.gpsX = e.getGpsX();
        this.gpsY = e.getGpsY();
    }
    
    /*Getters*/
    
    public String getCodeUser(){
        return this.codeUser;
    }
    public String getCodeLoja(){
        return this.codeLoja;
    }
    public String getCodeVoluntario(){
        return this.codeVoluntario;
    }
    public String getCodeEmpresa(){
        return this.codeEmpresa;
    }
    public String getCodEncomenda(){
        return this.codEncomenda;
    }
    public double getPendente(){
        return this.pendente;
    }
    public double getKm(){
        return this.km;
    }
    public double getGpsX(){ 
        return this.gpsX;
    }
    public double getGpsY(){
        return this.gpsY;
    }
    public int getEstadoEntrega(){
        return this.estado;
    }
    
    /*Setters*/
    public void setCodeUser(String eu){
        this.codeUser = eu;
    }
    public void setCodeLoja(String el){
        this.codeLoja = el;
    }
    public void setCodeVoluntario(String vol){
        this.codeVoluntario = vol;
    }
    public void setCodeEmpresa(String emp){
        this.codeEmpresa = emp;
    }
    public void setCodEncomenda(String cod){
        this.codEncomenda = cod;
    }
    public void setPendente(double p){
        this.pendente = p;
    }
    public void setKm(double k){
        this.km = k;
    }
    public void setGpsX(double x){
        this.gpsX = x;
    }
    public void setGpsY(double y){
        this.gpsY = y;
    }
    public void setEstadoEntrega(int i){
        this.estado = i;
    }
    
    /**
     * Metodo que duplica as entregas
     * 
     * @return     o clone da entrega
     */
    public Entregas clone(){
        return new Entregas(this);
    }
    
    /**
     * Metodo que verifica se duas entregas sao iguais
     * 
     * @param  o   o objeto a comparar
     * 
     * @return     o resultado da comparacao dos duas entregas
     */
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;
        else{
            Entregas a = (Entregas) o;
            return(a.getCodeUser().equals(this.getCodeUser())
                   && a.getCodeLoja().equals(this.getCodeLoja())
                   && a.getCodeVoluntario().equals(this.getCodeVoluntario())
                   && a.getCodeEmpresa().equals(this.getCodeEmpresa())
                   && a.getCodEncomenda().equals(this.getCodEncomenda())
                   && a.getPendente() == this.getPendente()
                   && a.getKm() == this.getKm()
                   && a.getGpsX() == this.getGpsX()
                   && a.getGpsY() == this.getGpsY());
        }
    }
    /**
     * Metodo que converte uma entrega para uma string
     * 
     * @return    a entrega em string
     */
    public String toString(){
        String aux = "Email do Utilizador: " + this.codeUser + ";\n"
                     + "Email da Loja: " + this.codeLoja + ";\n"
                     + "Email do Voluntario: " + this.codeVoluntario + ";\n"
                     + "Email da Empresa: " + this.codeEmpresa + ";\n"
                     + "Encomenda: " +  this.codEncomenda + ";\n"
                     + "Estado: " + this.pendente + ";\n"
                     + "Quilometros: " + this.km + ";\n"
                     + "Destino X " + this.gpsX + ";\n"
                     + "Destino Y: " + this.gpsY + ";\n";
        return aux;
    }
    /**
     * Metodo que devolve o codigo de hash para uma entrega
     * 
     * @return     o hashcode
     */
    public int hashCode(){
        int hash = 7; 
        hash = 31 * hash + codeUser.hashCode();
        hash = 31 * hash + codeLoja.hashCode();
        hash = 31 * hash + codeVoluntario.hashCode();
        hash = 31 * hash + codeEmpresa.hashCode();
        hash = 31 * hash + codEncomenda.hashCode();
        long aux2 = Double.doubleToLongBits(pendente);
        hash = 31 * hash + (int)(aux2 ^ (aux2 >>> 32));
        long aux = Double.doubleToLongBits(km);
        hash = 31 * hash + (int)(aux ^ (aux >>> 32));
        long aux3 = Double.doubleToLongBits(gpsX);
        hash = 31 * hash + (int)(aux3 ^ (aux3 >>> 32));
        long aux4 = Double.doubleToLongBits(gpsY);
        hash = 31 * hash + (int)(aux4 ^ (aux4 >>> 32));
        return hash;
    }
    
    /**
     * Metodo que implementa a ordem natural de comparacao de instancias de AluguerCarro
     */
    public int compareTo(Entregas c){
        return c.getCodEncomenda().compareTo(this.codEncomenda);
    }
}
