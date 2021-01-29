/**
 * Escreva a descrição da classe Empresa aqui.
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611
 */
import java.util.*;
import java.time.LocalDateTime;
import java.io.Serializable;

public class Empresa extends Transporte implements Serializable{
    private int nif;
    private double preco_por_km;
    private int numero_de_encomendas;

    /**
     * Construtores
     */
    public Empresa(){
        this.nif = 0;
        this.preco_por_km = 0;
        this.numero_de_encomendas = 0;
    }

    public Empresa(String codT,String nome,double kms,Coordenadas gps,double raio,double mediaclassificacao,double velocidade,boolean disponivel,List<Encomenda> registoT,
                    boolean certificado,int nif, double preco_por_km,int numero_de_encomendas){
        super(codT, nome,kms, gps, raio, mediaclassificacao, velocidade, disponivel,registoT, certificado);
        this.nif = nif;
        this.preco_por_km = preco_por_km;
        this.numero_de_encomendas = numero_de_encomendas;
    }
    
    public Empresa(Empresa v){
        super(v);
        this.nif = v.getNIF();
        this.preco_por_km = v.getPrecoKm();
        this.numero_de_encomendas = getNumDeEnc();
    }
    
    /**
     * Get's
     */
    public int getNIF(){
        return this.nif;
    }
    
    public double getPrecoKm(){
        return this.preco_por_km;
    }
    
    public int getNumDeEnc(){
        return this.numero_de_encomendas;
    }
    
    /**
     * Set's
     */
    public void setNIF(int nif){
        this.nif = nif;
    }
    
    public void setPrecoKm(double preco){
        this.preco_por_km = preco;
    }
    
    public void setNumDeEncomendas(int n){
        this.numero_de_encomendas = n;
    }
    
    /**
     * clone
     */
    public Empresa clone(){
        return new Empresa(this);
    }
    
    /**
     * equals
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Empresa l = (Empresa) obj;
        return super.equals(l) &&
               l.getNIF() == this.nif;
    }
    
    /**
     * toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("NIF: ").append(this.nif);
        sb.append("Preco por Km: ").append(this.preco_por_km);
        sb.append("Numero de Encomendas: ").append(this.numero_de_encomendas);
        return super.toString() + sb.toString();
    }
    
    /**
     * Calcula o total faturado por uma mepresa, num determinado periodo de tempo
     */
    public double totalFaturado(LocalDateTime hora1, LocalDateTime hora2){
        double total = 0;
        for (Encomenda e: this.getRegistoT()){
            if (e.getHora().isAfter(hora1) && e.getHora().isBefore(hora2)){
                total += (this.getGps().distancia(e.getVendedor().getGps()) + (e.getDestino().getGps().distancia(e.getVendedor().getGps()))) * this.preco_por_km;
            }
        }
        return total;
    }
}
