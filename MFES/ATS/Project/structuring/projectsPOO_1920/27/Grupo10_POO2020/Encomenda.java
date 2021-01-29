/**
 * Classe Encomenda
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611
 */

import java.util.*;
import java.time.LocalDateTime;
import java.io.Serializable;
public class Encomenda implements Serializable{
    private double preco;
    private double peso;
    private ArrayList<Produto> produtos;
    private String codEncomenda;
    private Boolean encomendaMedica;
    private Cliente destino;
    private Loja vendedor;
    private LocalDateTime hora_transporte;
    private Transporte transporte;

    /**
     * Construtores
     */
    public Encomenda(){
       this.preco = 0;
       this.peso = 0;
       this.produtos = new ArrayList<>();
       this.codEncomenda = "N/a";
       this.destino = new Cliente();
       this.encomendaMedica = false;
       this.vendedor = new Loja();
       this.hora_transporte = this.hora_transporte.now();
       this.transporte = new Voluntario();
    }
    
    public Encomenda(double preco,
                    Cliente destino, 
                    double peso, 
                    ArrayList<Produto> produto, 
                    String codEncomenda, 
                    boolean encomendaMedica,
                    Loja vendedor,
                    LocalDateTime hora_transporte,
                    Transporte transporte){
       this.preco = preco;
       this.peso = peso;
       this.produtos = produto;
       this.codEncomenda= codEncomenda;
       this.destino = destino;
       this.encomendaMedica = encomendaMedica;
       this.vendedor = vendedor;
       this.hora_transporte = hora_transporte;
       this.transporte = transporte;
    }
    
    public Encomenda (Encomenda e){
        this.preco = e.getPreco();
        this.peso = e.getPeso();
        this.produtos = e.getProduto();
        this.codEncomenda=e.getCodEncomenda();
        this.destino = e.getDestino();
        this.encomendaMedica = e.getEncomendaMedica();
        this.vendedor = e.getVendedor();
        this.hora_transporte = e.getHora();
        this.transporte = e.getTransporte();
    }

    /**
     * Get's
     */
    public LocalDateTime getHora(){
        return this.hora_transporte;
    }
    
    public Cliente getDestino(){
        return this.destino.clone();
    }
    
    public Transporte getTransporte(){
        return this.transporte.clone();
    }
    
    public Loja getVendedor(){
        return this.vendedor.clone();
    }
    
    public double getPreco(){
        return this.preco;
    }
    
    public double getPeso(){
        return this.peso;
    }
    
    public String getCodEncomenda(){
        return this.codEncomenda;
    }
    
    public ArrayList<Produto> getProduto(){
        ArrayList<Produto> copia = new ArrayList<>();
        for(Produto s: this.produtos){
            copia.add(s);
        }
        return copia;
    }
    
    public Boolean getEncomendaMedica(){
        return this.encomendaMedica;  
    }
    
    /**
     * Set's
     */
    public void setPreco(double preco){
        this.preco = preco;
    }

    public void setHora(LocalDateTime hora){
        this.hora_transporte = hora;
    }
    
    public void setDestino(Cliente destino){
        this.destino = destino;  
    }
    
    public void setVendedor(Loja vendedor){
        this.vendedor = vendedor;  
    }
    
    public void setPeso(double peso){
        this.peso=peso;
    }
    
    public void setCodEncomenda(String codEncomenda){
        this.codEncomenda=codEncomenda;
    }
    
    public void setProduto(ArrayList<Produto> Produto){
            ArrayList<Produto> n = new ArrayList<>();
            for (Produto s: n){
                n.add(s.clone());
            }
    }
    
    public void setEncomendaMedica(Boolean encomendaMedica){
        this.encomendaMedica=encomendaMedica;  
    }
    
    public void setTransporte(Transporte transporte){
        this.transporte = transporte;
    }
    
    /**
     * clone
     */
    public Encomenda clone(){
        return new Encomenda(this);
    } 
    
    /**
     * equals
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Encomenda le = (Encomenda) obj;
        return le.getCodEncomenda().equals(this.codEncomenda);
    }
    
    /**
     * toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Encomenda: ").append(this.codEncomenda);
        sb.append("Destino: ").append(this.destino);
        sb.append("Vendedor: ").append(this.vendedor);
        sb.append("Preco: ").append(this.preco);
        sb.append("Encomenda médica: ").append(this.encomendaMedica);
        return sb.toString();
    }
    
    public int compareTo(Encomenda x){
        return this.codEncomenda.compareTo(x.getCodEncomenda());  
    }
    
    /** 
     * Calcula Valor peso total da encomenda
     */
    public double pesototal(){
        double total=0;
        for(Produto e: this.produtos){
            total += e.pesoTotal();
        }
        return total;
    }
    
    /**
     * Calcula o número total de produtos da encomenda
     */
    public int TotalProdutos(){
        int total1 = 0;
        for(Produto e: this.produtos){
            total1 = total1 + e.getQuantidade();
        }
        return total1;
    }
    
    public double precototal(){
        double total=0;
        for(Produto e: this.produtos){
            total += e.calculaPreco();
        }
        return total;
    }
    
    /**
     * Verifica se um dado produto existe
     */
    public boolean existeProdutoEncomenda(String refProduto){
        boolean res = false;
        for(Produto e : this.produtos){
            if(refProduto.equals(e.getCodProduto())){
                res = true;
                break;
            }
        }
        return res;
    }
    
    /**
     * Remove um produto da encomenda
     */
    public void removeProduto(String codProd){
        int i = 0;
        for (Produto e : this.produtos){
            if (codProd.equals(e.getCodProduto())){
                this.produtos.remove(i);
                break;
            }
            i++;
        }
    }
    
    /**
     * Adiciona à encomenda
     */
    public void adicionaProduto(Produto produto){
        this.produtos.add(produto);
    }
}

