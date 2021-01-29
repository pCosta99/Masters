/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.*;
import java.util.stream.Collectors;
import java.util.Iterator;
import java.io.Serializable;

public abstract class Encomenda implements Serializable {
    
    private String codigoEncomenda;
    private String codigoUtilizador;
    private String codigoTransportador;
    private String codigoLoja;
    private double peso;
    private List<Produto> produtos;
    private EstadoEncomenda estadoEncomenda;
    private double distancia;
    private double duracaoEntrega;
    double precoTransporte;
    private Date dataEncomenda;
    private Date dataEntrega;
    
    private static double velocidadeMediaTransporte = 50;
    
    public Encomenda() {
        this.codigoEncomenda = "";
        this.codigoUtilizador = "";
        this.codigoLoja = "";
        this.codigoTransportador = "";
        this.peso = 0;
        this.produtos = new ArrayList<>();
        this.estadoEncomenda = EstadoEncomenda.SOLICITADO;
        this.distancia = 0;
        this.duracaoEntrega = 0;
        this.precoTransporte = 0;
        this.dataEncomenda = new Date();
        this.dataEntrega = new Date();
    }
    
     public Encomenda(String codigoEncomenda, String codigoUtilizador, String codigoLoja, double peso, List<Produto>produtos, EstadoEncomenda estadoEncomenda) {
        this.codigoEncomenda = codigoEncomenda;
        this.codigoUtilizador = codigoUtilizador;
        this.codigoLoja = codigoLoja;
        this.codigoTransportador = "";
        this.peso = peso;
        setProdutos(produtos);
        this.estadoEncomenda = estadoEncomenda;
        this.distancia = 1;
        this.duracaoEntrega = 1;
        this.precoTransporte = 0;
        this.dataEncomenda = new Date();
        this.dataEntrega = new Date();
    }
    
    public Encomenda(Encomenda e) {
        this.codigoEncomenda = e.getCodigoEncomenda();
        this.codigoUtilizador = e.getCodigoUtilizador();
        this.codigoLoja =  e.getCodigoLoja();
        this.codigoTransportador = e.getCodigoTransportador();
        this.peso = e.getPeso();
        this.produtos = e.getProdutos();
        this.estadoEncomenda = e.getEstadoEncomenda();
        this.distancia = e.getDistancia();
        this.duracaoEntrega = e.getDuracaoEntrega();
        this.precoTransporte = e.getPrecoTransporte();
        this.dataEncomenda = e.getDataEncomenda();
        this.dataEntrega = e.getDataEntrega();
    }
    
    public String getCodigoEncomenda() {
        return this.codigoEncomenda;
    }
    
    public String getCodigoUtilizador() {
        return this.codigoUtilizador;
    }
    
    public String getCodigoLoja() {
        return this.codigoLoja;
    }
    
    public String getCodigoTransportador() {
        return this.codigoTransportador;
    }
    
    public double getPeso() {
        return this.peso;
    }
    
    public List<Produto> getProdutos() {
        return this.produtos.stream().map(Produto::clone).collect(Collectors.toList());
    }
    
    public EstadoEncomenda getEstadoEncomenda() {
        return this.estadoEncomenda;
    }
    
    public double getDistancia() {
        return this.distancia;
    }
    
    public double getDuracaoEntrega() {
        return this.duracaoEntrega;
    }
    
    public double getPrecoTransporte() {
        return this.precoTransporte;
    }
    
    public Date getDataEncomenda() {
        return this.dataEncomenda;
    }
    
    public Date getDataEntrega() {
        return this.dataEntrega;
    }
    
    public void setCodigoEncomenda(String codigoEncomenda) {
        this.codigoEncomenda = codigoEncomenda;
    }
    
    public void setCodigoUtilizador(String codigoUtilzador) {
        this.codigoUtilizador = codigoUtilzador;
    }
    
    public void setCodigoLoja(String codigoLoja) {
        this.codigoLoja = codigoLoja;
    }
    
    public void setCodigoTransportador(String codigoTransportador) {
        this.codigoTransportador = codigoTransportador;
    }
    
    public void setPeso(double peso) {
        this.peso = peso;
    }
    
    private void setProdutos(List<Produto> produtos) {
        this.produtos = produtos.stream().map(Produto::clone).collect(Collectors.toList());
    }
    
    public void setEstadoEncomenda(EstadoEncomenda estadoEncomenda) {
        this.estadoEncomenda = estadoEncomenda;
    }
    
    public void setDistancia(double distancia) {
        this.distancia = distancia;
        this.duracaoEntrega = distancia/velocidadeMediaTransporte;
    }
    
    public void setPrecoTransporte(double precoTransporte) {
        this.precoTransporte = precoTransporte;
    }
    
    public void setDataEncomenda(Date dataEncomenda) {
        this.dataEncomenda = dataEncomenda;
    }
    
    public void setDataEntrega(Date dataEntrega) {
        this.dataEntrega = dataEntrega;
    }
    
    public boolean equals(Object obj) {
        if(this == obj) return true;
        if((obj == null) || (this.getClass() != obj.getClass())) return false;
        
        Encomenda e = (Encomenda) obj;
        return this.codigoEncomenda.equals(e.getCodigoEncomenda()) &&
               this.codigoUtilizador.equals(e.getCodigoUtilizador()) &&
               this.codigoLoja.equals(e.getCodigoLoja()) &&
               this.codigoTransportador.equals(e.getCodigoTransportador()) &&
               this.peso == e.getPeso() &&
               this.produtos.equals(e.getProdutos()) &&
               this.estadoEncomenda == e.getEstadoEncomenda() &&
               this.distancia == e.getDistancia() &&
               this.duracaoEntrega == e.getDuracaoEntrega() &&
               this.precoTransporte == e.getPrecoTransporte() &&
               this.dataEncomenda.equals(e.getDataEncomenda()) &&
               this.dataEntrega.equals(e.getDataEntrega());
    }
    
    public abstract String toString();
    
    public abstract Encomenda clone();
    
}
