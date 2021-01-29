
/**
 * Escreva a descrição da classe Loja aqui.
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611
 */

import java.util.*;
import java.time.LocalDateTime;
import java.time.LocalDate;
import java.io.Serializable;

public class Loja implements Serializable{
    private String codL;
    private String nome;
    private Coordenadas gps;
    private List<Produto> produtos;
    private List<Empresa> empresas;
    private List<Voluntario> voluntarios;
    private List<Encomenda> encomendas_feitas;
    private List<Encomenda> pedidos;
    private List<Cliente> fila;
    
    /**
     * Construtores
     */
    public Loja() {
        this.codL = "N/a";
        this.nome = "N/a";
        this.gps = new Coordenadas();
        this.produtos = new ArrayList <>();
        this.empresas = new ArrayList <>();
        this.voluntarios = new ArrayList <>();
        this.encomendas_feitas = new ArrayList <>();
        this.pedidos = new ArrayList<>();
        this.fila = new ArrayList<Cliente>();
    }
    
    public Loja(String codL, String nome, Coordenadas gps, ArrayList<Produto> produtos, List<Empresa> empresas, List<Voluntario> voluntarios, List<Encomenda> enc_feitas, List<Encomenda> pedidos, List<Cliente> fila){
        this.codL = codL;
        this.nome = nome;
        this.gps = gps;
        this.produtos = produtos;
        this.empresas = empresas;
        this.voluntarios = voluntarios;
        this.encomendas_feitas = enc_feitas;
        this.pedidos = pedidos;
        this.fila = fila;
    }
    
    public Loja(Loja loja) {
        this.codL = loja.getCodL();
        this.nome = loja.getNome();
        this.gps = loja.getGps();
        this.produtos = loja.getProdutos();
        this.empresas = loja.getEmpresas();
        this.voluntarios = loja.getVoluntarios();
        this.encomendas_feitas = loja.getEncFeitas();
        this.pedidos = loja.getPedidos();
        this.fila = loja.getFila();
    }

    /**
     * Get's
     */
    public String getCodL() {
        return this.codL;
    }

    public String getNome() {
        return this.nome;
    }

    public Coordenadas getGps() {
        return this.gps.clone();
    }
    
    public List <Produto> getProdutos(){
        ArrayList<Produto> copia = new ArrayList<>();
        for(Produto e: this.produtos){
            copia.add(e);
        }
        return copia;
    }
    
    public List <Empresa> getEmpresas(){
        ArrayList<Empresa> copia = new ArrayList<>();
        for(Empresa e: this.empresas){
            copia.add(e);
        }
        return copia;
    }
    
    public List<Voluntario> getVoluntarios(){
        ArrayList<Voluntario> copia = new ArrayList<>();
        for(Voluntario e: this.voluntarios){
            copia.add(e);
        }
        return copia;
    }
    
    public List <Encomenda> getEncFeitas(){
        ArrayList<Encomenda> copia = new ArrayList<>();
        for(Encomenda e: this.encomendas_feitas){
            copia.add(e);
        }
        return copia;
    }

    public List <Encomenda> getPedidos(){
        ArrayList<Encomenda> copia = new ArrayList<>();
        for(Encomenda e: this.pedidos){
            copia.add(e);
        }
        return copia;
    }
    
    public List <Cliente> getFila(){
        ArrayList<Cliente> copia = new ArrayList<>();
        for(Cliente c: this.fila){
            copia.add(c);
        }
        return copia;
    }

    /**
     * Set's
     */
    public void setCodL(String codL) {
        this.codL = codL;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setGps(Coordenadas gps) {
        this.gps = gps;
    }

    public void setProdutos(List<Produto> produtos){
        this.produtos = produtos;
    }
    
    public void setEmpresas(List<Empresa> empresas){
        this.empresas = empresas;
    }
    
    public void setVoluntarios(List<Voluntario> voluntarios){
        this.voluntarios = voluntarios;
    }
    
    public void setFila(List<Cliente> fila){
        this.fila = fila;
    }
    
    /**
     * clone
     */
    public Loja clone(){
        return new Loja(this);
    }
    
    /**
     * equals
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Loja l = (Loja) obj;
        return l.getCodL().equals(this.codL);
    }
    
    /**
     * toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código de Utilizador: ").append(this.codL).append("\n");
        sb.append("Nome: ").append(this.nome).append("\n");
        sb.append("GPS: ").append(this.gps).append("\n");
        sb.append("Produtos: ").append(this.produtos).append("\n");
        sb.append("Empresas: ").append(this.empresas).append("\n");
        sb.append("Voluntarios: ").append(this.voluntarios).append("\n");
        sb.append("Fila de clientes: ").append(this.fila).append("\n");
        return sb.toString();
    }
    
    /**
     * Obtem informação das encomendas feitas à loja, num determinado periodo de tempo
     */
    public ArrayList<Encomenda> obteminfoencomendas(LocalDateTime hora1, LocalDateTime hora2){
        ArrayList<Encomenda> encomendas = new ArrayList<>();
        for (Encomenda e: this.encomendas_feitas){
            if (e.getHora().isAfter(hora1) && e.getHora().isBefore(hora2)){
                encomendas.add(e);
            }
        }
        return encomendas;
    }
    
    /**
     * Adiciona uma encomenda à lista de encomendas feitas
     */
    public void adicionaencomenda(Encomenda encomenda){
        this.encomendas_feitas.add(encomenda);
    }
    
    /**
     * Calcula o preco total da encomenda
     */
    public double calcula_preco(){
        double total = 0;
        for(Produto p: this.produtos){
            total += p.calculaPreco();
        }
        return total;
    }
    
    /**
     * 
     */
    public int compareTo(Loja x){
        if (this.nome.equals(x.getNome()))
            return this.nome.compareTo(x.getNome());
        return this.codL.compareTo(x.getCodL());  
    }
    
    /**
     * Adiciona um pedido à encomenda
     */
    public void adicionaPedido(Encomenda encomenda){
        this.pedidos.add(encomenda);
    }
    
    /**
     * Calcula o tamanho da fila da loja
     */
    public int tamanho_fila(){
        return this.fila.size();
    }
    
    /**
     * Adiciona um cliente à fila
     */
    public void adiciona_fila(Cliente cliente){
        this.fila.add(cliente);
    }
    
    /**
     * Adiciona um produto à loja
     */
    public void adicionaProduto(Produto p){
        this.produtos.add(p);
    }
    
    /**
     * Método que calcula o preço total que a empresa irá cobrar da loja à empresa
     */
    public double precoTransporteLojaEmpresa(Empresa t){
        double total = 0;
        total = this.getGps().distancia(t.getGps()) * t.getPrecoKm();
        return total;
     }
}
