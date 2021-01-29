import java.util.ArrayList;
import java.time.LocalDateTime;
import java.io.Serializable;

public class Encomendas implements Serializable{
    private String codLoja;
    private String codUtil;
    private float peso;
    private String codEnc;
    private boolean disponivel;
    private ArrayList<Produto> produto;
    private String codBusca;
    private double preco;
    private LocalDateTime tmpIni;
    private LocalDateTime tmpEntrega;
    private boolean medicamentos;

    public Encomendas() {
        this.codLoja = "n/a";
        this.codUtil = "n/a";
        this.peso = 0;
        this.codEnc = "n/a";
        this.disponivel = false;
        this.produto = new ArrayList<>();
        this.preco = 0;
        this.codBusca = "n/a";
        this.tmpIni = null;
        this.tmpEntrega = null;
        this.medicamentos = false;
    }

    public Encomendas(String codLoja, String codUtil, float peso, String codEnc, boolean disponivel, ArrayList<Produto> produto, double preco, String codBusca, LocalDateTime tmpIni, LocalDateTime tmpEntrega, boolean meds) {
        this.codLoja = codLoja;
        this.codUtil = codUtil;
        this.peso = peso;
        this.codEnc = codEnc;
        this.disponivel = disponivel;
        this.produto = produto;
        this.preco = preco;
        this.codBusca = codBusca;
        this.tmpIni = tmpIni;
        this.tmpEntrega = tmpEntrega;
        this.medicamentos = meds;
    }

    public Encomendas(Encomendas produto) {
        this.codLoja = produto.getCodLoja();
        this.codUtil = produto.getCodUtil();
        this.peso = produto.getPeso();
        this.codEnc = produto.getCodEnc();
        this.produto = produto.getProduto();
        this.preco = produto.getPreco();
        this.codBusca = produto.getCodBusca();
        this.disponivel = produto.isDisponivel();
        this.tmpIni = produto.getTmpIni();
        this.tmpEntrega = produto.getTmpEntrega();
        this.medicamentos = produto.getMedicamentos();
    }

    public float calculaValorTotal() {
        float soma = 0;
        for (Produto s : this.produto) {
            soma += s.calculaValorTotal();
        }
        return soma;
    }
    
    public float calculaPesoTotal(){
        float soma = 0;
        for (Produto p: this.produto){
            soma += p.calculaValorpeso();
        }
        return soma;
    }

    public int TotalProdutos() {
        int quant = 0;
        for (Produto s : this.produto) {
            quant += s.getQuantidade();
        }
        return quant;
    }

    public String getCodLoja() {
        return this.codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public String getCodUtil() {
        return this.codUtil;
    }

    public void setCodUtil(String codUtil) {
        this.codUtil = codUtil;
    }

    public float getPeso() {
        return this.peso;
    }

    public void setPeso(float peso) {
        this.peso = peso;
    }

    public String getCodEnc() {
        return this.codEnc;
    }

    public void setCodEnc(String codEnc) {
        this.codEnc = codEnc;
    }

    public boolean isDisponivel() {
        return disponivel;
    }

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    public String getCodBusca() {
        return codBusca;
    }

    public void setCodBusca(String codBusca) {
        this.codBusca = codBusca;
    }

    public ArrayList<Produto> getProduto() {
        return this.produto;
    }

    public void setProduto(ArrayList<Produto> produto) {
        this.produto = produto;
    }

    public double getPreco() {
        return preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public LocalDateTime getTmpIni() {
        return this.tmpIni;
    }

    public void setTmpIni(LocalDateTime tmpIni) {
        this.tmpIni = tmpIni;
    }

    public LocalDateTime getTmpEntrega() {
        return this.tmpEntrega;
    }

    public void setTmpEntrega(LocalDateTime tmpEntrega) {
        this.tmpEntrega = tmpEntrega;
    }

    public boolean getMedicamentos(){
        return this.medicamentos; 
    }

    public void setMedicamentos(boolean b){
        this.medicamentos = b;
    }

    public Encomendas clone(){
        return new Encomendas(this);
    }

    public void addProduto(Produto p){
        this.produto.add(p);
    }

    public String toString(){
        return "Produtos: " + this.produto.toString() + ", Preço Total: " + this.preco + "€";
    }
}
