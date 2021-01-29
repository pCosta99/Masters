    import java.util.*;
    import java.io.*;
/**
 * classe Produto ou Linha Encomenda.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Produto extends Dados implements Serializable{
    private String codProduto;
    private String nomeProduto;
    private int quantidade;
    private double preco;
    private double totalFaturado;
    private double classificacao;
    private List<Encomenda> encomendas;
    private boolean disponibilidadeLoja;
        
    /**
     * Construtor para objetos da classe Produto (por omissao)
     */
    public Produto(){
        this.codProduto = "n/a";
        this.nomeProduto = "n/a";
        this.preco = 0;
        this.totalFaturado = 0;
        this.classificacao = 0;
        this.quantidade = 0;
        this.encomendas = new ArrayList<>();
        this.disponibilidadeLoja = true;
    }
        
    /**
     * Construtor para objetos da classe Veiculo (por parametrizacao)
     * 
     * @param  c   o codigo do produto
     * @param  n   o nome do produto
     * @param  p   o preco por unidade
     * @param  tf  o total faturado
     * @param  cla   a classificacao
     * @param  q   a quantidade encomendada
     * @param  d   a disponibilidade na loja
     */
        
    public Produto(String c, String n,double p, double tf, double cla, int q, List<Encomenda> en,boolean d){ 
        this.setCodProduto(c);
        this.setNomeProduto(n);
        this.setPreco(p);
        this.setTotalFaturado(tf);
        this.setClassificacaoProduto(cla);
        this.setQuantidade(q);
        this.setEncomendas(en);
        this.setDisponibilidadeLoja(d);
    }
        
    /**
     * Construtor para objetos da classe Produto (por copia)
     * 
     * @param  p   o produto a copiar
     */
    public Produto(Produto p){
        this.codProduto = p.getCodProduto();
        this.nomeProduto = p.getNomeProduto();
        this.preco = p.getPreco();
        this.totalFaturado = p.getTotalFaturado();
        this.classificacao = p.getClassificacaoProduto();
        this.quantidade = p.getQuantidade();
        this.encomendas = p.getEncomendas();
        this.disponibilidadeLoja = p.getDisponibilidadeLoja();
    }
        
    /*getters*/
    public String getCodProduto(){return this.codProduto;}
    public String getNomeProduto(){return this.nomeProduto;}
    public double getPreco(){return this.preco;}
    public double getTotalFaturado(){return this.totalFaturado;}
    public double getClassificacaoProduto(){return this.classificacao;}
    public int getQuantidade(){return this.quantidade;}    
    public List<Encomenda> getEncomendas(){
         List<Encomenda> aux = new ArrayList<>();
         for(Encomenda e : this.encomendas)
            aux.add(e.clone());
         return aux;
    }
    public boolean getDisponibilidadeLoja(){return this.disponibilidadeLoja;} 
    
    /*setters*/
    public void setCodProduto(String c){this.codProduto = c;}
    public void setNomeProduto(String n){this.nomeProduto = n;}
    public void setPreco(double p){ this.preco = p;}
    public void setTotalFaturado(double tf){this.totalFaturado = tf;}
    public void setClassificacaoProduto(double cla){this.classificacao = cla;}
    public void setQuantidade(int q){this.quantidade = q;}
    public void setEncomendas(List<Encomenda> en){
         this.encomendas = new ArrayList<>();
         for(Encomenda e : en)
            this.encomendas.add(e.clone());
    }
    
    public void setDisponibilidadeLoja(boolean d){ this.disponibilidadeLoja = d;} 
    
    /**
     * Metodo que insere uma encomenda na lista de encomendas
     * 
     * @param  e   encomenda a ser inserida
     */
    public void insereEncomenda(Encomenda e){
         this.encomendas.add(e.clone());
    }    
        
    /**
     * Metodo que altera a classificacao
     */
    public void alteraClassificacaoProd(double cla){
        double classi = this.classificacao * this.encomendas.size();
        classi = (classi + cla)/(this.encomendas.size() + 1);
        setClassificacaoProduto(classi);
    }
    /**
     * Metodo que duplica o produto
     * 
     * @return     o clone do produto
     */
    public Produto clone(){return new Produto(this);}
    
    /**
     * Metodo que verifica se dois produtos sao iguais
     * 
     * @param  o   o objeto a comparar
     * 
     * @return     o resultado da comparacao dos objetos
     */
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;
        
        Produto p = (Produto) o;
        return this.codProduto.equals(p.getCodProduto()) && 
               this.nomeProduto.equals(p.getNomeProduto()) && 
               this.preco == (p.getPreco()) && 
               this.totalFaturado == (p.getTotalFaturado()) &&
               this.classificacao == (p.getClassificacaoProduto()) &&
               this.quantidade == (p.getQuantidade()) &&
               this.disponibilidadeLoja == (p.getDisponibilidadeLoja()) && 
               this.encomendas.equals(p.getEncomendas());
    }
    
    /**
     * Metodo que converte um produto para uma string
     * 
     * @return     o produto em string
     */
    public String toString(){
        String aux = "Codigo do Produto: " + this.codProduto + ";\n"
                     + "Nome do Produto: " + this.nomeProduto + ";\n"
                     + "Preço do Produto: " + this.preco + ";\n"
                     + "Faturação Total da Encomenda: " + this.totalFaturado + ";\n"
                     + "Classificação do Produto: " + this.classificacao + ";\n"
                     + "Quantidade encomendada: " + this.quantidade + ";\n"
                     + "Diponibilidade: " + this.disponibilidadeLoja + ";\n"
                     
                     + "Encomenda: \n";
        for (Encomenda e : this.encomendas)
            aux += e.toString() + "\n";
        return aux;            
    }
    
    /**
     * Metodo que devolve o codigo de hash para um produto
     * 
     * @return     o hashcode
     */
    public int hashCode(){
        int hash = 7;
        hash = 31 * hash + codProduto.hashCode();
        hash = 31 * hash + nomeProduto.hashCode();
        long aux1 = Double.doubleToLongBits(preco);
        hash = 31 * hash + (int)(aux1 ^ (aux1 >>> 32));
        long aux2 = Double.doubleToLongBits(totalFaturado);
        hash = 31 * hash + (int)(aux2 ^ (aux2 >>> 32));
        long aux3 = Double.doubleToLongBits(classificacao);
        hash = 31 * hash + (int)(aux3 ^ (aux3 >>> 32));
        for(Encomenda e : this.encomendas)
            hash = 31 * hash + e.hashCode();
        return hash;
    }
    
     
}
