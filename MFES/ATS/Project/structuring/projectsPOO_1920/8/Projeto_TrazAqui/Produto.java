import java.io.Serializable;

public class Produto implements Serializable
{

  // Variáveis de instância
  private String referencia;
  private String descricao;
  private double preco;
  private double quantidade;

     /**
     * Construtor parametrizado da classe produto.
     * @param ref
     * @param des
     * @param pre
     * @param quan
     * @return
     */

     private Produto(){ }

    public Produto(String ref,String des,double pre,double quan)
    {
      this.referencia = ref;
      this.descricao = des;
      this.preco = pre;
      this.quantidade = quan;
    }
  
     /**
     * Construtor de cópia da classe produto
     * @param umproduto
     * @return
     */
      public Produto(Produto umproduto)
        {
          this.referencia = umproduto.getreferencia();
          this.descricao = umproduto.getdescricao();
          this.preco = umproduto.getpreco();    
          this.quantidade = umproduto.getquantidade();
        }
  
     /**
     * Devolve a referencia de um produto
     * @param
     * @return referencia
     */
      public String getreferencia()
      {
        return this.referencia;
    }
     
     /**
     * Atualiza a referencia de um produto
     * @param ref
     * @return
     */
    public void setreferencia(String ref)
    {
        this.referencia = ref;
    }
  
     /**
     * Devolve a descricao de um produto
     * @param
     * @return descricao
     */
      public String getdescricao()
      {
        return this.descricao;
    }
     
     /**
     * Atualiza a descricao de um produto
     * @param des
     * @return
     */
    public void setdescricao(String des)
    {
        this.descricao = des;
    }
  
     /**
     * Devolve o preco de um produto
     * @param
     * @return preco
     */
      public double getpreco()
      {
        return this.preco;
    }
     
     /**
     * Atualiza o preco de um produto
     * @param pre
     * @return
     */
    public void setpreco(double pre)
    {
        this.preco = pre;
    }
  
     /**
     * Devolve a quantidade de um produto 
     * @param
     * @return quantidade
     */
      public double getquantidade()
      {
        return this.quantidade;
    }
  
     /**
     * Atualiza a quantidade de um produto
     * @param quan
     * @return
     */
    public void setquantidade(double quan)
    {
        this.quantidade = quan;
    }

    /**
     * Metodo para calcular o preço total
     *
     */
    public double quantoCusta(){
        return this.preco * this.quantidade;
    }


    /**
     * Método que faz uma cópia da classe produto.
     * Para tal invoca o construtor de cópia.
     * @param
     * @return Produto clone da classe produto.
     */
    public Produto clone()
    {
        return new Produto(this);
    }
    
     
     /**
     * Método que devolve a representação em String da classe Produto.
     * @param
     * @return String
     */
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("Produto ---> ");
        sb.append("\tReferencia: ").append(this.getreferencia()).append("\n");
        sb.append("\tDescricao: ").append(this.getdescricao()).append("\n");
        sb.append("\tPreco: ").append(this.getpreco()).append("\n");
        sb.append("\tQuantidade: ").append(this.getquantidade()).append("\n");
        return sb.toString();
    }
  
  
     /**
     * Método que verifica se a classe Produto é igual à classe Produto atual.
     * @param o
     * @return boolean
     */
    public boolean equals(Object o)
    {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        
        Produto umproduto = (Produto) o;
        return this.getreferencia().equals(umproduto.getreferencia());
    }

    public int hashCode(){
        int hash = 5;
        long aux1, aux2;
        aux1 = Double.doubleToLongBits(this.preco);
        hash = 31*hash + (int)(aux1 ^ (aux1 >>> 32));
        aux2 = Double.doubleToLongBits(this.quantidade);
        hash = 31*hash + (int)(aux1 ^ (aux2 >>> 32));
        hash = 31 * hash + this.referencia.hashCode();
        hash = 31 * hash + this.descricao.hashCode();
        return hash;
    }
}
