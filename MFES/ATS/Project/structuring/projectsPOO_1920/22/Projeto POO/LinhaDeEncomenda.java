
/**
 * Escreva a descrição da classe LinhadeEncomenda aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class LinhaDeEncomenda
{
    
    private double valorunitario,quantidade;
    private String codproduto,descricao;
    
    public LinhaDeEncomenda(){
       this.descricao=""; 
       this.codproduto="";
       this.quantidade=0;
       this.valorunitario=0;
    }
       
   public LinhaDeEncomenda(String desc,double quant,String codprod,double preco ){
       this.descricao=desc; 
       this.codproduto=codprod;
       this.quantidade=quant;
       this.valorunitario=preco;
       
    }
    
    //copy
    public LinhaDeEncomenda(LinhaDeEncomenda linha) {
       this.descricao=linha.getDescricao(); 
       this.valorunitario=linha.getPreco();
       this.quantidade=linha.getQuantidade();
       this.codproduto=linha.getCod();
      }
      
      //retorna variaveis dentro da classe
      public String getDescricao() {
        return this.descricao;
      }
      public double getPreco() {
        return this.valorunitario;
      }
      public double getQuantidade() {
        return this.quantidade;
      }
      public String getCod() {
        return this.codproduto;
      }
       
      //recebe uma variavel e poe na classe
      
       
        public void setDescricao(String novoDescricao) {
          this.descricao = novoDescricao;
        }
        public void setPreco(double novoPreco) {
          this.valorunitario = novoPreco;
        }
        
        public void setQuant(double novoQuant) {
          this.quantidade = novoQuant;
        }
        public void setCod(String novoCod) {
          this.codproduto = novoCod;
        }
        
        public boolean equals(Object o){
        if(this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        
        LinhaDeEncomenda umLinha = (LinhaDeEncomenda) o;
        return (this.codproduto.equals(umLinha.getCod()) && this.descricao.equals(umLinha.getDescricao()));
    } 
        
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(this.codproduto);
            sb.append(","+this.descricao); 
      sb.append(","+this.quantidade);
      sb.append(","+this.valorunitario);
      return sb.toString();          
         } 
         
        public LinhaDeEncomenda clone(){
        return new LinhaDeEncomenda(this);
        }
        
      public double calculaValorLinhaEnc(){
        double valor = this.quantidade * this.valorunitario;
        return valor;
    }
        
}
