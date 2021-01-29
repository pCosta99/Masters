
/**
 * Escreva a descrição da classe Pedidos aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Pedidos
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private String codenc,codtransp;

    /**
     * COnstrutor para objetos da classe Pedidos
     */
    public Pedidos()
    {
        this.codenc="";
        this.codtransp="";
    }

    public Pedidos(String enc,String trans ){
      this.codenc=enc;
        this.codtransp=trans;

    }
    
    public Pedidos(Pedidos p){
        this.codenc=p.getCodenc();
        this.codtransp=p.getCodtransp();
}

   public String getCodenc(){
       return this.codenc;
    }
     public String getCodtransp(){
       return this.codtransp;
    }
    
    public void setCodenc(String enc){
        this.codenc=enc;
    }
    public void setCodtransp(String transp){
        this.codtransp=transp;
    }
    
    public boolean equals(Object o){
        if(this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        
        Pedidos umPedido = (Pedidos) o;
        return (this.codtransp.equals(umPedido.getCodenc()) && this.codenc.equals(umPedido.getCodtransp()));
    } 
    
     public String toString() {
            StringBuilder sb = new StringBuilder();
      sb.append(this.codtransp);
      sb.append(","+this.codenc); 
      
      return sb.toString();
         }
    public Pedidos clone(){
        return new Pedidos(this);
        }
}

