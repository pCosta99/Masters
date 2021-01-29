
/**
 * Escreva a descrição da classe EncomendasAceites aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class EncomendasAceites
{
    private String codencomenda,quem;
    public EncomendasAceites()
    {
      this.codencomenda="";
      this.quem="";
    }
    
    public EncomendasAceites(String cod,String es)
    {
      this.codencomenda=cod;
      this.quem=es;
    }
    
    public EncomendasAceites(EncomendasAceites ea)
    {
      this.codencomenda=ea.getEA();
      this.quem=ea.getQ();
    }
    
    public String getEA(){
        return this.codencomenda;
    }
    public String getQ(){
        return this.quem;
    }
    public void setEA(String novocod){
        this.codencomenda=novocod;
    }
    public void setQ(String novoq){
        this.quem=novoq;
    }
    public boolean equals(Object o){
        if(this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        
        EncomendasAceites umAceite = (EncomendasAceites) o;
        return (this.codencomenda.equals(umAceite.getEA()));
    }
    public String toString() {
              StringBuilder sb = new StringBuilder();
      sb.append("Aceite:");
      sb.append(this.codencomenda);
      sb.append(","+this.quem);
      return sb.toString();
         } 

    public EncomendasAceites clone(){
        return new EncomendasAceites(this);
        }
    
}
