import java.io.Serializable;
public class EncomendasAceites implements Serializable
{
    // variáveis de instância 
    private String codigo;

    /**
     * Construtor vazio de EncomendasAceites.
     */
    public EncomendasAceites() {
        this.codigo = new String();
    }
    
    /**
     * Construtor parametrizado de EncomendasAceites.
     */
    public EncomendasAceites(String codEncomenda)
    {
        this.setCodigo(codEncomenda);
    }

    /**
     * Construtor de copia de EncomendasAceites.
     */
    public EncomendasAceites(EncomendasAceites e) {
        this.setCodigo(e.getCodigo()); 
    }
    
    // getters
    public String getCodigo(){
        return this.toString();
    }
    
    // setters
    public void setCodigo(String codEncomenda){
        this.codigo = codEncomenda;
    }
    
    /**
     * Metodo que faz uma copia do objecto receptor da mensagem.
     * Para tal invoca o construtor de copia.
     */
    public EncomendasAceites clone(){
        return new EncomendasAceites(this);
    }
    
    /**
     *  Metodo que devolve a representaçao em String da classe EncomendasAceites.
     */
    public String toString() {
        return this.codigo;
    }
    
    /**
     * Método que representa em ficheiro CSV as EncomendasAceites.
     */
    public String toStringCSV(){
      StringBuilder sb = new StringBuilder();
      sb.append("Aceite:");
      sb.append(this.codigo);
      return sb.toString();
    }
    
    /**
     * Metodo que determina se duas encomendas aecites sao iguais.
     */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;      
        EncomendasAceites e = (EncomendasAceites) obj;
        return this.codigo.equals(e.getCodigo());
    }
}
