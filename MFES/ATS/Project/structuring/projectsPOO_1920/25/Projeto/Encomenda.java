
/**
 * Escreva a descrição da classe Encomenda aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.time.LocalDateTime;
import java.io.Serializable;
public class Encomenda implements Serializable
{
    private String codEncomenda;
    private String destinatario;
    private String vendedor;
    private double peso;
    private List<LinhaEncomenda> lE;
    private LocalDateTime data;
    private boolean medica;
    
    /**
     * Construtor por omissao de Encomenda.
     */
    public Encomenda() {
        this.codEncomenda = new String();
        this.peso = 0;
        this.destinatario = new String();
        this.vendedor = new String();
        this.lE = new ArrayList<LinhaEncomenda>();
        this.data = LocalDateTime.now();
        this.medica = false;
    }
    
    /**
     * Construtor parametrizado de Encomenda.
     */
    public Encomenda(String codEncomenda, String destinatario, String vendedor, double peso,  LocalDateTime data, boolean med,
                     List<LinhaEncomenda> novole)
    {
        this.setCodEncomenda(codEncomenda);
        this.setPeso(peso);
        this.setDestinatario(destinatario);
        this.setVendedor(vendedor);
        this.setLE(novole);
        this.setData(data);
        this.setMedica(med);
    }
    
    /**
     * Construtor de copia de Encomenda.
     */
    public Encomenda(Encomenda e) {
        this.setCodEncomenda(e.getCodEncomenda());
        this.setPeso(e.getPeso());
        this.setDestinatario(e.getDestinatario());
        this.setVendedor(e.getVendedor());
        this.setLE(e.getLE());
        this.setData(e.getData());
        this.setMedica(e.getMedica());  
    }
    
    // getters
    
    public String getCodEncomenda(){
        return this.codEncomenda;
    }

    public String getDestinatario(){
       return this.destinatario;
    }

    public String getVendedor(){
        return this.vendedor;
    }
    
    public double getPeso(){
        return this.peso;
    }

    public List<LinhaEncomenda> getLE(){
        ArrayList<LinhaEncomenda> novo = new ArrayList<LinhaEncomenda>();
        Iterator<LinhaEncomenda> iter = this.lE.iterator(); 
        while (iter.hasNext()){
                LinhaEncomenda elem = iter.next();
                novo.add(elem.clone());
        }
        return novo;
    }
    
    public LocalDateTime getData(){
        return this.data;
    }
    
    public boolean getMedica(){
        return this.medica;
    }
    
    // setters
    
    public void setCodEncomenda(String codEncomenda){
        this.codEncomenda = codEncomenda;
    }

    public void setDestinatario(String destinatario){
       this.destinatario = destinatario;
    }

    public void setVendedor(String vendedor){
        this.vendedor = vendedor;
    }
    
    public void setPeso(double novoPeso){
        this.peso = novoPeso;
    }

    public void setLE(List<LinhaEncomenda> novole){
        List<LinhaEncomenda> novo = new ArrayList<LinhaEncomenda>();
        for (LinhaEncomenda encomenda : novole){
            novo.add(encomenda.clone());
        }
        this.lE = novo;
    }
    
    public void setData(LocalDateTime data){
        this.data = data;
    }
    
    public void setMedica(boolean med){
        this.medica = med;
    }
    
    /**
     * Metodo que faz uma copia do objecto receptor da mensagem.
     * Para tal invoca o construtor de copia.
     */
    public Encomenda clone(){
        return new Encomenda(this);
    }
    
    /**
     *  Metodo que devolve a representaçao em String da classe Encomenda.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\ncodEncomenda:     " + this.codEncomenda + "\n");
        sb.append("destinatario:     " + this.destinatario + "\n");
        sb.append("vendedor:     " + this.vendedor + "\n");
        sb.append("Peso:     " + this.peso + "\n");
        sb.append("Data:     " + this.data + "\n");
        sb.append("É Medica: ").append(this.medica).append("\n");
        sb.append(this.lE).append("\n");
        sb.append("\n");
        return sb.toString();
    }
    
    /**
     * Metodo que determina se duas encomendas sao iguais.
     */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;      
        Encomenda e = (Encomenda) obj;
        return (this.codEncomenda.equals(e.getCodEncomenda()) &&
                this.peso == e.getPeso() && this.medica == e.getMedica()
              &&this.destinatario.equals(e.getDestinatario()) && this.vendedor.equals(e.getVendedor())
              &&this.data.equals(e.getData()) && this.lE.equals(e.getLE()));
    }
    
    /**
     * Método que representa em ficheiro CSV a informação de uma encomenda.
     */
    public String toStringCSV(){
      StringBuilder sb = new StringBuilder();
      sb.append("Encomenda:");
      sb.append(this.codEncomenda).append(",").append(this.destinatario).append(",");
      sb.append(this.vendedor).append(",").append(this.peso);
      for (LinhaEncomenda e: this.lE){
          sb.append(",").append(e.getCP()).append(",").append(e.getDP()).append(",").append(e.getQE()).append(",").append(e.getPP());
      } 
      return sb.toString();
    }
    
    /**
     * Método que determina o valor total da encomenda.
     */
    public double precoProdutos(){
        double valor = 0;
        for (LinhaEncomenda encomenda : this.lE) {   // percorrer a linha de encomenda
            valor += encomenda.calculaValorLinhaEnc();
        }
        return valor;
    }
}