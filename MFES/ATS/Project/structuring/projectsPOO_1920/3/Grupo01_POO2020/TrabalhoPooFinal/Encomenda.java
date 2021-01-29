import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.stream.Collectors;
import java.io.Serializable;
/**
 * descrição da classe Encomenda.
 */
public class Encomenda implements Serializable{
    private String codEncomenda;
    private String emailUtilizador;
    private String emailLoja;
    /**peso da encomenda */
    private double peso;
    /** linha de encomenda */
    private List<LinhaEncomenda> linhas;
    
    /**
     * Construtor de encomenda sem parametros
     */
    public Encomenda(){
        this.codEncomenda = null;
        this.emailUtilizador = null;
        this.emailLoja = null;
        this.peso = 0.0;
        this.linhas= new ArrayList<>();
    }
    
    /**
     * Construtor parametrizado
     */
    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso,
    List<LinhaEncomenda> linhas){
        this.codEncomenda = codEncomenda;
        this.emailUtilizador = codUtilizador;
        this.emailLoja = codLoja;
        this.peso = peso;
        setEncomendas(linhas);
    }
    
    /**
     * Construtor de copia
     */
    public Encomenda(Encomenda e){
        this.codEncomenda = e.getCodEncomenda();
        this.emailUtilizador = e.getEmailUtilizador();
        this.emailLoja = e.getEmailLoja();
        this.peso = e.getPeso();
        this.linhas = e.getLinhas();
    }
    
    /**
     * Get do codigo da encomenda 
     */
    public String getCodEncomenda(){
        return codEncomenda;
    }
    
    
    /**
     * Get do codigo de utilizador
     */
    public String getEmailUtilizador(){
        return emailUtilizador;
    }
    
    /**
     * Get do codigo da loja
     */
    public String getEmailLoja(){
        return emailLoja;
    }
    
    /**
     * Get do peso da encomenda 
     */
    public double getPeso(){
        return peso;
    }
    
    /** 
     * Get de linhas de encomenda 
     */
    public List<LinhaEncomenda> getLinhas(){
        return this.linhas.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());
    }
    
    /**
     * Altera o codigo da encomenda 
     */
    public void setCodEncomenda(String e){
        codEncomenda = e;
    }
    
    /**
     * Altera o codigo de utilizador
     */
    public void setEmailUtilizador(String e){
        emailUtilizador = e;
    }
    
    /**
     * Altera o codigo de loja
     */
    public void setEmailLoja(String e){
        emailLoja = e;
    }
    
    /**
     * Altera o peso da encomenda 
     */
    public void setPeso(double p){
        peso = p;
    }
    
    public void setEncomendas(List<LinhaEncomenda> linhasEnc){
        this.linhas=new ArrayList<>();
        for(LinhaEncomenda le:linhasEnc){
            this.linhas.add(le.clone());
        }
    }
    
    
    /**
     * Implementaçao do metodo toString
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Código da encomenda: "+codEncomenda+"\n");
        sb.append("Tag do cliente: "+emailUtilizador+"\n");
        sb.append("Tag da loja: "+emailLoja+"\n");
        sb.append("Peso da encomenda: "+peso+"\n");
        sb.append(this.linhas.toString());
        sb.append("\n");
        return sb.toString();
    }
    
    /**
     * Metodo equals
     * Compara um objeto para ver se e uma encomenda 
     */
    public boolean equals(Object o){
        if(this == o) return true;
        if(this.getClass() != o.getClass()) return false;
        Encomenda a =(Encomenda) o;
        if(this.codEncomenda != a.getCodEncomenda()) return false;
        if(this.emailUtilizador != a.getEmailUtilizador()) return false;
        if(this.emailLoja != a.getEmailLoja()) return false;
        if(this.linhas != a.getLinhas()) return false;
        return true;
    }
    
    /** 
     * Metodo clone faz uma copia do objeto Encomenda
     */
    public Encomenda clone(){
        return new Encomenda(this);
    }
    
        
    public void addLinhaEncomenda(LinhaEncomenda linha){
        this.linhas.add(linha.clone());
        }
        
        
}    
   
   

