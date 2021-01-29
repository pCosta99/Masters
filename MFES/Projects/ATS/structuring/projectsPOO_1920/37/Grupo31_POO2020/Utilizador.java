 
/**
 * Escreva a descrição da classe Utilizadores aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.io.*;
public class Utilizador extends Join implements Serializable{
    /** Variáveis de instância*/
    
    /** Codigo de utilizador */
    private String codU;
    /** Nome do Utilizador */
    private String nomeUt;
    
    
    
    /** Construtor vazio */
    public Utilizador(){
        super();
        this.codU="";
        this.nomeUt = "";
    }
    
    /** Construtor parametrizado*/
    public Utilizador(String user,String pass,String codU,Localizacao localizacao,String nomeUt){
        super(localizacao,user,pass);
        this.codU=codU;
        this.nomeUt = nomeUt;
    }
    
    /** Construtor clone */
    public Utilizador(Utilizador utilizador){
        super(utilizador.getLocalizacao(),utilizador.getUser(), utilizador.getPass());
        
        this.codU = utilizador.getCodU();
        this.nomeUt = utilizador.getNomeUt();   
    }
    
    
    /** Gets */
    
    
    public String getCodU(){
        return this.codU;
    }
    
    public String getNomeUt(){
        return this.nomeUt;
    }
    
    
    /** Sets */
    
    
    public void setCodU(String codU){
        this.codU = codU;
    }
    
    public void setNomeUt(String nomeUt){
        this.nomeUt = nomeUt;
    }
    
    //Método Equals
    
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Utilizador that = (Utilizador) obj;
        return (super.equals(that) &&
               that.getCodU().equals(this.codU) &&
               that.getNomeUt() == this.nomeUt);
    }
    
    //Método clone()
    
    public Utilizador clone(){
        return new Utilizador(this);
    }
    
    
    //Método toString()
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Nome do Utilizador: ").append (this.nomeUt);
        sb.append("\nUser: ").append(this.getUser());
        sb.append("\nPass: ").append (this.getPass());
        sb.append("\nCódigo do Utilizador: ").append (this.codU);
        sb.append("\nLocalização: ").append (this.getLocalizacao());
        
        return sb.toString();
    } 
}
