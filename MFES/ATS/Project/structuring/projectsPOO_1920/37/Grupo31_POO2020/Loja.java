

/**
 * Escreva a descrição da classe Lojas aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.io.*;
public class Loja extends Join implements Serializable{
    
    /**
     * 
     * variaveis de instância
     */
    
    private String codLoja; 
    private String nomeLoja;
   
   
    /**
     * construtores
     */
    public Loja(){
        super();
        this.codLoja="";
        this.nomeLoja="";
        
    }
    
    public Loja(Localizacao localizacao,String codLoja,String nomeLoja,String user, String pass){
        super(localizacao, user, pass);
        this.codLoja=codLoja;
        this.nomeLoja=nomeLoja;
        
    }
    
    public Loja(Loja loja){
        super(loja.getLocalizacao(),loja.getUser(), loja.getPass());
        this.codLoja = loja.getCodLoja();
        this.nomeLoja = loja.getNomeLoja();
    }
    
    /**
     * Gets
     */
    public String getCodLoja(){
        return this.codLoja;
    }
   
    public String getNomeLoja(){
        return this.nomeLoja;
    }
    
    
    
    
    /**
     * Sets
     *
     */
    public void getCodLoja(String codLoja){
        this.codLoja =codLoja;
    }
   
    public void getNomeLoja(String nomeLoja){
        this.nomeLoja = nomeLoja;
    }
    
    /**
     * Método clone
     */
    
    public Loja clone(){
        return new Loja(this);
    }
    
    /**
     * Método equals
     */
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Loja lo = (Loja) obj;
        return (super.equals(lo) && 
                lo.getCodLoja().equals(this.codLoja) &&
                lo.getNomeLoja().equals(this.nomeLoja));
    }
    
    
    /**
     * Método toString
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append ("\nNome da Loja: ").append (this.nomeLoja);
        sb.append ("\nCódigo da Loja: ").append (this.codLoja);
        sb.append ("\nUser da Loja: ").append (this.getUser());
        sb.append ("\nPassword da Loja: ").append (this.getPass());
        sb.append ("\nLocalização da Loja: ").append (this.getLocalizacao());
        return sb.toString();
    }
    
    /**
     * Metodo para esconder o user e a password quando escolhemos uma loja
     */
    public String toStringMaisBonito(){
        StringBuilder sb = new StringBuilder();
        sb.append ("\nNome da Loja: ").append (this.nomeLoja);
        sb.append ("\nCódigo da Loja: ").append (this.codLoja);
        return sb.toString();
    }
}
