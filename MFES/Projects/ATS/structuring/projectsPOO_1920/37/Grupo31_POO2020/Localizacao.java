
/**
 * Escreva a descrição da classe Localizacao aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.io.*;
public class Localizacao implements Serializable{
    /**Coordenadas de longitude e latitude */
    private double x;
    private double y;
    
    /** Construtor vazio */
    public Localizacao(){
        this.x = 0;
        this.y = 0;
    }
    
    
    /** Construtor Parametrizado */
    public Localizacao(double x, double y){
        this.x = x;
        this.y = y;
    }
    
    /** Construtor clone */
    public Localizacao(Localizacao localizacao){
        this.x = localizacao.getX();
        this.y = localizacao.getY();
    }
    
    /** Gets*/
    public double getX(){
        return this.x;
    }
    
    
    public double getY(){
        return this.y;
    }
    
    /** Sets */
    
    /** Actualiza o valor da coordenada em x para novoX */ 
    
    public void setX(double novoX){
        this.x = novoX;
    }
    
    /** Actualiza o valor da coordenada em y para novoY */ 
    
    public void setY(double  novoY){
        this.y = novoY;
    }
    
    
    /** Método Equals */
    
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Localizacao that = (Localizacao) obj;
        return that.getX() == (this.x) &&
               that.getY()==(this.y) ;
    }
    
    
    /** Método clone() */
    
    public Localizacao clone(){
        return new Localizacao(this);
    }
    
    
    /** Método toString() */
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Longitude(X): ").append (this.x);
        sb.append("; Latitude(Y): ").append(this.y);
        
        
        return sb.toString();
    } 
}
