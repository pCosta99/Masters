
/**
 * Classe das Lojas
 * 
 * @author (Jo„o Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
import java.util.Map;
import java.util.HashMap;
import java.time.LocalDateTime;
import java.util.Iterator;
import java.io.Serializable;
public class Loja implements Serializable
{
    // vari·veis de inst‚ncia da classe Loja
    private String codLoja;
    private String nomeLoja;
    private double coordX;
    private double coordY;
    private String email;
    private String password;
    private LocalDateTime tempoEspera;
    
    // construtor por omiss„o
    public Loja(){
        this.codLoja = "n/a";
        this.nomeLoja = "n/a";
        this.coordX = 0.0;
        this.coordY = 0.0;
        this.email = "n/a";
        this.password = "n/a";
    }
    
    // construtor parametrizado
    public Loja(String codL, String nome, double cX, double cY){
        this.codLoja = codL;
        this.nomeLoja = nome;
        this.coordX = cX;
        this.coordY = cY;
        this.email = codL+"@loja.com";
        this.password = codL;
        //this.tempoEspera = ;
    }
    
    // construtor de c√≥pia
    public Loja(Loja l){
        this.codLoja = l.getCodL();
        this.nomeLoja = l.getNomeL();
        this.coordX = l.getCoordX();
        this.coordY = l.getCoordY();
        this.email = l.getCodL()+"@loja.com";
        this.password = l.getCodL();
        this.tempoEspera = l.getTempoEspera();
    }
    
    //  metodo que devolve o codigo da Loja (ex: l123)
    public String getCodL(){
        return this.codLoja;
    }
    
    // metodo que devolve o nome da Loja
    public String getNomeL(){
        return this.nomeLoja;
    }
    
    // metodo que devolve a coordenada X
    public double getCoordX(){
        return this.coordX;
    }
    
    // metodo que devolve a coordenada Y
    public double getCoordY(){
        return this.coordY;
    }
    
    //metodo que devolve o email
    public String getEmail(){
        return this.email;
    }
    
    //metodo que devolve a password
    public String getPassword(){
        return this.password;
    }
    
    //metodo que devolve tempo de espera por pessoa
    public LocalDateTime getTempoEspera(){
        return this.tempoEspera;
    }
    
    // metodo para definir o codigo da Loja
    public void setCodL(String codL){
        this.codLoja = codL;
    }
    
    // metodo para definir o nome da Loja
    public void setNomeL(String nome){
        this.nomeLoja = nome;
    }
    
    // metodo para definir a coordenada X
    public void setCoordX(double cX){
        this.coordX = cX;
    }
    
    // metodo para definir a coordenada Y
    public void setCoordY(double cY){
        this.coordY = cY;
    }
    
    // metodo para definir o tempo de espera
    public void setTempoEspera(LocalDateTime tempo){
        this.tempoEspera = tempo;
    }
    
    // metodo que coloca toda a informa√ß√£o sobre uma loja numa string
    public String toString(){
        StringBuffer sb = new StringBuffer();
        sb.append("Loja:"+this.codLoja+","+this.nomeLoja+","+this.coordX+","+this.coordY);
        return sb.toString();
    }
    
    // metodo de copia de uma loja
    public Loja clone(){
        return new Loja(this);
    }
    
    // metodo que compara se duas lojas sao iguais
    public boolean equals(Object o){
        if (o==this) return true;
        if ((o.getClass()!=this.getClass())||o==null) return false;
        Loja l = (Loja) o;
        return this.codLoja.equals(l.getCodL()) &&
               this.nomeLoja.equals(l.getNomeL()) &&
               this.coordX == l.getCoordX() &&
               this.coordY == l.getCoordY();
    }
}
