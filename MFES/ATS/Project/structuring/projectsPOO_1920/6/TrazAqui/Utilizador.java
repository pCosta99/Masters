
/**
 * Classe dos Utilizadores
 * 
 * @author (Jo„o Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
import java.io.Serializable;
public class Utilizador implements Serializable
{
    // variaveis de instancia da classe Utilizador
    private String codUtilizador;
    private String nomeUtilizador;
    private double coordX;
    private double coordY;
    private String email;
    private String password;
    
    // construtor por omiss„o
    public Utilizador(){
        this.codUtilizador = "n/a";
        this.nomeUtilizador = "n/a";
        this.coordX = 0.0;
        this.coordY = 0.0;
        this.email = "n/a";
        this.password = "n/a";
    }
    
    // construtor parametrizado
    public Utilizador(String codU, String nome, double cX, double cY){
        this.codUtilizador = codU;
        this.nomeUtilizador = nome;
        this.coordX = cX;
        this.coordY = cY;
        this.email = codU+"@gmail.com";
        this.password = codU;
    }
    
    // construtor de c√≥pia
    public Utilizador(Utilizador u){
        this.codUtilizador = u.getCodU();
        this.nomeUtilizador = u.getNomeU();
        this.coordX = u.getCoordX();
        this.coordY = u.getCoordY();
        this.email = u.getCodU()+"@gmail.com";
        this.password = u.getCodU();
    }
    
    //  metodo que devolve o codigo do Utilizador (ex: u2982)
    public String getCodU(){
        return this.codUtilizador;
    }
    
    // metodo que devolve o nome do Utilizador
    public String getNomeU(){
        return this.nomeUtilizador;
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
    
    // metodo para definir o codigo do Utilizador
    public void setCodU(String codU){
        this.codUtilizador = codU;
    }
    
    // metodo para definir o nome do Utilizador
    public void setNomeU(String nome){
        this.nomeUtilizador = nome;
    }
    
    // metodo para definir a coordenada X
    public void setCoordX(double cX){
        this.coordX = cX;
    }
    
    // metodo para definir a coordenada Y
    public void setCoordY(double cY){
        this.coordY = cY;
    }
    
    // metodo que coloca toda a informa√ß√£o sobre um utilizador numa string
    public String toString(){
        StringBuffer sb = new StringBuffer();
        sb.append("Utilizador:"+this.codUtilizador+","+this.nomeUtilizador+","+this.coordX+","+this.coordY);
        return sb.toString();
    }
    
    // metodo de copia de um utilizador
    public Utilizador clone(){
        return new Utilizador(this);
    }
    
    // metodo que compara se dois utilizadores sao iguais
    public boolean equals(Object o){
        if (o==this) return true;
        if ((o.getClass()!=this.getClass())||o==null) return false;
        Utilizador u = (Utilizador) o;
        return this.codUtilizador.equals(u.getCodU()) &&
               this.nomeUtilizador.equals(u.getNomeU()) &&
               this.coordX == u.getCoordX() &&
               this.coordY == u.getCoordY();
    }
}
