
/**
 * Write a description of class Localizacao here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Localizacao
{
    /** Coordenada X **/
    private double x;
    /** Coordenada Y **/
    private double y;
    
    /** Construtor nulo **/
    public Localizacao(){
        this.x = 0;
        this.y = 0;
    }
    
    /** Construtor parametrizado da classe Localizacao **/
    public Localizacao(double nx, double ny){
        this.x=nx;
        this.y=ny;
    }
    
    /** Construyor de cópia **/
    public Localizacao(Localizacao l){
        this.x=l.getX();
        this.y=l.getY();
    }
    
    /** Retorna a coordenada X da LOcalizacao **/
    public double getX(){
        return this.x;
    }
    
    /** Define a coordenada X da Localizacao **/
    public void setX(double nx){
        this.x=nx;
    }
    
    /** Retorna a coordenada Y da Localizacao **/
    public double getY(){
        return this.y;
    }
    
    /** Define a coordenada de Y da Localizacao **/
    public void setY(double ny){
        this.y=ny;
    }
    
    /** Método que clona uma Localizacao **/
    public Localizacao clone(){
        return new Localizacao(this);
    }
    
    /** Método que devolve um boolean true caso as Localizacoes sejam iguais e false caso não sejam **/
     
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        Localizacao l = (Localizacao) o;
        return this.x == l.getX() && this.y == l.getY();
    }
    
    /** Método que cria uma string com a informação da Localizacao **/
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("(X: ").append(this.x+",");
        sb.append("Y: ").append(this.y+")");
        return sb.toString();
    }
     public double distancia(Localizacao l, Localizacao l2){
        return Math.sqrt( Math.pow(l.getX()-l2.getX(),2) + Math.pow(l.getY()-l2.getY(),2));
}
}
