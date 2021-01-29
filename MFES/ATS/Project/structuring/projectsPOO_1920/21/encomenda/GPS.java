import java.lang.Math;
/**
 * Escreva a descrição da classe GPS aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class GPS
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private double x;
    private double y;
    
    public Double distancia(GPS g){
        return Math.abs(x-g.getx())+Math.abs(x-g.gety());
    }
    
    
    /**
     * COnstrutor para objetos da classe GPS
     */
    public GPS()
    {
        // inicializa variáveis de instância
        x = 0;
        y = 0;
    }
    public GPS(double x, double y)
    {
        this.x=x;
        this.y=y;
    }
    public GPS(GPS n)
    {
        this.x=n.getx();
        this.y=n.gety();
    }
    
    public Double getx(){
        return this.x;
    }
    public Double gety(){
        return this.y;
    }
    
    public void setx(Double x){
        this.x=x;
    }
    public void sety(Double y){
        this.y=y;
    }
    
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        GPS s = (GPS) obj;
        return s.getx()==this.x &&
               this.y==s.gety();
    }
    public GPS clone()
    {
        return new GPS(this);
    }
}
