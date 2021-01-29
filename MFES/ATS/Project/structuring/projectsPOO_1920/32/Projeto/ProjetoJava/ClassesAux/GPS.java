

package ClassesAux;

import java.io.Serializable;
import java.util.List;
public class GPS implements Serializable
{
    private double x;
    private double y;

    public GPS()
    {
        x = 0;
        y = 0;
    }

     public GPS (double x,double y)
    {
        this.x=x;
        this.y=y;
    }
    
    public GPS (GPS g){
        this.x=g.getX();
        this.y=g.getY();
    }
    
    public double getX(){return this.x;}
    public double getY(){return this.y;}
    public void setX(double x){this.x=x;}
    public void setY(double y){this.y=y;}
    
    public double distancia(GPS a1){
    return Math.sqrt(Math.pow(this.getX()-a1.getX(),2)+(Math.pow(this.getY()-a1.getY(),2)));
    }
    //distancia de empresa a loja e loja a utilizador
    public double distancia2(GPS a2,GPS a3){
    return this.distancia(a2)+a2.distancia(a3);
    }
    public double distanciaRota (List<GPS> rota){
    double res=0;
    GPS anterior=this;
    for(GPS a:rota){
     res+=anterior.distancia(a);
     anterior=a;
       }
    return res;
    }
    //indica se a2 e a3 entao a menos de "raio"km de a1.
    public boolean estaoDentroDoRaio(double raio,GPS a2,GPS a3){
        return this.distancia(a2)<raio && this.distancia(a3)<raio;
    }
    public boolean estaoDentroDoRaio(double raio,GPS a1){
        return this.distancia(a1)<raio;
    }
    public GPS clone(){
    return new GPS(this);
    }
    public String toString(){
       StringBuilder sb =new StringBuilder();
    sb.append(",").append(this.x);
    sb.append(",").append(this.y);
    return sb.toString(); 
    }
    public boolean equals (GPS o){
        if (this==o) return true;
        if ((o == null) || (this.getClass() != o.getClass()))
        return false;
        GPS p = (GPS) o;
        return (p.getX()==this.x && 
        p.getY()==this.y
     );
    }
}
