import static java.lang.Math.sqrt;
import static java.lang.Math.pow;
import static java.lang.Math.abs;
import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;

/**
 * Escreva a descrição da classe Gps aqui.
 * 
 * @author (seu nome)
 * @version (número de versão ou data)
 */
public class Ponto2D implements Serializable
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private double x, y;

    /**
     * COnstrutor para objetos da classe Gps
     */
    
    public Ponto2D()
    {
        this(0,0);
    }
    public Ponto2D(double x, double y)
    {
        this.x= x;
        this.y= y;
    }
    public Ponto2D(Ponto2D t)
    {
        this.x= t.getX();
        this.y= t.getY();
    }

    /**
     * Exemplo de método - substitua este comentário pelo seu próprio
     * 
     * @param  y   exemplo de um parâmetro de método
     * @return     a soma de x com y 
     */
    public double getX(){
        return x;
    }

    public double getY(){
        return y;
    }
    
    public void setX(double x){this.x=x;}
    
    public void setY(double y){this.y=y;}
    
    //distancia (euclidiana) entre o ponto atual selecionado e o ponto t (passado por parametro)
    public double distance(Ponto2D t)
    {
        return distance(t.getX(),t.getY());
    }
    public double distance(double a, double b)
    {
        return abs(sqrt(pow(this.x- a,2)+pow(this.y - b,2)));
    }
    //dado um arraylist de varios pontos, retorna o ponto mais proximo 
    public Ponto2D closest( List<Ponto2D> lt)
    {
        double min = distance(lt.get(0));
        double tmp = 0;
        Ponto2D ponto= lt.get(0);

        for (Ponto2D t: lt){
            tmp=distance(t);
            if (tmp<min){
                min=tmp;
                ponto=t;
            }
        }
        return ponto;
    }

    //mover o ponto para uma nova localizaçao
    public void move(Ponto2D t)
    {
        this.x=t.getX();
        this.y=t.getY();
    }
    
    public void move(double x, double y){
        this.x = x;
        this.y = y;
    }


    public boolean equals(Object o)
    {
        if( this==o ) return true;

        if (o==null || o.getClass()!=this.getClass()) return false;
        
        Ponto2D p = (Ponto2D) o;
        
        return this.x == p.getX()  &&  this.y == p.getY(); 
    }

    public Ponto2D clone() {
        return new Ponto2D(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        sb.append(x);
        sb.append(",");
        sb.append(y);
        sb.append(")");
        
        return sb.toString();
    }
}
