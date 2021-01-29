import static java.lang.Math.sqrt;
import static java.lang.Math.pow;
import static java.lang.Math.abs;
import java.util.ArrayList;
import java.util.List;
import java.io.Serializable; 
 /**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class Localizacao implements Serializable{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private double latitude, longitude;

    /**
     * COnstrutor para objetos da classe Localizaçao 
     */
    
    public Localizacao()
    {
        this.latitude=0;
        this.longitude=0;
    }
    
    
    public Localizacao(double latitude, double longitude)
    {
        this.latitude= latitude;
        this.longitude= longitude;
    }
    public Localizacao(Localizacao t)
    {
        this.latitude= t.getLatitude();
        this.longitude= t.getLongitude();
    }


    public double getLatitude(){
        return latitude;
    }

    public double getLongitude(){
        return longitude;
    }
    
    public void setLatitude(double latitude){
         this.latitude = latitude;
    }

    public void setLongitude(double longitude){
         this.longitude = longitude;
    }
    
    public boolean equals(Object o)
    {
        if( this==o ) return true;

        if (o==null || o.getClass()!=this.getClass()) return false;
        
        Localizacao p = (Localizacao) o;
        
        return this.latitude == p.getLatitude()  &&  this.longitude == p.getLongitude(); 
    }

    public Localizacao clone() {
        return new Localizacao(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        sb.append(latitude);
        sb.append(",");
        sb.append(longitude);
        sb.append(")");
        
        return sb.toString();
    }
    
    // calculo da distancia do transporte
    public double distance(Localizacao l){
        return distance(l.getLatitude(),l.getLongitude());
    }
    public double distance(double a, double b){
        return abs(sqrt(pow(this.latitude-a,2)+pow(this.longitude-b,2)));
    }
}
