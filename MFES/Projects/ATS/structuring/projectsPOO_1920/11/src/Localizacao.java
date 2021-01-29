import java.io.Serializable;

/**
 * Classe que trata das localizacoes
 * 
 * @author Rui Cunha
 * @version 06/04/2020
 */
public class Localizacao implements Serializable
{
    private double latitude;
    private double longitude;
    
    //construtor vazio
    public Localizacao()
    {
        this.latitude = 0.0;
        this.longitude = 0.0;
    }
    
    //construtor por parametros
    public Localizacao(double lat, double lon)
    {
        this.latitude = lat;
        this.longitude = lon;
    }
    
    //Construtor copia
    public Localizacao(Localizacao local)
    {
        this.latitude = local.getLatitude();
        this.longitude = local.getLongitude();
    }
    
    //Gets
    public double getLatitude()
    {
        return this.latitude;
    }
    
    public double getLongitude()
    {
        return this.longitude;
    }
    
    //Sets
    public void setLatitude(double lat)
    {
        this.latitude = lat;
    }
    
    public void setLongitude(double lon)
    {
        this.longitude = lon;
    }
    
    //Equals
    public boolean equals(Localizacao loc)
    {
        if(this==loc) return true;
        if(loc==null || loc.getClass() != this.getClass()) return false;
        Localizacao u = (Localizacao) loc;
        return this.latitude == u.getLatitude() && this.longitude == u.getLatitude();
    }
    
    //toString
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("Localizacao: \n")
        .append("\tLatitude: ").append(this.latitude + "\n")
        .append("\tLongitude: ").append(this.longitude);
        return sb.toString();
    }
    
    public Localizacao clone()
    {
        return new Localizacao(this);
    }
}
