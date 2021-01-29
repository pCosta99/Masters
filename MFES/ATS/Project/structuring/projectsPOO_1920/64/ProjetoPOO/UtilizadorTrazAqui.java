/**
 * Write a description of class UtilizadorTrazAqui here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 5 - 11-06-2020
 */

import java.lang.String;
import java.io.Serializable;

public class UtilizadorTrazAqui implements Serializable
{
    private String codigoU;
    private String nomeU;
    private double latitude;
    private double longitude;
    private String email;
    private String password;
    private int numEncs;
    
    public UtilizadorTrazAqui ()
    {   this.codigoU = new String();
        this.nomeU = new String();
        this.latitude = 0.0;
        this.longitude = 0.0;
        this.email = new String();
        this.password = new String();
        this.numEncs = 0;
    }
    
    public UtilizadorTrazAqui (String codigoU, String nomeU, double latitude, double longitude, String email, String password, int numEncs)
    {   this.codigoU = codigoU;
        this.nomeU = nomeU;
        this.latitude = latitude;
        this.longitude = longitude;
        this.email = email;
        this.password = password;
        this.numEncs = numEncs;
    }
    
    public UtilizadorTrazAqui (UtilizadorTrazAqui u)
    {   this.codigoU = u.getCodigoU();
        this.nomeU = u.getNomeU();
        this.latitude = u.getLatitude();
        this.longitude = u.getLongitude();
        this.email = u.getEmail();
        this.password = u.getPassword();
        this.numEncs = u.getNumEncs();
    }
    
    public String getCodigoU ()
    {   return this.codigoU;
    }
    
    public String getNomeU ()
    {   return this.nomeU;
    }
    
    public double getLatitude ()
    {   return this.latitude;
    }
    
    public double getLongitude ()
    {   return this.longitude;
    }
    
    public String getEmail ()
    {   return this.email;
    }
    
    public String getPassword ()
    {   return this.password;
    }
    
    public int getNumEncs ()
    {   return this.numEncs;
    }
    
    public void setCodigoU (String codigoU)
    {   this.codigoU = codigoU;
    }
    
    public void setNomeU (String nomeU)
    {   this.nomeU = nomeU;
    }
    
    public void setLatitude (double latitude)
    {   this.latitude = latitude;
    }
    
    public void setLongitude (double longitude)
    {   this.longitude = longitude;
    }
    
    public void setEmail (String email)
    {   this.email = email;
    }
    
    public void setPassword (String password)
    {   this.password = password;
    }
    
    public void setNumEncs (int numEncs)
    {   this.numEncs = numEncs;
    }
    
    public void atualizaNumEncs ()
    {   int numEncsI = this.numEncs;
        int numEncsF = numEncsI + 1;
        setNumEncs(numEncsF);
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Utilizador: ").append("\n").append("CodigoU: ").append(this.codigoU).append("\n")
                                             .append("NomeU :").append(this.nomeU).append("\n")
                                             .append("Latitude: ").append(this.latitude).append("\n")
                                             .append("Longitude: ").append(this.longitude).append("\n");
        return s.toString();
    }
    
    public boolean equals (Object o)
    {   boolean flag = false;
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        UtilizadorTrazAqui u = (UtilizadorTrazAqui) o;
        if (this.latitude == u.getLatitude() && this.longitude == u.getLongitude()){
            flag = true;
        }
        return flag && this.codigoU.equals(u.getCodigoU()) && this.nomeU.equals(u.getNomeU());
    }
    
    public UtilizadorTrazAqui clone ()
    {   return new UtilizadorTrazAqui(this);
    }
    
    public int compareTo (UtilizadorTrazAqui u)
    {   return this.nomeU.compareTo(u.getNomeU());
    }
}