/**
 * Write a description of class Loja here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 3 - 11-06-2020
 */

import java.lang.String;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class Loja implements Serializable
{
    private String codigoL;
    private String nome;
    private double latitude;
    private double longitude;
    private ArrayList<Encomenda> pedidos;
    private int pedidosEmEspera;
    private String email;
    private String password;
    
    public Loja ()
    {   this.codigoL = new String();
        this.nome = new String();
        this.latitude = 0.0;
        this.longitude = 0.0;
        this.pedidos = new ArrayList<Encomenda>();
        this.pedidosEmEspera = 0;
        this.email = new String();
        this.password = new String();
    }
    
    public Loja (String codigoL, String nome, double latitude, double longitude, ArrayList<Encomenda> pedidos, int pedidosEmEspera, String email, String password)
    {   this.codigoL = codigoL;
        this.nome = nome;
        this.latitude = latitude;
        this.longitude = longitude;
        setPedidos(pedidos);
        this.pedidosEmEspera = pedidosEmEspera;
        this.email = email;
        this.password = password;
    }
    
    public Loja (Loja l)
    {   this.codigoL = l.getCodigoL();
        this.nome = l.getNome();
        this.latitude = l.getLatitude();
        this.longitude = l.getLongitude();
        setPedidos(l.getPedidos());
        this.pedidosEmEspera = pedidosEmEspera;
        this.email = l.getEmail();
        this.password = l.getPassword();
    }
    
    public String getCodigoL()
    {   return this.codigoL = codigoL;
    }
    
    public String getNome ()
    {   return this.nome;
    }
    
    public double getLatitude ()
    {   return this.latitude;
    }
    
    public double getLongitude ()
    {   return this.longitude;
    }
    
    public ArrayList<Encomenda> getPedidos()
    {   ArrayList<Encomenda> aux = new ArrayList<Encomenda>();
        for (Encomenda e : this.pedidos){
            aux.add(e.clone());
        }
        return aux;
    }
    
    public int getPedidosEmEspera ()
    {   return this.pedidosEmEspera;
    }
    
    public String getEmail ()
    {   return this.email;
    }
    
    public String getPassword ()
    {   return this.password;
    }
    
    public void setCodigoL (String codigoL)
    {   this.codigoL = codigoL;
    }
    
    public void setNome (String nome)
    {   this.nome = nome;
    }
    
    public void setLatitude (double latitude)
    {   this.latitude = latitude;
    }
    
    public void setLongitude (double longitude)
    {   this.longitude = longitude;
    }
    
    public void setPedidos (ArrayList<Encomenda> pedidos)
    {   this.pedidos = new ArrayList<Encomenda>();
        for (Encomenda e : pedidos){
            this.pedidos.add(e.clone());
        }
    }
    
    public void setPedidosEmEspera (int pedidosEmEspera)
    {   this.pedidosEmEspera = pedidosEmEspera;
    }
    
    public void setEmail (String email)
    {   this.email = email;
    }
    
    public void setPassword (String password)
    {   this.password = password;
    }
    
    public void pedidosEmEspera ()
    {   this.pedidosEmEspera = this.pedidos.size();
    }
    
    public String toString()
    {   StringBuilder s = new StringBuilder();
        s.append("Loja: ").append(this.codigoL).append(" , ").append(this.nome).append("\n");
        return s.toString();
    }
    
    public boolean equals (Object o)
    {   boolean flag = false;
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Loja l = (Loja) o;
        if (this.latitude == l.getLatitude() && this.longitude == l.getLongitude()){
            flag = true;
        }
        return flag && this.codigoL.equals(l.getCodigoL()) && this.nome.equals(l.getNome()) && this.pedidos.equals(l.getPedidos());
    }
    
    public Loja clone()
    {   return new Loja (this);
    }
    
    public int compareTo (Loja l)
    {   return this.nome.compareTo(l.getNome());
    }
}