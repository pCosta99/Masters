/**
 * Write a description of class EncomendaAceite here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 3 - 09-06-2020
 */

import java.util.ArrayList;
import java.util.List;
import java.lang.String;
import java.io.Serializable;

public class EncomendaAceite implements Serializable
{
    private String encAceite;
    
    public EncomendaAceite ()
    {   this.encAceite = new String();
    }
    
    public EncomendaAceite (String encAceite)
    {   this.encAceite = encAceite;
    }
    
    public EncomendaAceite (EncomendaAceite e)
    {   this.encAceite = e.getEncAceite();
    }
    
    public String getEncAceite ()
    {   return this.encAceite;
    }
    
    public void setEncAceite (String encAceite)
    {   this.encAceite = encAceite;
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Encomenda Aceite: ").append(this.encAceite);
        return s.toString();
    }
    
    public boolean equals (Object o)
    {   boolean flag = false;
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        EncomendaAceite e = (EncomendaAceite) o;
        return this.encAceite.equals(e.getEncAceite());
    }
    
    public EncomendaAceite clone ()
    {   return new EncomendaAceite(this);
    }
}