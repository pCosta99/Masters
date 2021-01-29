/**
 * Write a description of class ValidaCredenciais here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 1 - 08-06-2020
 */
public class ValidaCredenciaisUtilizador
{
    private boolean flag;
    private UtilizadorTrazAqui user;
    
    public ValidaCredenciaisUtilizador ()
    {   this.flag = false;
        this.user = new UtilizadorTrazAqui();
    }
    
    public ValidaCredenciaisUtilizador (boolean flag, UtilizadorTrazAqui user)
    {   this.flag = flag;
        this.user = user;
    }
    
    public ValidaCredenciaisUtilizador (ValidaCredenciaisUtilizador v)
    {   this.flag = v.getFlag();
        this.user = v.getUser();
    }
    
    public boolean getFlag ()
    {   return this.flag;
    }
    
    public UtilizadorTrazAqui getUser ()
    {   return this.user;
    }
    
    public void setFlag (boolean flag)
    {   this.flag = flag;
    }
    
    public void setUtilizadorTrazAqui (UtilizadorTrazAqui user)
    {   this.user = user;
    }
    
    public ValidaCredenciaisUtilizador clone()
    {   return new ValidaCredenciaisUtilizador(this);
    }
}