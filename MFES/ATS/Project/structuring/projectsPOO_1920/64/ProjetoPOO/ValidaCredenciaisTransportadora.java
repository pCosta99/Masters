/**
 * Write a description of class ValidaCredenciais here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 1 - 08-06-2020
 */
public class ValidaCredenciaisTransportadora
{
    private boolean flag; 
    private EmpresaTransportadora transportadora;
    
    public ValidaCredenciaisTransportadora ()
    {   this.flag = false;
        this.transportadora = new EmpresaTransportadora();
    }
    
    public ValidaCredenciaisTransportadora (boolean flag, EmpresaTransportadora transportadora)
    {   this.flag = flag;
        this.transportadora = transportadora;
    }
    
    public ValidaCredenciaisTransportadora (ValidaCredenciaisTransportadora v)
    {   this.flag = v.getFlag();
        this.transportadora = v.getTransportadora();
    }
    
    public boolean getFlag ()
    {   return this.flag;
    }
    
    public EmpresaTransportadora getTransportadora ()
    {   return this.transportadora;
    }
    
    public void setFlag (boolean flag)
    {   this.flag = flag;
    }
    
    public void setTransportadora (EmpresaTransportadora transportadora)
    {   this.transportadora = transportadora;
    }
    
    public ValidaCredenciaisTransportadora clone()
    {   return new ValidaCredenciaisTransportadora(this);
    }
}