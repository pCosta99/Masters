/**
 * Write a description of class ValidaCredenciais here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 1 - 08-06-2020
 */
public class ValidaCredenciaisLoja
{
    private boolean flag;
    private Loja loja;
    
    public ValidaCredenciaisLoja ()
    {   this.flag = false;
        this.loja = new Loja();
    }
    
    public ValidaCredenciaisLoja (boolean flag, Loja loja)
    {   this.flag = flag;
        this.loja = loja;
    }
    
    public ValidaCredenciaisLoja (ValidaCredenciaisLoja v)
    {   this.flag = v.getFlag();
        this.loja = v.getLoja();
    }
    
    public boolean getFlag ()
    {   return this.flag;
    }
    
    public Loja getLoja ()
    {   return this.loja;
    }
    
    public void setFlag (boolean flag)
    {   this.flag = flag;
    }
    
    public void setLoja (Loja loja)
    {   this.loja = loja;
    }
    
    public ValidaCredenciaisLoja clone()
    {   return new ValidaCredenciaisLoja(this);
    }
}