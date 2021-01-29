/**
 * Write a description of class ValidaCredenciais here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 1 - 08-06-2020
 */
public class ValidaCredenciaisVoluntario
{
    private boolean flag;
    private Voluntario voluntario;
    
    public ValidaCredenciaisVoluntario ()
    {   this.flag = false;
        this.voluntario = new Voluntario();
    }
    
    public ValidaCredenciaisVoluntario (boolean flag, Voluntario voluntario)
    {   this.flag = flag;
        this.voluntario = voluntario;
    }
    
    public ValidaCredenciaisVoluntario (ValidaCredenciaisVoluntario v)
    {   this.flag = v.getFlag();
        this.voluntario = v.getVoluntario();
    }
    
    public boolean getFlag ()
    {   return this.flag;
    }
    
    public Voluntario getVoluntario ()
    {   return this.voluntario;
    }
    
    public void setFlag (boolean flag)
    {   this.flag = flag;
    }
    
    public void setVoluntario (Voluntario voluntario)
    {   this.voluntario = voluntario;
    }
    
    public ValidaCredenciaisVoluntario clone()
    {   return new ValidaCredenciaisVoluntario(this);
    }
}