/**
 * Write a description of class RegistoVoluntario here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 3 - 09-06-2020
 */

import java.time.LocalDateTime;
import java.io.Serializable;

public class RegistoVoluntarios implements Serializable
{
    private Voluntario voluntario;
    private LocalDateTime dataDeInicio;
    private double tempo;
    private Encomenda enc;
    
    public RegistoVoluntarios ()
    {   this.voluntario = voluntario;
        this.dataDeInicio = LocalDateTime.now();
        this.tempo = 0.0;
        this.enc = new Encomenda();
    }
    
    public RegistoVoluntarios (Voluntario voluntario, LocalDateTime dataDeInicio, double tempo, Encomenda enc)
    {   this.voluntario = voluntario;
        this.dataDeInicio = dataDeInicio;
        this.tempo = tempo;
        this.enc = enc;
    }
    
    public RegistoVoluntarios (RegistoVoluntarios r)
    {   this.voluntario = r.getVoluntario();
        this.dataDeInicio = r.getDataDeInicio();
        this.tempo = r.getTempo();
        this.enc = r.getEncomenda();
    }
    
    public Voluntario getVoluntario ()
    {   return this.voluntario;
    }
    
    public LocalDateTime getDataDeInicio ()
    {   return this.dataDeInicio;
    }
    
    public double getTempo ()
    {   return this.tempo;
    }
    
    public Encomenda getEncomenda ()
    {   return this.enc;
    }
    
    public void setVoluntario (Voluntario voluntario)
    { this.voluntario = voluntario;
    }
    
    public void setDataDeInicio (LocalDateTime dataDeInicio)
    {   this.dataDeInicio = dataDeInicio;
    }
    
    public void setTempo (double tempo)
    {   this.tempo = tempo;
    }
    
    public void setEncomenda (Encomenda enc)
    {   this.enc = enc;
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Data de Inicio do Transporte: ").append(this.dataDeInicio).append(" , ").append("Tempo total da viagem: ").append(this.tempo).append(" , ")
                                                   .append("Encomenda Transportada: ").append(this.enc).append("\n");
        return s.toString();
    }
    
    public RegistoVoluntarios clone ()
    {   return new RegistoVoluntarios(this);
    }
}