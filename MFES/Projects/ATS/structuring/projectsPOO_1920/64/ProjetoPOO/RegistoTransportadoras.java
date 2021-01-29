/**
 * Write a description of class RegistoTransportadora here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 3 - 09-06-2020
 */

import java.time.LocalDateTime;
import java.io.Serializable;

public class RegistoTransportadoras implements Serializable
{
    private EmpresaTransportadora empresa;
    private LocalDateTime dataDeInicio;
    private double tempo;
    private double preco;
    private Encomenda enc;
    
    public RegistoTransportadoras ()
    {   this.empresa = empresa;
        this.dataDeInicio = LocalDateTime.now();
        this.tempo = 0.0;
        this.preco = 0.0;
        this.enc = new Encomenda();
    }
    
    public RegistoTransportadoras (EmpresaTransportadora empresa, LocalDateTime dataDeInicio, double tempo, double preco, Encomenda enc)
    {   this.empresa = empresa;
        this.dataDeInicio = dataDeInicio;
        this.tempo = tempo;
        this.preco = preco;
        this.enc = enc;
    }
    
    public RegistoTransportadoras (RegistoTransportadoras r)
    {   this.empresa = r.getEmpresa();
        this.dataDeInicio = r.getDataDeInicio();
        this.tempo = r.getTempo();
        this.preco = r.getPreco();
        this.enc = r.getEncomenda();
    }
    
    public EmpresaTransportadora getEmpresa ()
    {   return this.empresa;
    }
    
    public LocalDateTime getDataDeInicio ()
    {   return this.dataDeInicio;
    }
    
    public double getTempo ()
    {   return this.tempo;
    }
    
    public double getPreco ()
    {   return this.preco;
    }
    
    public Encomenda getEncomenda ()
    {   return this.enc;
    }
    
    public void setEmpresa (EmpresaTransportadora empresa)
    { this.empresa = empresa;
    }
    
    public void setDataDeInicio (LocalDateTime dataDeInicio)
    {   this.dataDeInicio = dataDeInicio;
    }
    
    public void setTempo (double tempo)
    {   this.tempo = tempo;
    }
    
    public void setPreco (double preco)
    {   this.preco = preco;
    }
    
    public void setEncomenda (Encomenda enc)
    {   this.enc = enc;
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Data de Inicio do Transporte: ").append(this.dataDeInicio).append(" , ").append("Tempo total da viagem: ").append(this.tempo).append(" , ")
                                                   .append("Encomenda Transportada: ").append(this.enc).append(" , ").append("Custo :").append(this.preco).append("\n");
        return s.toString();
    }
    
    public RegistoTransportadoras clone ()
    {   return new RegistoTransportadoras(this);
    }
}