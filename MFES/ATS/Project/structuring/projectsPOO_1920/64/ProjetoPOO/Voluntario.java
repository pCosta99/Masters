/**
 * Write a description of class Voluntario here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 5 - 11-06-2020
 */

import java.lang.String;
import java.io.Serializable;

public class Voluntario implements Serializable
{
    private String codigoV;
    private String nome;
    private double latitude;
    private double longitude;
    private double raioGeografico;
    private int estado; /* 0-Livre; 1-Ocupado */
    private boolean estadoEncMedica;
    private String email;
    private String password;
    private double classificacao;
    private int numClassificacoes;
    
    public Voluntario ()
    {   this.codigoV = new String();
        this.nome = new String();
        this.latitude = 0.0;
        this.longitude = 0.0;
        this.raioGeografico = 0.0;
        this.estado = 0;
        this.estadoEncMedica = false;
        this.email = new String();
        this.password = new String();
        this.classificacao = 0.0;
        this.numClassificacoes = 0;
    }
    
    public Voluntario (String codigoV, String nome, double latitude, double longitude, double raioGeografico,
                       int estado, boolean estadoEncMedica, String email, String password, double classificacao, int numClassificacoes)
    {   this.codigoV = codigoV;
        this.nome = nome;
        this.latitude = latitude;
        this.longitude = longitude;
        this.raioGeografico = raioGeografico;
        this.estado = estado;
        this.estadoEncMedica = estadoEncMedica;
        this.email = email;
        this.password = password;
        this.classificacao = classificacao;
        this.numClassificacoes = numClassificacoes;
    }
    
    public Voluntario (Voluntario v)
    {   this.codigoV = v.getCodigoV();
        this.nome = v.getNome();
        this.latitude = v.getLatitude();
        this.longitude = v.getLongitude();
        this.raioGeografico = v.getRaioGeografico();
        this.estado = v.getEstado();
        this.estadoEncMedica = v.aceitoTransporteMedicamentos();
        this.email = v.getEmail();
        this.password = v.getPassword();
        this.classificacao = v.getClassificacao();
        this.numClassificacoes = v.getNumClassificacoes();
    }
    
    public String getCodigoV ()
    {   return this.codigoV;
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
    
    public double getRaioGeografico ()
    {   return this.raioGeografico;
    }
    
    public int getEstado ()
    {   return this.estado;
    }
    
    /* getEstadoEncMedica */
    public boolean aceitoTransporteMedicamentos ()
    {   return this.estadoEncMedica;
    }
    
    public String getEmail ()
    {   return this.email;
    }
    
    public String getPassword ()
    {   return this.password;
    }
    
    public double getClassificacao ()
    {   return this.classificacao;
    }
    
    public int getNumClassificacoes ()
    {   return this.numClassificacoes;
    }
    
    public void setCodigoV (String codigoV)
    {   this.codigoV = codigoV;
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
    
    public void setRaioGeografico (double raioGeografico)
    {   this.raioGeografico = raioGeografico;
    }
    
    public void setEstado (int estado)
    {   this.estado = estado;
    }
    
    /* setEstadoEncMedica */
    public void aceitaMedicamentos (boolean estadoEncMedica)
    {   this.estadoEncMedica = estadoEncMedica;
    }
    
    public void setEmail (String email)
    {   this.email = email;
    }
    
    public void setPassword (String password)
    {   this.password = password;
    }
    
    public void setClassificacao (double classificacao)
    {   this.classificacao = classificacao;
    }
    
    public void setNumClassificacoes (int numClassificacoes)
    {   this.numClassificacoes = numClassificacoes;
    }
    
    public void classifica (int classificacao)
    {   double classificacaoI = this.classificacao;
        int numClassificacoesI = this.numClassificacoes;
        double classificacaoF = 0.0;
        int numClassificacoesF = numClassificacoesI + 1;
        classificacaoF = (classificacaoI + classificacao) / numClassificacoesF;
        setClassificacao(classificacaoF);
        setNumClassificacoes(numClassificacoesF);
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Voluntario: ").append("CodigoV: ").append(this.codigoV).append("\n")
                                .append("Nome: ").append(this.nome).append("\n").append("Coordenadas GPS:").append("\n")
                                .append("Latitude: ").append(this.latitude).append("\n")
                                .append("Longitude: ").append(this.longitude).append("\n")
                                .append("Raio Geografico: ").append(this.raioGeografico).append("\n")
                                .append("Estado: ").append(this.estado).append("\n")
                                .append("Estado Encomenda Medica: ").append(this.estadoEncMedica).append("\n")
                                .append("Classificacao :").append(this.classificacao).append(" estrelas").append("\n");
        return s.toString();
    }
    
    public boolean equals (Object o)
    {   boolean flag = false;
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Voluntario v = (Voluntario) o;
        if (this.codigoV == v.getCodigoV() &&
            this.nome == v.getNome() && 
            this.latitude == v.getLatitude() && 
            this.longitude == v.getLongitude() && 
            this.raioGeografico == v.getRaioGeografico() && 
            this.estado == v.getEstado() && 
            this.estadoEncMedica == v.aceitoTransporteMedicamentos()){
            flag = true;
        }
        return flag;
    }
    
    public Voluntario clone ()
    {   return new Voluntario(this);
    }
    
    public int compareTo (Voluntario v)
    {   int compareTo = 0;
        if (this.estadoEncMedica != v.aceitoTransporteMedicamentos()){
            compareTo = 1;
        }
        return compareTo;
    }
}