/**
 * Write a description of class EmpresaTransportadora here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 5 - 11-06-2020
 */

import java.lang.String;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class EmpresaTransportadora implements Comparable<EmpresaTransportadora>, Serializable
{
    private String codigoE;
    private String nome;
    private double latitude;
    private double longitude;
    private int nif;
    private double raioAcao;
    private double precoPorKm;
    private int estado; /*0-Livre,1-Ocupado*/
    private String email;
    private String password;
    private ArrayList<Proposta> listaDePropostas;
    private double classificacao;
    private int numClassificacoes;
    private double numKmsPercorridos;
    
    public EmpresaTransportadora ()
    {   this.codigoE = new String();
        this.nome = new String();
        this.latitude = 0.0;
        this.longitude = 0.0;
        this.nif = 0;
        this.raioAcao = 0.0;
        this.precoPorKm = 0.0;
        this.estado = 0;
        this.email = new String();
        this.password = new String();
        this.listaDePropostas = new ArrayList<Proposta>();
        this.numKmsPercorridos = numKmsPercorridos;
    }
    
    public EmpresaTransportadora (String codigoE, String nome, double latitude, double longitude, int nif, double raioAcao,
    double precoPorKm, int estado, String email, String password, ArrayList<Proposta> listaDePropostas, double classificacao, int numClassificacoes, double numKmsPercorridos)
    {   this.codigoE = codigoE;
        this.nome = nome;
        this.latitude = latitude;
        this.longitude = longitude;
        this.nif = nif;
        this.raioAcao = raioAcao;
        this.precoPorKm = precoPorKm;
        this.estado = estado;
        this.email = email;
        this.password = password;
        setListaDePropostas(listaDePropostas);
        this.classificacao = classificacao;
        this.numClassificacoes = numClassificacoes;
        this.numKmsPercorridos = numKmsPercorridos;
    }
    
    public EmpresaTransportadora (EmpresaTransportadora e)
    {   this.codigoE = e.getCodigoE();
        this.nome = e.getNome();
        this.latitude = e.getLatitude();
        this.longitude = e.getLongitude();
        this.nif = e.getNif();
        this.raioAcao = e.getRaioAcao();
        this.precoPorKm = e.getPrecoPorKm();
        this.estado = e.getEstado();
        this.email = e.getEmail();
        this.password = e.getPassword();
        setListaDePropostas(e.getListaDePropostas());
        this.classificacao = e.getClassificacao();
        this.numClassificacoes = e.getNumClassificacoes();
        this.numKmsPercorridos = e.getNumKmsPercorridos();
    }
    
    public String getCodigoE ()
    {   return this.codigoE;
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
    
    public int getNif ()
    {   return this.nif;
    }
    
    public double getRaioAcao ()
    {   return this.raioAcao;
    }
    
    public double getPrecoPorKm ()
    {   return this.precoPorKm;
    }
    
    public int getEstado ()
    {   return this.estado;
    }
    
    public String getEmail ()
    {   return this.email;
    }
    
    public String getPassword ()
    {   return this.password;
    }
    
    public ArrayList<Proposta> getListaDePropostas ()
    {   ArrayList<Proposta> listaDePropostas = new ArrayList<Proposta>();
        for (Proposta p : this.listaDePropostas){
            listaDePropostas.add(p);
        }
        return listaDePropostas;
    }
    
    public double getClassificacao ()
    {   return this.classificacao;
    }
    
    public int getNumClassificacoes ()
    {   return this.numClassificacoes;
    }
    
    public double getNumKmsPercorridos ()
    {   return this.numKmsPercorridos;
    }
    
    public void setCodigoE (String codigoE)
    {   this.codigoE = codigoE;
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
    
    public void setNif (int nif)
    {   this.nif = nif;
    }
    
    public void setRaioAcao (double raioAcao)
    {   this.raioAcao = raioAcao;
    }
    
    public void setTaxa (double taxa)
    {   this.precoPorKm = precoPorKm;
    }
    
    public void setEstado (int estado)
    {   this.estado = estado;
    }
    
    public void setEmail (String email)
    {   this.email = email;
    }
    
    public void setPassword (String password)
    {   this.password = password;
    }
    
    public void setListaDePropostas (ArrayList<Proposta> listaDePropostas)
    {   this.listaDePropostas = new ArrayList<Proposta>();
        for (Proposta p : listaDePropostas){
            this.listaDePropostas.add(p);
        }
    }
    
    public void setClassificacao (double classificacao)
    {   this.classificacao = classificacao;
    }
    
    public void setNumClassificacoes (int numClassificacoes)
    {   this.numClassificacoes = numClassificacoes;
    }
    
    public void setNumKmsPercorridos (double numKmsPercorridos)
    {   this.numKmsPercorridos = numKmsPercorridos;
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
    
    public void atualizaKms (double numKms)
    {   double numKmsI = this.numKmsPercorridos;
        double numKmsF = numKmsI + numKms;
        setNumKmsPercorridos(numKmsF);
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Empresa Transportadora: ").append(this.codigoE).append(", ")
                                            .append(this.nome).append("\n").append("Coordenadas GPS:").append("\n")
                                            .append("Latitude: ").append(this.latitude).append("\n")
                                            .append("Longitude: ").append(this.longitude).append("\n")
                                            .append("Nif: ").append(this.nif).append("\n")
                                            .append("Raio de Acao: ").append(this.raioAcao).append("\n")
                                            .append("PrecoPorKm: ").append(this.precoPorKm).append("\n")
                                            .append("Estado: ").append(this.estado).append("\n")
                                            .append("Classificacao :").append(this.classificacao).append(" estrelas").append("\n");
        return s.toString();
    }
    
    public boolean equals (Object o)
    {   boolean flag = false;
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        EmpresaTransportadora e = (EmpresaTransportadora) o;
        if (this.latitude == e.getLatitude() && this.longitude == e.getLongitude() && this.nif == e.getNif() && this.raioAcao == e.getRaioAcao() && this.precoPorKm == e.getPrecoPorKm()){
            flag = true;
        }
        return flag && this.codigoE.equals(e.getCodigoE()) && this.nome.equals(e.getNome());
    }
    
    public EmpresaTransportadora clone ()
    {   return new EmpresaTransportadora(this);
    }
    
    public int compareTo (EmpresaTransportadora e)
    {   return this.codigoE.compareTo(e.getCodigoE());
    }
}