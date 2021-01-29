package trazaqui;

import java.io.Serializable;
import java.util.ArrayList;

public class LogTransportadora extends Transportadora implements Serializable {
    private String username;
    private String password;
    private ArrayList<Classificacao> classificacoes;

    //getters
    public String getUsername(){return this.username;}

    public String getPassword(){return this.password;}

    public ArrayList<Classificacao> getClassificacoes(){return new ArrayList<>(this.classificacoes);}

    //setters
    public void setUsername(String user){this.username=user;}

    public void setPassword(String pass){this.password=pass;}

    public void setClassificacoes(ArrayList<Classificacao> cl){this.classificacoes=new ArrayList<>(cl);}

    //construtor vazio
    public LogTransportadora(){
        super();
        this.username="";
        this.password="";
        this.classificacoes=new ArrayList<>();
    }

    //construtores parametrizados
    public LogTransportadora(String cod, String nome, Localizacao pos, String nif, double raio, double pk, String user, String pass, ArrayList<Classificacao> cl){
        super(cod,nome,pos,nif,raio,pk);
        this.username=user;
        this.password=pass;
        setClassificacoes(cl);
    }

    public LogTransportadora(Transportadora t, String user, String pass, ArrayList<Classificacao> cl){
        super(t.getCodEmpresa(),t.getNome(),t.getGps(),t.getNif(),t.getRaio(),t.getPrecokm());
        this.username=user;
        this.password=pass;
        setClassificacoes(cl);
    }

    //construtor por cópia
    public LogTransportadora(LogTransportadora log){
        super(log.getCodEmpresa(),log.getNome(),log.getGps(),log.getNif(),log.getRaio(),log.getPrecokm());
        this.username=log.getUsername();
        this.password=log.getPassword();
        setClassificacoes(log.getClassificacoes());
    }

    //metodo toString
    public String toString(){
        StringBuilder sb= new StringBuilder();
        sb.append(super.toString())
                .append("Username:").append(this.username).append("\n")
                .append("Password:").append(this.password).append("\n")
                .append("Classificações:").append(this.classificacoes).append("\n");
        return sb.toString();
    }

    //metodo clone
    public LogTransportadora clone(){return new LogTransportadora(this);}

    //metodo equals
    public boolean equals(Object o){
        if (this==o) return true;
        if ((o==null) || (o.getClass()!=this.getClass())) return false;
        LogTransportadora log = (LogTransportadora) o;
        return (super.equals(log))
                && log.getUsername().equals(this.getUsername())
                && log.getPassword().equals(this.getPassword())
                && log.getClassificacoes().equals(this.getClassificacoes());
    }
}
