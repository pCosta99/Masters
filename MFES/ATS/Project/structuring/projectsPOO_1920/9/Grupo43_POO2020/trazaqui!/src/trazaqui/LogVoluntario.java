package trazaqui;

import java.io.Serializable;
import java.util.ArrayList;

public class LogVoluntario extends Voluntario implements Serializable {
    private String username;
    private String password;
    private boolean disponibilidade;
    private ArrayList<Classificacao> classificacoes;

    //getters
    public String getUsername(){return this.username;}

    public String getPassword(){return this.password;}

    public boolean getDisponibilidade(){return this.disponibilidade;}

    public ArrayList<Classificacao> getClassificacoes(){return new ArrayList<>(this.classificacoes);}

    //setters
    public void setUsername(String user){this.username=user;}

    public void setPassword(String pass){this.password=pass;}

    public void setDisponibilidade(boolean tf){this.disponibilidade=tf;}

    public void setClassificacoes(ArrayList<Classificacao> cl){this.classificacoes=new ArrayList<>(cl);}

    //construtor por omissão
    public LogVoluntario(){
        super();
        this.username="";
        this.password="";
        this.disponibilidade=false;
        this.classificacoes=new ArrayList<>();
    }

    //construtor parametrizado
    public LogVoluntario(String cod, String nome, Localizacao gps, double raio, String user, String pass, boolean tf, ArrayList<Classificacao> cl){
        super(cod,nome,gps,raio);
        this.username=user;
        this.password=pass;
        this.disponibilidade=tf;
        setClassificacoes(cl);
    }

    public LogVoluntario(Voluntario v, String user, String pass, boolean tf,ArrayList<Classificacao> cl){
        super(v.getCodVoluntario(),v.getNome(),v.getGps(),v.getRaio());
        this.username=user;
        this.password=pass;
        this.disponibilidade=tf;
        setClassificacoes(cl);
    }

    //construtor por cópia
    public LogVoluntario(LogVoluntario log){
        super(log.getCodVoluntario(),log.getNome(),log.getGps(),log.getRaio());
        this.username=log.getUsername();
        this.password=log.getPassword();
        this.disponibilidade=log.getDisponibilidade();
        setClassificacoes(log.getClassificacoes());
    }

    //metodo toString
    public String toString(){
        StringBuilder sb= new StringBuilder();
        sb.append(super.toString());
        sb.append("Username").append(this.username).append("\n")
                .append("Password:").append(this.password).append("\n")
                .append("Classificações").append(this.classificacoes).append("\n");
        return sb.toString();
    }

    //metodo clone
    public LogVoluntario clone(){return new LogVoluntario(this);}

    //metodo equals
    public boolean equals(Object o){
        if(this==o) return true;
        if( (o==null) || (o.getClass()!=this.getClass())) return false;

        LogVoluntario vol =(LogVoluntario) o;

        return (super.equals(vol))
                && vol.getUsername().equals(this.username)
                && vol.getPassword().equals(this.password)
                && vol.getClassificacoes().equals(this.classificacoes);
    }

}
