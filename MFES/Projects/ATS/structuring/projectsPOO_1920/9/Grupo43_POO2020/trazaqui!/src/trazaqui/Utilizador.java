package trazaqui;

import java.io.Serializable;

public class Utilizador implements Serializable {
    private String codUtilizador;
    private String nome;
    private Localizacao gps;

    //getters
    public String getCodUtilizador(){return this.codUtilizador;}

    public String getNome(){return this.nome;}

    public Localizacao getGps(){return new Localizacao(this.gps.getX(),this.gps.getY());}

    //setters
    public void setCodUtilizador(String c){this.codUtilizador=c;}

    public void setNome(String n){this.nome=n;}

    public void setGps(Localizacao pos){this.gps = new Localizacao(pos);}

    //construtor por omissão

    public Utilizador(){
        this.codUtilizador="";
        this.nome="";
        this.gps=new Localizacao();
    }

    //construtor parametrizado

    public Utilizador(String cod, String nome, Localizacao pos){
        this.codUtilizador=cod;
        this.nome=nome;
        setGps(pos);
    }

    //construtor por cópia

    public Utilizador(Utilizador u){
        this.codUtilizador=u.getCodUtilizador();
        this.nome=u.getNome();
        setGps(u.getGps());
    }

    //metodo toString
    public String toString(){
        StringBuilder sb= new StringBuilder();
        sb.append("Utilizador:").append("\n")
                .append("Código de Utilizador:").append(this.codUtilizador).append("\n")
                .append("Nome:").append(this.nome).append("\n")
                .append("GPS:").append(this.gps).append("\n");
        return sb.toString();
    }

    //metodo clone
    public Utilizador clone(){ return new Utilizador(this);}
}
