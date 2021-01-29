package trazaqui;

import java.io.Serializable;

public class Voluntario implements Serializable {
    private String codVoluntario;
    private String nome;
    private Localizacao gps;
    private double raio;


    //getters
    public String getCodVoluntario(){return this.codVoluntario;}

    public String getNome(){return this.nome;}

    public Localizacao getGps(){return new Localizacao(this.gps.getX(),this.gps.getY());}

    public double getRaio(){return this.raio;}


    //setters
    public void setCodVoluntario(String cod){this.codVoluntario=cod;}

    public void setNome(String nome){this.nome=nome;}

    public void setGps(Localizacao p){this.gps=new Localizacao(p);}

    public void setRaio(double raio) {this.raio=raio;}


    //construtor por omissão
    public Voluntario(){
        this.codVoluntario="";
        this.nome="";
        this.gps=new Localizacao();
        this.raio=0;
    }

    //construtor parametrizado
    public Voluntario(String cod,String nome, Localizacao pos, double raio){
        this.codVoluntario=cod;
        this.nome=nome;
        setGps(pos);
        this.raio=raio;
    }

    //construtor por cópia
    public Voluntario(Voluntario v){
        this.codVoluntario=v.getCodVoluntario();
        this.nome=v.getNome();
        setGps(v.getGps());
        this.raio=v.getRaio();
    }

    //metodo toString
    public String toString(){
        StringBuilder sb= new StringBuilder();
        sb.append("Voluntário: \n")
                .append("Nome:").append(this.nome).append("\n")
                .append("Codigo do Voluntário:").append(this.codVoluntario).append("\n")
                .append("Coordenadas:").append(this.gps).append("\n")
                .append("Raio:").append(this.raio).append("\n");
        return sb.toString();
    }

    //metodo clone
    public Voluntario clone(){ return new Voluntario(this);}

}
