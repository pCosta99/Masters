package trazaqui;

import java.io.Serializable;

public class Loja implements Serializable {
    private String codLoja;
    private String nome;
    private Localizacao gps;

    //getters
    public String getCodLoja(){return this.codLoja;}

    public String getNome(){return this.nome;}

    public Localizacao getGps(){return new Localizacao(this.gps.getX(),this.gps.getY());}

    //setters
    public void setCodLoja(String cod){this.codLoja=cod;}

    public void setNome(String nome){this.nome=nome;}

    public void setGps(Localizacao pos){this.gps=new Localizacao(pos);}

    //construtor vazio
    public Loja(){
        this.codLoja="";
        this.nome="";
        this.gps=new Localizacao();
    }

    //construtor parametrizado
    public Loja(String cod, String nome, Localizacao gps){
        this.codLoja=cod;
        this.nome=nome;
        setGps(gps);
    }

    //construtor por cópia
    public Loja(Loja l){
        this.codLoja=l.getCodLoja();
        this.nome=l.getNome();
        setGps(l.getGps());
    }

    //metodo toString
    public String toString(){
        StringBuilder sb= new StringBuilder();
        sb.append("Loja:").append("\n")
                .append("Código da Loja:").append(this.codLoja).append("\n")
                .append("Nome:").append(this.nome).append("\n")
                .append("Coordenadas gps:").append(this.gps).append("\n");

        return sb.toString();
    }

    //metodo loja
    public Loja clone(){return new Loja(this);}
}
