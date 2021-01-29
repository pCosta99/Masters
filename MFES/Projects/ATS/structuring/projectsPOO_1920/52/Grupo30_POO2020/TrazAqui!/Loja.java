import java.util.*;
import java.io.*;



public class Loja extends Utilizador implements Serializable {
    //variaveis de instancia
    private double tempoEspera;
    

    //construtores

    public Loja(){
        super();
        this.tempoEspera=0;
    }

    public Loja(String emailAux,String nomeAux, String passwordAux, Localizacao localizacaoAux, double tempoEsperaAux){
        super(emailAux,nomeAux,passwordAux,localizacaoAux);
        this.tempoEspera=tempoEsperaAux;
    }

    public Loja(Loja c){
        this(c.getEmail(),c.getNome(),c.getPassword(),c.getLocalizacao(),c.getTempoEspera());
    }

    //getters

    public double getTempoEspera(){
        return this.tempoEspera;
    }

    //setters

    public void setTempoEspera(double tempoEsperaAux){
        this.tempoEspera=tempoEsperaAux;
    }

    //equals
    public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || this.getClass() != object.getClass()) return false;
        if (super.equals(object) == false) return false;
        Loja aux = (Loja) object;
        return (this.getTempoEspera()==aux.getTempoEspera());
    }

    //clone

    public Loja clone(){
        return new Loja(this);
    }

    //toString
    public String toString(){
        return  "\nLoja->" +
                " Email: " + this.getEmail() + " | " +
                " Nome: " + this.getNome() +  " | " +
                " Localização:" + this.getLocalizacao();
    }
}
