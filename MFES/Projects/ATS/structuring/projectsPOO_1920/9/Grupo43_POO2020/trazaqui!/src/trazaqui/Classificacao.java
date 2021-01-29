package trazaqui;

import java.io.Serializable;

public class Classificacao implements Serializable {
    private double classificacao;

    //getter
    public double getClassificacao(){return this.classificacao;}

    //setter
    public void setClassificacao(double cl){this.classificacao=cl;}

    //construtor por omissão
    public Classificacao(){
        this.classificacao=0;
    }

    //construtor parametrizado
    public Classificacao(double cl){
        this.classificacao=cl;
    }

    //construtor cópia
    public Classificacao(Classificacao cl){
        this.classificacao=cl.getClassificacao();
    }

    //metodo toString
    public String toString(){
        StringBuilder sb= new StringBuilder();
        sb.append("Classificacao: ").append(this.classificacao).append("\n");
        return sb.toString();
    }
}
