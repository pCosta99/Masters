


import java.util.ArrayList;
import java.io.Serializable;


public abstract class Transportador implements Serializable
{
    private String cod;
    private String nome;
    private GPS local;
    private double raio;
    
    private double avaliacaoTotal;
    private double numAvaliacoes;
    public Transportador(String cod, String nome, GPS local, double raio)
    {
        this.cod = cod;
        this.nome = nome;
        this.local = local;
        this.raio = raio;
        this.avaliacaoTotal = -1;
        this.numAvaliacoes = 0;
    }

    public void avaliar(double avaliacao){
        if (this.numAvaliacoes == 0){
            this.avaliacaoTotal = avaliacao;
            this.numAvaliacoes = 1;
        }
        else {
            this.avaliacaoTotal += avaliacao;
            this.numAvaliacoes += 1;
        }
    }
    
    public String getNome(){
        return this.nome;
    }
    
    public GPS getLocal(){
        return this.local;
    }

    public void transportar(String cod){ 
        //Transporte imediato. Caso contrário, 
        //this.livre = false e Main.removeDisponiveis(this.cod);
    }
    
    public boolean disponivel(GPS o, GPS d){
        return this.local.inRaio(this.raio, o, d);
    }
    
    public String getCod(){
        return this.cod;
    }
    
    public abstract double getPreco (double dist, Encomenda e);
        
    
}
