package MVC.Models.BaseModels;

import java.io.Serializable;

/**
 * Write a description of class Classificacao here.
 *
 * @author 89510-89561-89501
 * @version 01/05/2020
 */
public class Classificacao implements Serializable{
    private int somaNotas;
    private int numNotas;

    /**
     * Construtor de Classificacao por defeito.
     */
    public Classificacao(){
        this.somaNotas = 0;
        this.numNotas = 0;
    }

    /**
     * Construtor de Classificacao por Cópia.
     * @param c Classificacao a copiar.
     */
    public Classificacao(Classificacao c){
        this.somaNotas = c.getSomaNotas();
        this.numNotas = c.getNumNotas();
    }

    /**
     * Método que retorna a Soma de Todas as Classificações dadas.
     * @return Soma de todas as Classificações.
     */
    public int getSomaNotas(){
        return this.somaNotas;
    }

    /**
     * Método que retorna o Número Total de Classificações dadas.
     * @return Número Total de Classificações.
     */
    public int getNumNotas(){
        return this.numNotas;
    }

    /**
     * Método que adiciona uma Classificação.
     * @param nota Classificação dada.
     */
    public void addNota(int nota){
        this.somaNotas+= nota;
        this.numNotas++;
    }

    /**
     * Método que devolve a Classificação.
     * @return Classificação resultante.
     */
    public double getNota(){
        if(this.numNotas==0) return 0;
        return (this.somaNotas/this.numNotas);
    }

    /**
     * Método Clone.
     * @return Classificacao Clonada.
     */
    public Classificacao clone(){
        return new Classificacao(this);
    }
}
