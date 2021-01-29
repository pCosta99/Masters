package Model.Atores;

import Model.Encomenda;
import Model.Produto;

import java.awt.geom.Point2D;
import java.io.Serializable;
import java.util.*;

public class Loja extends Ator implements Serializable {


    //Variáveis de instância

    private int fila;
    private float espera; //tempo médio de espera por cliente
    private Map<String, Encomenda> encomendas;
    private Map<String, Produto> produtos;




    public Loja(){
        super();

        this.fila = 0;
        this.espera = 0;
        this.encomendas = new HashMap<>();
        this.produtos = new HashMap<>();

    }


    public Loja(String email, String nome,String referencia,String password, Point2D.Double morada, long nif, int fila, float espera, Map<String,Encomenda> enc, Map<String,Produto> prods) {
        super(email,referencia,nome,password,morada,nif);

        this.fila = fila;
        this.espera = espera;
        this.encomendas = enc;
        this.produtos = prods;

    }

    public Loja (Loja a) {
        super(a.getEmail(), a.getReferencia(),a.getNome(),a.getPassword(),a.getMorada(),a.getNif());
        setFila(a.getFila());
        setEspera(a.getEspera());
        this.encomendas = a.getEncomendas();
        this.produtos = a.getProdutos();

    }


    public int getFila() {
        return fila;
    }

    public void setFila(int fila) {
        this.fila = fila;
    }

    public float getEspera() {
        return espera;
    }

    public void setEspera(float espera) {
        this.espera = espera;
    }

    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> aux = new HashMap<>();
        for (Map.Entry<String,Encomenda> e : this.encomendas.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    public void setEncomendas(Map<String,Encomenda>enc){
        this.encomendas = new HashMap<>();
        enc.entrySet().forEach(e-> this.encomendas.put(e.getKey(),
                e.getValue().clone()));
    }

    public Map<String,Produto> getProdutos()  {
        Map<String,Produto> aux = new HashMap<>();
        for (Map.Entry<String,Produto> e : this.produtos.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    public String navString(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getReferencia()+ "  " + this.getNome() + " com " + this.getEspera() + " minutos de fila de espera ");
        return sb.toString();
    }
    @Override
    public String toString() {
        return super.toString()  +
                ", fila=" + fila +
                ", espera=" + espera +
                ", encomendas=" + encomendas +
                ", produtos=" + produtos
                ;
    }




    public void setProdutos(Map<String,Produto>prod){
        this.produtos = new HashMap<>();
        prod.entrySet().forEach(e-> this.produtos.put(e.getKey(),
                e.getValue().clone()));
    }


    public void removeEncomendaLoja(Encomenda e) {
        this.encomendas.remove(e.getReferencia());
    }

    /*Metodos*/

    public void adicionaEncomendaLoja(Encomenda e) {
        this.encomendas.put(e.getReferencia(),e.clone());
    }
    public void adicionaProdutoLoja(Produto e) {
        this.produtos.put(e.getReferencia(),e.clone());
    }



    public Loja clone(){

    return new Loja(this);
    }



    /**
    Métodos
     */

    public double tempoTotalEspera(){
        return this.getFila() * this.getEspera();
    }

}
