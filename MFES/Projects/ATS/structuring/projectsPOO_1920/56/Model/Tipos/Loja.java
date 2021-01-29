package Model.Tipos;

import Model.Catalogos.CatalogoProds;
import Model.Catalogos.ICatalogoProds;
import Model.Catalogos.Produto;
import Model.Encomendas.Encomenda;
import Model.Encomendas.ILinhaEncomenda;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Objects;

public class Loja extends Tipo implements ILoja, Serializable {

    private int pessoasFila; //sistema.encs.size()
    private double tempoPessoa; //tempo medio a atender uma pessoa
    private ICatalogoProds stock; //Stock da loja

    public Loja () {

        //Variáveis da App
        this.tempoPessoa  = 0.0;
        this.pessoasFila = 0;
        this.stock = new CatalogoProds();
    }

    public Loja (String nome, String codLoja, int pessoasFila, LocalDateTime tempo,
                 double tempoPessoa,ArrayList <Encomenda> encs, ICatalogoProds stock){

        super(nome,codLoja,0,0);
        this.pessoasFila = pessoasFila;
        this.tempoPessoa = tempoPessoa;
        this.stock = stock;
    }

    public Loja (Loja l){
        super(l);
        l.pessoasFila = l.getPessoasFila();
        l.tempoPessoa = l.getTempoPessoa();
        l.stock = l.getStock();
    }

    public int getPessoasFila() {
        return this.pessoasFila;
    }


    public double getTempoPessoa() {
        return tempoPessoa;
    }


    public ICatalogoProds getStock() {
        return stock;
    }

    public void setPessoasFila(int pessoasFila) {
        this.pessoasFila = pessoasFila;
    }


    public void setTempoPessoa(double tempoPessoa) {
        this.tempoPessoa = tempoPessoa;
    }


    public void setStock(ICatalogoProds stock) {
        this.stock = stock;
    }

    //Método Clone
    public Loja clone(){
        return new Loja(this);
    }

    //Equals para as variáveis do ficheiro Logs.txt
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Loja loja = (Loja) o;
        return  super.equals(loja) &&
                pessoasFila == loja.pessoasFila &&
                Double.compare(loja.tempoPessoa, tempoPessoa) == 0 &&
                this.stock.equals(loja.stock);
    }

    //HashCode para as variáveis do ficheiro Logs.txt
    public int hashCode() {
        return Objects.hash(super.hashCode(), getPessoasFila(), getTempoPessoa(), getStock());
    }

    public String toString() {
        return "Loja:" +
                this.getId() + "," +
                this.getNome() + "\n" +
                stock;
    }

    public String getIdSuper (ILoja loja) {
        return super.getId();
    }
}