import java.util.*;
import java.io.*;

/**
 * Escreva a descrição da classe LinhaEncomenda aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class LinhaEncomenda implements Serializable {
    private String referencia;
    private String descricao;
    private double preco;
    private double quantidade;

    public LinhaEncomenda() {
        this.referencia = "n/a";
        this.descricao = "n/a";
        this.preco = 0;
        this.quantidade = 0;

    }
    
    public LinhaEncomenda(String referencia, String descricao, double preco,
                double quantidade) {
        this.referencia = referencia;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
    }
    
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.referencia = linha.getReferencia();
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade = linha.getQuantidade();
    }
    

    public String getReferencia() {
        return this.referencia;
    }
    
     public void setReferencia(String referencia) {
        this.referencia = referencia;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public double getPreco() {
        return this.preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public double getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }


    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }
    
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getReferencia().equals(this.referencia) &&
              le.getDescricao().equals(this.descricao) && 
              le.getPreco() == this.preco &&
              le.getQuantidade() == this.quantidade;
    }
    
          
    
        public String toString() {
        return  "Referencia:" + this.referencia+
                " Descricao:" + this.descricao + 
                " Pre�o:" + this.preco + 
                " Quantidade: " + this.quantidade;

    }
}

