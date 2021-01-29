/**
 * Representacao de Linha de Encomendas
 *
 * @author MaterialPOO
 * @version 20180312
 */
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collection;
import java.util.ArrayList;
import java.util.List;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.Iterator;
public class LinhaEncomenda implements Serializable
{
    private String referencia;
    private String descricao;
    private double quantidade;
    private double valorUni;
    private double peso; //por unidade
    
    public LinhaEncomenda() {
        this.referencia = "n/a";
        this.descricao="n/a";
        this.valorUni = 0;
        this.quantidade = 0;
        this.peso=0;
    }
    
    public LinhaEncomenda(String referencia,String descricao, double valorUni,
                double quantidade,double peso) {
        this.referencia = referencia;
        this.descricao=descricao;
        this.valorUni = valorUni;
        this.quantidade = quantidade;
        this.peso=peso;
    }
    
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.referencia = linha.getReferencia();
        this.descricao=linha.getDescricao();
        this.valorUni = linha.getValorUni();
        this.quantidade = linha.getQuantidade();
        this.peso=linha.getPeso();
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
        this.descricao=descricao;
    }

    public double getValorUni() {
        return this.valorUni;
    }

    public void setValorUni(double valorUni) {
        this.valorUni=valorUni;
    }

    public double getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }
    
    public double getPeso(){
        return this.peso;
    }
    
    public void setPeso(double peso){
        this.peso=peso;
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
              le.getValorUni() == this.valorUni;
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("--Referencia: ").append(this.referencia);
        sb.append("--Descriçao: ").append(this.descricao);
        sb.append("--Valor por unidade : ").append(this.valorUni);
        sb.append("--Quanidade : ").append(this.quantidade);
        return sb.toString();
    }            
    
    public int compareTo(LinhaEncomenda a){
        return this.referencia.compareTo(a.getReferencia());
     }
    
    
    /**
     * Devolve o preço total dos produtos
     */
    public double precoLinha(){
        return this.valorUni*this.quantidade;
    }
}
