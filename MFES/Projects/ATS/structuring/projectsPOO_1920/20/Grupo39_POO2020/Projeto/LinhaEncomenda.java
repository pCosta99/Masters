
/**
 * Representacao de Linha de Encomenda
 *
 * @author MaterialPOO
 * @version 20180312
 * @version 20200317
 */
import java.io.Serializable;
public class LinhaEncomenda implements Serializable{
    private String referencia;
    private String descricao;
    private double preco;
    private double quantidade;
    private double imposto;
    private double desconto;
    
    public LinhaEncomenda() {
        this.referencia = "n/a";
        this.descricao = "n/a";
        this.preco = 0;
        this.quantidade = 0;
        this.imposto = 0;
        this.desconto = 0;
    }
    
    public LinhaEncomenda(String referencia, String descricao, double preco,
                double quantidade, double imposto, double desconto) {
        this.referencia = referencia;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
        this.imposto = imposto;
        this.desconto = desconto;
    }
    
    public LinhaEncomenda(String referencia,String descricao,double quantidade,double preco){
        this.referencia = referencia;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.preco = preco;
        this.imposto = 0;
        this.desconto = 0;
    }   
    
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.referencia = linha.getReferencia();
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade = linha.getQuantidade();
        this.imposto = linha.getImposto();
        this.desconto = linha.getDesconto();
    }
    
    public LinhaEncomenda (LinhaEncomenda linha, double quantidade){
        this.referencia = linha.getReferencia();
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade = quantidade;
        this.imposto = linha.getImposto();
        this.desconto = linha.getDesconto();
    }
    
    public double calculaValorLinhaEnc() {
        double valor = this.quantidade * this.preco;
        valor -= valor*this.desconto;
        valor *= 1+this.imposto;
        return valor;
    }

    public double calculaValorDesconto() {
        double valor = this.quantidade * this.preco;
        valor *= this.imposto; //e.g. imposto = 1.06
        return this.calculaValorLinhaEnc()-valor;
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

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    public double getImposto() {
        return this.imposto;
    }

    public void setImposto(double imposto) {
        this.imposto = imposto;
    }

    public double getDesconto() {
        return this.desconto;
    }

    public void setDesconto(double desconto) {
        this.desconto = desconto;
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
              le.getPreco() == this.preco;
    }      
}
