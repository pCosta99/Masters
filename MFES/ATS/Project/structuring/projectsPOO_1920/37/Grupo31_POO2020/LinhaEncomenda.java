
/**
 * Escreva a descrição da classe LinhaEncomenda aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.io.*;

public class LinhaEncomenda implements Serializable {
    private String codProduto;
    private String descricao;
    private double preco;
    private double quantidade;
    private double imposto;
    private double desconto;
   
    
    public LinhaEncomenda() {
        this.codProduto = "n/a";
        this.descricao = "n/a";
        this.preco = 0;
        this.quantidade = 0;
        this.imposto = 0;
        this.desconto = 0;
    }
    
    public LinhaEncomenda(String codProduto, String descricao, double preco,
                double quantidade, double imposto, double desconto) {
        this.codProduto= codProduto;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
        this.imposto = imposto;
        this.desconto = desconto;
    }
    
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.codProduto= linha.getCodProduto();
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade = linha.getQuantidade();
        this.imposto = linha.getImposto();
        this.desconto = linha.getDesconto();
    }
    
    /**
     * B)
     */
    public double calculaValorLinhaEnc() {
        double valor = this.quantidade * this.preco;
        valor -= valor*this.desconto;
        valor *= 1+this.imposto;
        return valor;
    }
    
    /**
     * C)
     */
    public double calculaValorDesconto() {
        double valor = this.quantidade * this.preco;
        valor *= this.imposto; //e.g. imposto = 1.06
        return this.calculaValorLinhaEnc()-valor;
    }  
    
    public String getCodProduto() {
        return this.codProduto;
    }
    
     public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
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
        return le.getCodProduto().equals(this.codProduto) &&
              le.getDescricao().equals(this.descricao) && 
              le.getPreco() == this.preco &&
              le.getQuantidade() == this.quantidade &&
              le.getImposto() == this.imposto &&
              le.getDesconto()== this.desconto;
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\nCódigo do Produto: ").append (this.codProduto);
        sb.append(", Descricao: ").append (this.descricao); 
        sb.append(", Preco: ").append (this.preco);
        sb.append(", Quantidade: ").append (this.quantidade);
        sb.append(", Imposto: ").append (this.imposto);
        sb.append(", Desconto: ").append (this.desconto);
        
        return sb.toString();
    }            
    
}
