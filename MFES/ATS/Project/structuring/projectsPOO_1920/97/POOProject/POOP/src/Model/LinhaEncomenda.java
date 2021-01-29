package Model;
import java.io.Serializable;

/**
* Classe que representa as linhas de encomenda
*/

public class LinhaEncomenda implements Serializable{
    private static final long serialVersionUID = -7257798564898604584L;
    private String codProdu;
    private String descricao;
    private double quantidade;
    private double preco;
    private double pesoUnidade;

    public LinhaEncomenda() {
        this.codProdu= "";
        this.descricao = "";
        this.preco = 0.0;
        this.quantidade=0.0;
        this.pesoUnidade=0.0;
    }


    public LinhaEncomenda(String cdP, String descricao, double preco, double quantidade, double pesoUnidade) {
        this.codProdu = cdP;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
        this.pesoUnidade = pesoUnidade;
    }

    public LinhaEncomenda(LinhaEncomenda l) {
        this.codProdu = l.getCodProdu();
        this.descricao = l.getDescricao();
        this.preco = l.getPreco();
        this.quantidade = l.getQuantidade();
        this.pesoUnidade = l.getPesoUnidade();
    }

    /**
     * Getter do código de produto da linha de encomenda
     */

    public String getCodProdu() {
        return this.codProdu;
    }

    /**
     * Getter da descrição da linha de encomenda
     */  

    public String getDescricao() {
        return this.descricao;
    }

    /**
     * Getter do preco da linha de encomenda
     */
    
    public double getPreco() {
        return this.preco;
    }

      /**
     * Getter da quantidade da linha de encomenda
     */
    

    public double getQuantidade() {
        return this.quantidade;
    }

    /**
     * Getter do peso por unidade da linha de encomenda
     */

    public double getPesoUnidade(){
        return this.pesoUnidade;
    }
    

    /**
    * Setter do codigo de produto da linha de encomenda
    */

    public void setCodProdu(String cdP) {
        this.codProdu = cdP;
    }

    /**
    * Setter da descricao da linha de encomenda
    */

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    /**
    * Setter do preco da linha de encomenda
    */

    public void setPreco(double preco) {
        this.preco = preco;
    }

    /**
    * Setter da quantidade da linha de encomenda
    */

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    /**
    * Setter do peso por unidade da linha de encomenda
    */

    public void setPesoUnidade(int pesoUni){
        this.pesoUnidade = pesoUni;
    }


    /**
    * Método que calcula o valor da linha de encomenda
    */

    public double ValorLinhaEnc() {
        return (this.preco) * this.quantidade;
    }

    /**
    * Método que retorna o peso da linha de encomenda
    */

    public double getPesoLinha(){
        return (this.quantidade)* this.pesoUnidade;
    }

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj != null && obj.getClass() == this.getClass()) {
            LinhaEncomenda le = (LinhaEncomenda)obj;
            return le.codProdu.equals(this.codProdu)
                    && le.descricao.equals(this.descricao)
                    && le.preco == this.preco
                    && le.quantidade==this.quantidade;

        } else {
            return false;
        }
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("LinhaEncomenda: ").append(this.codProdu).append(",")
                                     .append(this.descricao).append(",")
                                     .append(this.quantidade).append(",")
                                     .append(this.preco).append(",")
                                     .append(this.pesoUnidade);
        return sb.toString();
    }


    @Override
    public int hashCode() {
        return 1;
    }


}
