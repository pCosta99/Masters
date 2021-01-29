import java.io.Serializable;

public class Produto implements Serializable{
    private String referencia;
    private String descricao;
    private float preco;
    private int quantidade;
    private float peso;

    public Produto() {
        this.referencia = "n/a";
        this.descricao = "n/a";
        this.preco = 0;
        this.quantidade = 0;
        this.peso = 0;
    }

    public Produto(String referencia, String descricao, float preco, int quantidade, float peso) {
        this.referencia = referencia;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
        this.peso = peso;
    }

    public Produto(Produto p) {
        this.referencia = p.getReferencia();
        this.descricao = p.getDescricao();
        this.preco = p.getPreco();
        this.quantidade = p.getQuantidade();
        this.peso = p.getPeso();
    }

    public float calculaValorTotal() {
        return this.quantidade * this.preco;
    }

    public float calculaValorpeso() {
        return this.quantidade * this.peso;
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

    public float getPreco() {
        return this.preco;
    }

    public void setPreco(float preco) {
        this.preco = preco;
    }

    public int getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    public float getPeso() {
        return this.peso;
    }

    public void setPeso(float peso) {
        this.peso = peso;
    }

    public Produto clone() {
        return new Produto(this);
    }

    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Produto p = (Produto) obj;
        return p.getReferencia().equals(this.referencia) &&
                p.getDescricao().equals(this.descricao) &&
                p.getPreco() == this.preco && p.getQuantidade() == this.quantidade;
    }
    
    public String toString(){
        return "\nDescrição: " + this.descricao + ", Preço: " + this.preco + "€" + ", Quantidade: " + this.quantidade + ", Peso: " + this.peso + "kg";
    }
}
