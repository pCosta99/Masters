import java.io.Serializable;

public class LinhaEncomenda implements Serializable {
    private String codigo;
    private final String descricao;
    private final double quantidade;
    private final double preco;

    public LinhaEncomenda(String codigo, String descricao, double quantidade, double  preco){
        this.codigo = codigo;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
    }

    public LinhaEncomenda(LinhaEncomenda a){
        this.codigo = a.getCodigo();
        this.descricao = a.getDescricao();
        this.preco = a.getPreco();
        this.quantidade = a.getQuantidade();
    }

    public String getCodigo(){
        return this.codigo;
    }

    public double getPreco() {
        return this.preco;
    }

    public double getQuantidade() {
        return this.quantidade;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public void setCodigo(String codigo){
        this.codigo = codigo;
    }

    public LinhaEncomenda clone(){

        return (new LinhaEncomenda(this));
    }


    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda o = (LinhaEncomenda) obj;

        return this.codigo.equals(o.getCodigo()) &&
                this.descricao.equals(o.getDescricao()) &&
                this.preco == o.getPreco() &&
                this.quantidade == o.getQuantidade();
    }



    public String toString(){
        return "\n" +
                "Código de produto: " +
                this.codigo + "\n" +
                "Produto: " +
                this.descricao + "\n" +
                "Quantidade: " +
                this.quantidade + "\n" +
                "Preço: " +
                this.preco + "\n";
    }

    public boolean isMed(){
        return descricao.equals("Alcool") || descricao.equals("Desinfetante") || descricao.equals("Saco de lixo 30l") || descricao.equals("Saco de lixo de 50l");
    }


}
