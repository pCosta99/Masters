public class LinhaEncomenda {

    //representa uma ordem de encomenda de um
    //único produto.
    private String idProduto;
    private String descricao;
    private double quantidade;
    private double preco;


    //construtor vazio
    
    public LinhaEncomenda(){
        this.idProduto = "";
        this.descricao = "";
        this.quantidade = 0.0;
        this.preco = 0.0;

    }

    //construtor parametrizado

    public LinhaEncomenda (String idP, String descricao, double quantidade, double preco){
        this.idProduto = idP;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.preco = preco;

    }

    //construtor de cópia
    
    public LinhaEncomenda (LinhaEncomenda l){
        this.idProduto = l.getIdProduto();
        this.descricao = l.getDescricao();
        this.quantidade = l.getQuantidade();
        this.preco = l.getPreco();

    }
    
    //metodo que devolve o ID do produto associado à linha de encomenda em causa

    public String getIdProduto() {
        return idProduto;
    }

    //metodo que dá set ao ID do produto associado à linha de encomenda em causa
    
    public void setIdProduto(String idProduto) {
        this.idProduto = idProduto;
    }
    
    //metodo que devolve a descrição da linha de encomenda em causa

    public String getDescricao() {
        return descricao;
    }
    
    //metodo que dá set à descrição da linha de encomendas em causa

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }
    
    //metodo que dá set da quantidade de produto na linha de encomenda em causa

    public double getQuantidade() {
        return quantidade;
    }
    
    //metodo que dá set à quantidade de produto na linha de encomenda em causa

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    //metodo que devolve o preço por unidade da linha de encomenda em causa
    
    public double getPreco() {
        return preco;
    }
    
    //metodo que dá set ao preço por unidade da linha de encomenda em causa

    public void setPreco(double preco) {
        this.preco = preco;
    }


    //metodo que verifica se 2 objetos sao iguais

    public boolean equals (Object o){
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        LinhaEncomenda l = (LinhaEncomenda) o;
        return (this.idProduto.equals(l.getIdProduto()) &&
                this.descricao.equals(l.getDescricao()) &&
                this.quantidade == l.getQuantidade() &&
                this.preco == l.getPreco() );
    }

    //metodo que devolve a classe numa string

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("Linha de Encomenda: ") .append(this.idProduto)
                                         .append("\n")
                                         .append(this.descricao)
                                         .append("\n")
                                         .append(this.quantidade)
                                         .append("\n")
                                         .append(this.preco)
                                         .append("\n");
        return s.toString();
    }
    
    //metodo que devolve um clone da classe em causa

    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }



}
