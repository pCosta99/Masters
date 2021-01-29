public class LinhaEncomenda{
    /** Codigo do produto **/
    private String codProduto;
    /** Nome do produto **/
    private String nomeProduto;
    /** Quantidade de produto **/
    private double qtdProduto;
    /** Preço do produto **/
    private double precoProduto;

    /** Construtor nulo **/
    public LinhaEncomenda(){
        this.codProduto = "";
        this.nomeProduto = "";
        this.qtdProduto = 0.0;
        this.precoProduto = 0.0;
    }

    /** Construtor parametrizado para a classe LinhaEncomenda **/
    public LinhaEncomenda(String cP,String nP, double qtd, double pc){
        this.codProduto = cP;
        this.nomeProduto = nP;
        this.qtdProduto = qtd;
        this.precoProduto = pc;
    }

    /** Construtor de cópia **/
    public LinhaEncomenda(LinhaEncomenda le){
        this.codProduto = le.getCodProduto();
        this.qtdProduto = le.getQtdProduto();
        this.precoProduto = le.getPrecoProduto();
    }

    /** Retorna o Codigo do Produto **/
    public String getCodProduto(){
        return this.codProduto;
    }

    /** Retorna o nome do Produto **/
    public String getNomeProduto(){
        return this.nomeProduto;
    }

    /** Retorna a quantidade de Produto **/
    public double getQtdProduto(){
        return this.qtdProduto;
    }

    /** Retorna o preço do Produto **/
    public double getPrecoProduto(){
        return this.precoProduto;
    }

    /** Define o codigo do Produto **/
    public void setCodProduto(String cPd){
        this.codProduto = cPd;
    }

    /** Define o nome do Produto **/
    public void setNomeProduto(String nPd){
        this.nomeProduto = nPd;
    }

    /** Define a quantidade de Produto **/
    public void setQtdProduto(double qtdP){
        this.qtdProduto = qtdP;
    }

    /** Define o preço do Produto **/
    public void setPrecoProduto(double pco){
        this.precoProduto = pco;
    }

    /** Método que clona uma Linha de Encomenda **/
    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }

    /** Método que cria uma string com a informação da Encomenda **/
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo do Produto: ").append(this.codProduto+"\n");
        sb.append("Nome do Produto: ").append(this.nomeProduto+"\n");
        sb.append("Preço do Produto: ").append(this.precoProduto+"\n");
        sb.append("Quantidade de Produto: ").append(this.qtdProduto+"\n");
        return sb.toString();
    }
}