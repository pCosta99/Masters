public class LinhaEncomenda {
    private String codProduto;
    private String descricao;
    private float quantidade;
    private float valorUnitario;
    private float pesoUnitario;
    
    public LinhaEncomenda() {
        this.codProduto = "n/a";
        this.descricao = "n/a";
        this.quantidade = 0;
        this.valorUnitario = 0;
        this.pesoUnitario = 0;
    }
    
    public LinhaEncomenda(String codP, String desc,float quant,float vU, float pU) {
        this.codProduto = codP;
        this.descricao = desc;
        this.quantidade = quant;
        this.valorUnitario = vU;
        this.pesoUnitario = pU;
    }
    
    public LinhaEncomenda(LinhaEncomenda le) {
        this.codProduto = le.getCodProduto();
        this.descricao = le.getDescricao();
        this.quantidade = le.getQuantidade();
        this.valorUnitario = le.getValorUnitario();
        this.pesoUnitario = le.getPesoUnitario();
    }


    public String getCodProduto() {
        return this.codProduto;
    }
    
    public void setCodProduto(String cod) {
        this.codProduto = cod;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public void setDescricao(String des) {
        this.descricao = des;
    }

    public float getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(float quant) {
        this.quantidade = quant;
    }

    public float getValorUnitario() {
        return this.valorUnitario;
    }

    public void setValorUnitario(float vU) {
        this.valorUnitario = vU;
    }

    public float getPesoUnitario() {
        return this.pesoUnitario;
    }

    public void setPesoUnitario(float pU) {
        this.pesoUnitario = pU;
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
              le.getQuantidade()==this.quantidade &&
              le.getValorUnitario()==this.valorUnitario &&
              le.getPesoUnitario()==this.pesoUnitario;
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("codProduto: ").append(this.codProduto).append("; ").
             append("Descricao: ").append(this.descricao).append("; ").
             append("Quantidade: ").append(this.quantidade).append("; ").
             append("Valor Unitario: ").append(this.valorUnitario).append("; ").
             append("Peso Unitario: ").append(this.pesoUnitario).append("; ").append("\n");
        return sb.toString();
    }            
    
    public float getPesoTotal (){
        return (float) (this.quantidade * this.pesoUnitario);
    }

    public float getPrecoTotal (){
        return (float) (this.quantidade * this.valorUnitario);
    }
}