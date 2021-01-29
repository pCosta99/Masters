import java.io.Serializable;
import java.net.PortUnreachableException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class LinhaEncomenda  extends Produto implements Serializable {
    private List<Produto> produtos;

    public LinhaEncomenda(){
        this.produtos = new ArrayList<>();
    }

    public LinhaEncomenda(List<Produto> produtos){
        setLinhaEncomenda(produtos);
    }

    public LinhaEncomenda(LinhaEncomenda le){
        this.produtos = le.getProdutos();
    }


    private void setLinhaEncomenda(List<Produto> produtos) {
        this.produtos = new ArrayList<>();
        for(Produto p: produtos){
            this.produtos.add(p.clone());
        }
    }

    public List<Produto> getProdutos() {
        return this.produtos.stream().map(Produto::clone).collect(Collectors.toList());
    }
    public void addProduto(String codProduto, String nomeProduto, double quantidade, double valorUnitario, boolean tipoMedico, double peso){
        Produto p = new Produto(codProduto,nomeProduto,quantidade,valorUnitario,tipoMedico,peso);
        this.produtos.add(p.clone());
    }

    public void addProduto(Produto p){
        this.produtos.add(p.clone());
    }

    public double calculaPeso(){
        double res = 0.0;
        for(Produto p : this.produtos){
            res += p.getQuantidade()*p.getPeso();
        }
        return res;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LinhaEncomenda that = (LinhaEncomenda) o;
        return Objects.equals(produtos, that.produtos);
    }

    @Override
    public int hashCode() {
        return Objects.hash(produtos);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.produtos).append("\n");
        return sb.toString();
    }

    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }

}