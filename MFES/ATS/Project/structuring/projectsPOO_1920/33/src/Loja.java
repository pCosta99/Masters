import java.io.Serializable;
import java.util.*;

public class Loja extends Empresa implements Serializable {

    private List<Produto> produtos;


    public Loja(){
        super();
        this.produtos = new ArrayList<>();
    }

    public Loja(String codLoja, String nomeLoja, double gpsx, double gpsy, String email, String password,List<Produto> p){
        super(codLoja,nomeLoja,gpsx,gpsy,email,password);
        setProdutos(p);
    }

    public Loja(Loja l){
        super(l);
        setProdutos(l.getProdutos());
    }


    public void setProdutos(List<Produto> p){
        this.produtos = new ArrayList<>();
        p.stream().forEach(pAux -> this.produtos.add(pAux.clone()));
    }

    public List<Produto> getProdutos(){
        List<Produto> aux = new ArrayList<>();
        this.produtos.stream().forEach(p -> aux.add(p.clone()));
        return aux;
    }


    public void adicionaProduto(Produto p)throws ProdutoRepetidoException {
        if (this.produtos.contains(p))
            throw new ProdutoRepetidoException("Produto repetido.");
        else {
            this.produtos.add(p.clone());
        }
    }


    public Produto compraDeProduto(String codProd, double quantidade) throws QuantidadeEmExcessoException,ProdutoInexistenteException{
        Produto pp = new Produto();
        if(this.produtos.stream().filter(p -> p.getCodProduto().equals(codProd)).count() ==1){
            for(Produto p : this.produtos){
                if(p.getCodProduto().equals(codProd)) {
                    p.retiraQuantidade(quantidade);
                    pp = new Produto(codProd,p.getNomeProduto(),quantidade,p.getValorUnitario(),p.getTipoMedico(),p.getPeso());
                }
            }
        } else throw new ProdutoInexistenteException("O codigo de produto nao existe !!");
        return pp;
    }

    public Loja clone() {
        return new Loja(this);
    }


    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(" Loja{ \n");
        sb.append(super.toString()).append(", ");
        sb.append("Produtos: ").append(produtos.toString());
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Loja that = (Loja) o;
        return Objects.equals(that.produtos, produtos);
    }
}

