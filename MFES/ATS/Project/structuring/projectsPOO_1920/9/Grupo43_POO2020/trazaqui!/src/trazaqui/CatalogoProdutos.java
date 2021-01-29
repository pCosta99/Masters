package trazaqui;

import trazaqui.Exceptions.ProdutoNaoExisteException;

import java.io.Serializable;
import java.util.ArrayList;

public class CatalogoProdutos implements Serializable {
    private String codLoja;
    private ArrayList<Produto> produtos;

    public CatalogoProdutos(){
        this.codLoja="";
        this.produtos= new ArrayList<Produto>();
    }

    public CatalogoProdutos(String cod,ArrayList<Produto> p){
        this.codLoja=cod;
        this.produtos=new ArrayList<>();
        for(Produto s : p){
            this.produtos.add(s);
        }
    }

    public CatalogoProdutos(CatalogoProdutos e){
        this.codLoja=e.getCodLoja();
        setProdutos(e.getProdutos());
    }

    public ArrayList<Produto> getProdutos(){
        return new ArrayList<>(this.produtos);
    }

    public void setProdutos(ArrayList<Produto> prod){
        this.produtos= new ArrayList<>(prod);
    }

    public String getCodLoja(){
        return this.codLoja;
    }

    public void setCodLoja(String cod) {
        this.codLoja = cod;
    }

    public Produto getProduto(String cod) throws ProdutoNaoExisteException {
        for(Produto p : this.produtos){
            if(p.getCodProd().equals(cod)){
                return p;
            }
        }
        throw new ProdutoNaoExisteException();
    }

    public CatalogoProdutos clone() {
        return new CatalogoProdutos(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Loja:").append(this.codLoja).append("\n");
        sb.append("Produtos:").append(this.produtos).append("\n");
        return sb.toString();
    }

}
