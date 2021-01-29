package Model.Catalogos;

import Model.Encomendas.*;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Objects;

public class CatalogoProds implements ICatalogoProds, Serializable {
    private HashMap<String,IProduto> catalogo;

    public CatalogoProds(){
        this.catalogo = new HashMap<>();
    }

    public CatalogoProds(CatalogoProds p){
        this.catalogo = p.getCatProds();
    }

    public HashMap<String,IProduto> getCatProds() {
        return this.catalogo;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CatalogoProds)) return false;
        CatalogoProds that = (CatalogoProds) o;
        return Objects.equals(catalogo, that.catalogo);
    }

    @Override
    public int hashCode() {
        return Objects.hash(catalogo);
    }

    public String toString() {
        return "Catalogo dos produtos\n" +
                catalogo;
    }

    public void insereProd(IProduto produto){
        this.catalogo.put(produto.getCodProduto(),produto);
    }

    public boolean existsProd (IProduto produto) { return(this.catalogo.containsValue(produto)); }

    public int totalProds(){ return (this.catalogo.size()); }

    public IProduto getProd (String codProduto){
        IProduto prod = new Produto();
        prod = catalogo.get(codProduto);
        return prod;
    }

    public boolean existsProdStr(String codProd) {
         return this.catalogo.containsKey(codProd);
    }

}
