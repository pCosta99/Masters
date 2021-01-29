package model;

import java.io.Serializable;

public class ProdutoNormal extends Produto implements Serializable {

    private String idNormal;
    private String nome;

    public ProdutoNormal() {
        super();
        this.idNormal = new String();

    }

    public ProdutoNormal(double preco,double peso,String idloja,int quantidade, String str,String nome) {
        super(preco,idloja,peso,quantidade);
        this.idNormal = str;
        this.nome = nome;
    }

    public ProdutoNormal(ProdutoNormal prod) {
        super(prod);
        setIdNormal(prod.getIdNormal());
        setNome(prod.getNome());

    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setIdNormal(String id) {
        this.idNormal = id;
    }

    public String getIdNormal() {
        return this.idNormal;
    }

    public ProdutoNormal clone() {
        return new ProdutoNormal(this);
    }


    public boolean equals(Object obj){
        if (obj == this) return true;

        if (obj == null || obj.getClass() != this.getClass()) return false;

        ProdutoMedico prod = (ProdutoMedico) obj;

        return super.equals(prod) && super.equals(obj)
                    && this.getIdNormal().equals(prod.getIdESpecial());

    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getNome()).append("\n");
        sb.append(this.getIdNormal()).append("\n");
        sb.append(this.getIdLoja()).append("\n");
        sb.append(this.getPreco()).append("\n");
        sb.append(this.getPeso()).append("\n");
        sb.append((this.getQuantidade())).append("\n");
        return sb.toString();
    }
}