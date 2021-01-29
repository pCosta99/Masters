package model;

import java.io.Serializable;

public class ProdutoMedico extends Produto implements Serializable {

    private String idEspecial;
    private String nome;


    public ProdutoMedico() {
        super();
        this.idEspecial = new String();

    }

    public ProdutoMedico(double preco,double peso,String idloja,int quantidade, String idspecial,String nome) {
        super(preco,idloja,peso,quantidade);
        this.idEspecial=idspecial;
        this.nome = nome;
    }



    public ProdutoMedico(ProdutoMedico prod) {
        super(prod);
        setIdESpecial(prod.getIdESpecial());
    }

    public void setIdESpecial(String id) {
        this.idEspecial = id;
    }

    public String getIdESpecial() {
        return this.idEspecial;
    }

    public String getNome() {
        return this.nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public ProdutoMedico clone() {
        return new ProdutoMedico(this);
    }


    public boolean equals(Object obj){
        if (obj == this) return true;

        if (obj == null || obj.getClass() != this.getClass()) return false;

        ProdutoMedico prod = (ProdutoMedico) obj;

        return super.equals(prod) && super.equals(obj)
                && this.getIdESpecial().equals(prod.getIdESpecial());

    }


    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getNome()).append("\n");
        sb.append(this.getIdESpecial()).append("\n");
        sb.append(this.getIdLoja()).append("\n");
        sb.append(this.getPreco()).append("\n");
        sb.append(this.getPeso()).append("\n");
        sb.append((this.getQuantidade())).append("\n");
        return sb.toString();
    }
}