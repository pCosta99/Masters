import java.util.*;
import java.io.*;

public class Artigo implements Serializable{

    private String id;
    private String descricao;
    private float peso; //quantidade
    private float valor;

    public Artigo(){
        this.id = new String();
        this.descricao = new String();
        this.peso = 0;
        this.valor = 0;
    }

    public Artigo(String id, String descricao, float peso, float valor) {
        this.id = id;
        this.descricao = descricao;
        this.peso = peso;
        this.valor = valor;
    }

    public Artigo(Artigo a){
        this.id = a.getId();
        this.descricao = a.getDescricao();
        this.peso = a.getPeso();
        this.valor = a.getValor();
    }

    public String getId() {
        return this.id;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public float getPeso() {
        return this.peso;
    }

    public float getValor(){
        return this.valor;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public void setPeso(float peso) {
        this.peso = peso;
    }

    public void setValor(){
        this.valor = valor;
    }

    public String toString() {
        return "Artigo{" +
                "id=" + this.id +
                ", descricao='" + this.descricao + '\'' +
                ", peso=" + this.peso +
                ", valor=" + this.valor +
                '}';
    }

    public boolean equals(Object obj){
        if(this == obj) return true;
        if ((obj == null) || ( this.getClass()!= obj.getClass())) return false;
        Artigo a = (Artigo) obj;
        return (this.id.equals(a.getId()) &&
                this.descricao == a.getDescricao() &&
                this.peso == a.getPeso() &&
                this.valor == a.getValor());
    }

    public Artigo clone(){ return new Artigo(this);}
}
