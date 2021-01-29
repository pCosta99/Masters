import java.util.*;
import java.io.*;

public class Encomenda implements Serializable{

    private String id;
    private String utilizador;
    private String loja;
    private String transporte;
    private Map <String,Artigo> artigos;
    private float peso;
    private float valor;
    private int estado;

    public Encomenda(){
        this.id = new String();
        this.utilizador = new String();
        this.loja = new String();
        this.transporte = new String();
        this.artigos = new HashMap<>();
        this.peso = 0;
        this.valor = 0;
        this.estado = 0;
    }

    public Encomenda(String id, String utilizador, String loja, String transporte, Map<String, Artigo> artigos, float peso, float valor, int estado) {
        this.id = id;
        this.utilizador = utilizador;
        this.loja = loja;
        this.transporte = transporte;
        this.artigos = artigos;
        this.peso = peso;
        this.valor = valor;
        this.estado = estado;
    }

    public Encomenda(Encomenda p){
        this.id = p.getId();
        this.utilizador = p.getUtilizador();
        this.loja = p.getLoja();
        this.transporte = p.getTransporte();
        this.artigos = p.getArtigos();
        this.peso = p.getPeso();
        this.valor = p.getValor();
        this.estado = p.getEstado();
    }

    public String getId() {
        return this.id;
    }

    public String getUtilizador() {
        return this.utilizador;
    }

    public String getLoja() {
        return this.loja;
    }

    public String getTransporte() {
        return this.transporte;
    }

    public Map<String, Artigo> getArtigos() {
        return this.artigos;
    }

    public float getPeso(){
        return this.peso;
    }

    public float getValor() {
        return this.valor;
    }

    public int getEstado(){
        return this.estado;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setUtilizador(String utilizador) {
        this.utilizador = utilizador;
    }

    public void setLoja(String loja) {
        this.loja = loja;
    }

    public void setTransporte(String transporte) {
        this.transporte = transporte;
    }

    public void setArtigos(Map<String, Artigo> encomendas) {
        this.artigos = encomendas;
    }

    public void setPeso(float peso){
        this.peso = peso;
    }

    public void setValor(float valor) {
        this.valor = valor;
    }

    public void setEstado(int estado){
        this.estado = estado;
    }

    public String toString() {
        return "Encomenda{" +
                "id=" + this.id +
                ", utilizador='" + this.utilizador + '\'' +
                ", loja='" + this.loja + '\'' +
                ", transporte='" + this.transporte + '\'' +
                ", encomendas=" + this.artigos +
                ", peso=" + this.peso +
                ", valor=" + this.valor +
                ", estado=" + this.estado +
                '}';
    }

    public boolean equals(Object obj){
        if(this == obj) return true;
        if ((obj == null) || ( this.getClass()!= obj.getClass())) return false;
        Encomenda p = (Encomenda) obj;
        return (this.id.equals(p.getId()) &&
                this.utilizador.equals(p.getUtilizador()) &&
                this.loja.equals(p.getLoja()) &&
                this.transporte.equals(p.getTransporte()) &&
                this.artigos.equals(p.getArtigos()) &&
                this.peso == p.getPeso() &&
                this.valor == p.getValor() &&
                this.estado == p.getEstado());
    }

    public Encomenda clone(){ return new Encomenda(this);}

    public void adicionarArtigo(Artigo a) throws ArtigoInvalidoException{
        if(!this.artigos.containsKey(a.getId())){
            this.artigos.put(a.getId(),a);
            this.peso += a.getPeso();
            this.valor += a.getValor();
        }
        else throw new ArtigoInvalidoException("Artigo nao existe");
    }
}
