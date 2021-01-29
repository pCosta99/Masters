package Model;

import java.io.Serializable;
import java.util.*;

public class Encomenda implements Serializable, IEncomenda {
    private String id;
    private String loja;
    private String userId;
    private double peso;
    private Set<String> estafeta;
    private double preco;
    private ArrayList<LinhaEncomenda> produtos;
    private String tipo;
    private int tempo;

    public Encomenda(String id, String loja, String userId, String tipo, double preco, double peso, Set<String> estafeta, ArrayList<LinhaEncomenda> produtos, int tempo) {
        this.id = id;
        this.loja = loja;
        this.userId = userId;
        this.peso = peso;
        this.estafeta = estafeta;
        this.produtos = produtos;
        this.preco = preco;
        this.tipo = tipo;
        this.tempo = tempo;
    }

    public Encomenda() {
        this.id = null;
        this.loja = null;
        this.userId = null;
        this.peso = 0;
        this.estafeta = new HashSet<>();
        this.tipo = null;
        this.produtos = new ArrayList<>();
        this.preco = 0;
        this.tempo = 0;
    }

    public int getTempo() {
        return tempo;
    }

    public void setTempo(int tempo) {
        this.tempo = tempo;
    }

    public double getPreco() {
        return preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public String getId() {
        return id;
    }

    public String getLoja() {
        return loja;
    }

    public String getUserId() {
        return userId;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public String getTipo() {
        return tipo;
    }

    public void setTipo(String tipo) {
        this.tipo = tipo;
    }

    public double getPeso() {
        return peso;
    }

    public Set<String> getEstafeta() {
        return estafeta;
    }

    public ArrayList<LinhaEncomenda> getProdutos() {
        return produtos;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setLoja(String loja) {
        this.loja = loja;
    }

    public void setEstafeta(Set<String> estafeta) {
        this.estafeta = estafeta;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda encomenda = (Encomenda) o;
        return Double.compare(encomenda.peso, peso) == 0 &&
                Objects.equals(id, encomenda.id) &&
                Objects.equals(tipo, encomenda.tipo) &&
                Objects.equals(loja, encomenda.loja) &&
                Objects.equals(userId, encomenda.userId) &&
                Objects.equals(estafeta, encomenda.estafeta) &&
                Objects.equals(produtos, encomenda.produtos);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, loja, userId, peso, estafeta, produtos, tipo);
    }

    @Override
    public String toString() {
        return "Encomenda{" +
                "id='" + id + '\'' +
                ", loja='" + loja + '\'' +
                ", userId='" + userId + '\'' +
                ", estafeta='" + estafeta + '\'' +
                '}';
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    public void setProdutos(ArrayList<LinhaEncomenda> produtos) {
        this.produtos = produtos;
    }

    public void addProdutos(LinhaEncomenda p) {
        produtos.add(p);
    }

    public ArrayList<LinhaEncomenda> addProdutosFromString (String s){
        ArrayList<LinhaEncomenda> l = new ArrayList<>();
        int i =0;
        int r = 0;
        String[] parts = s.split(",", 20);
        while (r<20) {
            LinhaEncomenda le = new LinhaEncomenda();

            le.setCodProd(parts[r]);
            le.setDescricao(parts[r+1]);
            le.setPreco(Double.parseDouble(parts[r+2]));

            l.add(le);

            r++;
        }
        return l;
    }

    public boolean existeProduto(String codProd) {
        for (LinhaEncomenda f : produtos) {
            if (f.getCodProd().equals(codProd)) return true;
        }
        return false;
    }

    public double getPrecoTot () {
        double i = 0;
        for (LinhaEncomenda f : produtos) {
            i += f.getPreco();
        }
        return i;
    }

    public double getPesoTot () {
        double i = 0;
        for (LinhaEncomenda f : produtos) {
            i += f.getPeso();
        }
        return i;
    }
}

