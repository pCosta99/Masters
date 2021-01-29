package Models;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Classe que guarda informação sobre uma encomenda
 *
 */
public class Encomenda implements Serializable {
    private String loja, user, enc;
    private Set<Produto> produtos;
    private double peso;

    public Encomenda(String Loja, String User, String nome, double peso) {
        this.loja = Loja;
        this.user = User;
        this.enc = nome;
        this.peso = peso;
        this.produtos = new HashSet<>();
    }

    public Encomenda(String loja, String user, String nome, Collection<Produto> fds) {
        this.loja = loja;
        this.user = user;
        this.enc = nome;
        this.peso = 999;
        this.produtos = new HashSet<>(fds);
    }

    public Encomenda(Encomenda e) {
        this.loja = e.getLoja();
        this.user = e.getUser();
        this.enc = e.getEnc();
        this.peso = e.getPeso();
        this.setProdutos(e.getProdutos());

    }

    public void add_linha(Produto l) {
        this.produtos.add(l);
    }

    public String getLoja() {
        return loja;
    }

    public void setLoja(String loja) {
        this.loja = loja;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getEnc() {
        return enc;
    }

    public void setEnc(String enc) {
        this.enc = enc;
    }

    public double getPeso() {
        return peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public Set<Produto> getProdutos() {
        return new HashSet<>(this.produtos);
    }

    public void setProdutos(Set<Produto> s) {
        this.produtos = new HashSet<>(s);
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

    public String toString() {
        return "\n[Encomenda " + this.enc +
                "]\nUtilizador - " + this.user +
                "\nLoja - " + this.loja +
                "\nPeso: " + this.peso;
        // o string fica excessivamente longo se se incluir a linhaencomenda
    }
}
