package Models;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que guarda informações sobre a loja
 *
 */
public class Loja implements Serializable {
    private final Map<String, Boolean> estado;
    private final List<Produto> produtos;
    private String cod, nome;
    private double coordenadaX, coordenadaY;


    public Loja(String cod, String n, double x, double y, String tipo) {

        ListaProdutos fornecedores = new ListaProdutos();

        this.cod = cod;
        this.nome = n;
        this.coordenadaX = x;
        this.coordenadaY = y;
        this.estado = new HashMap<>();


        this.produtos = fornecedores.generateList(tipo);

    }

    public static Loja getLoja_by_name(String loja, Collection<Loja> lojas) {
        for (Loja l : lojas) {
            if (l.getNome().equals(loja)) return l;
        }
        return null;
    }

    public List<Produto> getProductList() { return this.produtos; }

    public String getCod() {
        return cod;
    }

    public void setCod(String cod) {
        this.cod = cod;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getCoordenadaX() {
        return coordenadaX;
    }

    public void setCoordenadaX(double coordenadaX) {
        this.coordenadaX = coordenadaX;
    }

    public double getCoordenadaY() {
        return coordenadaY;
    }

    public void setCoordenadaY(double coordenadaY) {
        this.coordenadaY = coordenadaY;
    }

    public void request(String codEnc) {
        this.estado.put(codEnc, false);
    }

    public Boolean ready(String codEnc) {
        return this.estado.replace(codEnc, true);
    }

    public Boolean deliver(String codEnc) {
        return this.estado.remove((codEnc));
    }

    /**
     * Devolve a lista de todas as encomendas prontas a entregar
     *
     * @return lista com os codigos das encomendas prontas
     */
    public List<String> readyEncs() {
        List<String> res = new ArrayList<>();
        for (Map.Entry<String, Boolean> e : this.estado.entrySet()) {
            if (e.getValue()) res.add(e.getKey());
        }
        return res;
    }

}
