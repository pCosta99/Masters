package app.controllers;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import app.exceptions.CodigoProdutoJaExistenteException;
import app.exceptions.CodigoProdutoNaoExistenteException;
import app.models.Produto;

public class RegistosProdutos implements Serializable {


    /**
    *
    */
    private static final long serialVersionUID = -2449920089888234766L;
    // #region variables
    private Map<String, List<Produto>> produtos;
    // #endregion


    // #region Construtores
    public RegistosProdutos() {
        this.produtos = new HashMap<>();
    }

    /**
     * @param produtos
     */
    public RegistosProdutos(Map<String, List<Produto>> produtos) {
        this.setProdutos(produtos);
    }

    /**
     * @param r
     */
    public RegistosProdutos(RegistosProdutos r) {
        this.setProdutos(r.produtos);
    }
    // #endregion


    // #region getters setter
    /**
     * @return the produtos
     */
    public Map<String, List<Produto>> getProdutos() {
        return this.produtos.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    /**
     * @param produtos the produtos to set
     */
    public void setProdutos(Map<String, List<Produto>> produtos) {
        this.produtos = produtos.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }
    // #endregion


    // #region Overrrides

    @Override
    public boolean equals(Object o) {
        // self check
        if (this == o) {
            return true;
        }
        // null check
        if (o == null) {
            return false;
        }
        // type check and cast
        if (getClass() != o.getClass()) {
            return false;
        }
        RegistosProdutos registo = (RegistosProdutos) o;
        // field comparison
        return Objects.equals(this.produtos, registo.produtos);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Lista Produtos por Loja: ");
        sb.append('\n');
        for (Entry<String, List<Produto>> entry : this.produtos.entrySet()) {
            sb.append("Código da Loja: ");
            sb.append('\n');
            sb.append(entry.getKey());
            sb.append('\n');
            sb.append("Lista de Produtos: ");
            sb.append('\n');
            for (Produto p : entry.getValue()) {
                sb.append(p.toString());
                sb.append('\n');
            }
        }
        return sb.toString();
    }

    public RegistosProdutos clone() {
        return new RegistosProdutos(this);
    }
    // #endregion


    // #region Methods

    public void adicionaProduto(String codLoja, Produto p)
            throws CodigoProdutoJaExistenteException {
        if (existeProduto(codLoja, p.getCodProduto())) {
            throw new CodigoProdutoJaExistenteException("Código de produto já existente!");
        }
        List<Produto> lista = listaProdutos(codLoja);
        lista.add(p.clone());
        this.produtos.put(codLoja, lista);
    }

    public boolean existeProduto(String codLoja, String codProduto) {
        return listaProdutos(codLoja).stream()
                .anyMatch(pa -> pa.getCodProduto().equals(codProduto));
    }

    public Produto devolveProduto(String codLoja, String codProduto)
            throws CodigoProdutoNaoExistenteException {
        List<Produto> lista = listaProdutos(codLoja);
        Optional<Produto> produto =
                lista.stream().filter(p -> p.getCodProduto().equals(codProduto)).findAny();
        if (!produto.isPresent()) {
            throw new CodigoProdutoNaoExistenteException("Código de Produto não existente!");
        }
        return produto.get().clone();
    }

    public List<Produto> listaProdutos(String codLoja) {
        List<Produto> lista;
        if (this.produtos.containsKey(codLoja)) {
            lista = this.produtos.get(codLoja).stream().map(Produto::clone)
                    .collect(Collectors.toList());
        } else {
            lista = new ArrayList<>();
        }
        return lista;
    }

    public void removeProduto(String codLoja, String codProduto)
            throws CodigoProdutoNaoExistenteException {
        if (!this.existeProduto(codLoja, codProduto))
            throw new CodigoProdutoNaoExistenteException("Código de Produto não existente!");
        List<Produto> lista = listaProdutos(codLoja);
        Optional<Produto> produtoAntigo =
                lista.stream().filter(p -> p.getCodProduto().equals(codProduto)).findAny();
        if (produtoAntigo.isPresent()) {
            lista.remove(produtoAntigo.get());
        }
        this.produtos.put(codLoja, lista);
    }

    public void removeProduto(String codLoja, Produto p) throws CodigoProdutoNaoExistenteException {
        this.removeProduto(codLoja, p.getCodProduto());
    }

    public void atualizaProduto(String codLoja, Produto prod) {
        List<Produto> lista = listaProdutos(codLoja);
        Optional<Produto> produtoAntigo = lista.stream()
                .filter(p -> p.getCodProduto().equals(prod.getCodProduto())).findAny();
        if (produtoAntigo.isPresent()) {
            lista.remove(produtoAntigo.get());
        }
        lista.add(prod.clone());
        this.produtos.put(codLoja, lista);
    }


    // #endregion

}
