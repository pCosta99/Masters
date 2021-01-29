package TrazAqui;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe que representa todas as encomendas do sistema.
 */
public class Encomenda implements Serializable {
    private double peso;
    private String cod;
    private LocalDateTime data;
    private String utilizador;
    private String loja;
    private boolean medicamentos;
    private String estafeta;
    private List<LinhaEncomenda> produtos;

    /**
     * Construtor parametrizado de uma Encomenda.
     * @param peso Peso da encomenda.
     * @param med Informa se é uma encomenda médica ou não.
     * @param cod Código da encomenda.
     * @param utilizador Utilizador que pediu a encomenda.
     * @param loja  Loja de onde foi pedida a encomenda.
     * @param produtos  List de produto(s) que são pedidos na encomenda.
     * @param l Data da encomenda.
     * @param estafeta  Estafeta que transporta a encomenda da loja até ao utilizador.
     */
    public Encomenda(double peso, boolean med, String cod, String utilizador, String loja,ArrayList<LinhaEncomenda> produtos,LocalDateTime l,String estafeta) {
        this.peso = peso;
        this.cod = cod;
        this.utilizador = utilizador;
        this.loja = loja;
        this.produtos = produtos;
        this.data = l;
        this.medicamentos = med;
        this.estafeta =estafeta;
    }

    /**
     * Construtor vazio de uma Encomenda.
     */
    public Encomenda() {
        this.peso = 0;
        this.cod = "";
        this.utilizador = "";
        this.loja = "";
        this.medicamentos = false;
        this.produtos = new ArrayList<>();
        this.data = LocalDateTime.now();
        this.estafeta="";
    }

    /**
     * Construtor por cópia de uma Encomenda.
     * @param e Encomenda que pretendemos copiar.
     */
    public Encomenda(Encomenda e) {
        this.peso = e.getPeso();
        this.data = e.getData();
        this.produtos = e.getProdutos();
        this.loja = e.getLoja();
        this.utilizador = e.getUtilizador();
        this.cod = e.getCod();
        this.medicamentos = e.getMedicamentos();
        this.estafeta= e.getEstafeta();
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

    /**
     * Transforma uma Encomenda numa string.
     * @return Encomenda em string.
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Encomenda{");
        sb.append("Transportador='").append(estafeta).append('\'');
        sb.append(", peso='").append(peso).append('\'');
        sb.append(", cod='").append(cod).append('\'');
        sb.append(", data=").append(data);
        sb.append(", utilizador='").append(utilizador).append('\'');
        sb.append(", loja='").append(loja).append('\'');
        sb.append(", medicamentos=").append(medicamentos);
        sb.append(", produtos=").append(produtos);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Método para comparar encomendas.
     * @param o Encomenda a comparar.
     * @return Booleano que diz se são iguais ou não.
     */
    public boolean equals(Object o) {

        if (this == o) return true;
        if (o==null || this.getClass().equals(o.getClass())) return true;
        Encomenda e = (Encomenda) o;

        return this.peso==e.getPeso() &&
                this.cod.equals(e.getCod()) &&
                this.utilizador.equals(e.getUtilizador()) &&
                this.loja.equals(e.getLoja()) &&
                this.produtos.equals(e.getProdutos()) &&
                this.medicamentos==e.getMedicamentos() &&
                this.data.equals(e.getData())  &&
                this.estafeta.equals(e.getEstafeta());
    }

    /**
     * Setter de produtos que são pedidos numa encomenda.
     * @param produtos Produtos que pretendemos inserir na encomenda.
     */
    public void setProdutos(List<LinhaEncomenda> produtos) {
        this.produtos = new ArrayList<>();
        for (LinhaEncomenda e : produtos) {
            this.produtos.add(e.clone());
        }
    }

    /**
     * Getter da informação que nos diz se a encomenda é médica ou não.
     * @return Booleano que diz se a encomenda é médica ou não.
     */
    public boolean getMedicamentos() {
        return medicamentos;
    }

    /**
     * Setter de uma encomenda para médica ou não.
     * @param medicamentos Booleano que queremos dar.
     */
    public void setMedicamentos(boolean medicamentos) {
        this.medicamentos = medicamentos;
    }

    /**
     * Getter da data da encomenda.
     * @return Data da encomenda.
     */
    public LocalDateTime getData() {
        return data;
    }

    /**
     * Setter da data da encomenda.
     * @param data Data da encomenda.
     */
    public void setData(LocalDateTime data) {
        this.data = data;
    }

    /**
     * Getter do peso da encomenda.
     * @return Peso da encomenda.
     */
    public double getPeso() {
        return this.peso;
    }

    /**
     * Setter do peso da encomenda.
     * @param peso Peso que pretendemos dar à encomenda.
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Getter do código da encomenda.
     * @return Código da encomenda.
     */
    public String getCod() {
        return cod;
    }

    /**
     * Setter do código da encomenda.
     * @param cod Código da encomenda.
     */
    public void setCod(String cod) {
        this.cod = cod;
    }

    /**
     * Getter do utilizador que pediu a encomenda.
     * @return Utilizador.
     */
    public String getUtilizador() {
        return this.utilizador;
    }

    /**
     * Setter do utilizador que pediu a encomenda.
     * @param utilizador Utilizador.
     */
    public void setUtilizador(String utilizador) {
        this.utilizador = utilizador;
    }

    /**
     * Getter da loja onde a encomenda foi pedida.
     * @return Loja.
     */
    public String getLoja() {
        return this.loja;
    }

    /**
     * Setter da loja de onde a encomenda foi pedida.
     * @param loja Loja.
     */
    public void setLoja(String loja) {
        this.loja = loja;
    }

    /**
     * Getter dos produtos pedidos numa encomenda.
     * @return Produtos pedidos numa encomenda.
     */
    public ArrayList<LinhaEncomenda> getProdutos() {
        ArrayList<LinhaEncomenda> res = new ArrayList<>();
        for (LinhaEncomenda l : this.produtos) {
            res.add(l.clone());
        }
        return res;
    }

    /**
     * Setter dos produtos pedidos numa encomenda.
     * @param produtos Produtos pedidos numa encomenda.
     */
    public void setProdutos(ArrayList<LinhaEncomenda> produtos) {
        this.produtos = new ArrayList<>();
        for (LinhaEncomenda l : produtos) {
            this.produtos.add(l.clone());
        }
    }

    /**
     * Getter do estafeta que transportou a encomenda.
     * @return Estafeta.
     */
    public String getEstafeta() {
        return estafeta;
    }

    /**
     * Setter do estafeta que transportou a encomenda.
     * @param estafeta Estafeta.
     */
    public void setEstafeta(String estafeta) {
        this.estafeta = estafeta;
    }

    /**
     * Adiciona um produto à lista de produtos pedidos numa encomenda.
     * @param l Produto que vamos adicionar.
     */
    public void addProduto(LinhaEncomenda l) {
        this.produtos.add(l.clone());
    }

    /**
     * Remove um produto à lista de produtos pedidos numa encomenda.
     * @param l Produto que pretendemos remover.
     */
    public void removeProduto(LinhaEncomenda l) {
        int i,size = this.produtos.size();
        for (i=0; i<size; i++) {
            if (this.produtos.get(i).equals(l)) {
                this.produtos.remove(i);
            }
        }
    }

}