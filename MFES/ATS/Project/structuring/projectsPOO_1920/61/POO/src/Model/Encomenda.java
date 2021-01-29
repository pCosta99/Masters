package Model;

import java.io.Serializable;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe que define uma encomenda
 */
public class Encomenda implements Serializable {

    private String nome;
    private String numEnc;
    private LocalDateTime data;
    private LocalDateTime dataEntrega;
    private double peso;
    private List<LinhaEncomenda> elementos;
    private Transporte transp;
    private String loja;
    private boolean containsMed;
    private double preco;


    /**
     * Construtor da classe
     */
    public Encomenda() {
        nome = numEnc = null;
        data = null;
        dataEntrega = null;
        peso = 0.0;
        elementos = new ArrayList<>();
        transp = null;
        containsMed = false;
    }

    /**
     * Construtor da classe
     *
     * @param nome        O nome da encomenda
     * @param numEnc      O código do utilizador
     * @param data        A data de criação da encomenda
     * @param dataEntrega A data de entrega da encomenda
     * @param peso        O peso da encomenda
     * @param elementos   Os elementos da encomenda
     * @param transp      O transporte encarregue pela entrega da encomenda
     * @param loja        A loja onde a encomenda foi preparada
     * @param preco       Preço da encomenda
     */
    public Encomenda(String nome, String numEnc, LocalDateTime data, LocalDateTime dataEntrega, double peso, List<LinhaEncomenda> elementos, Transporte transp, String loja, boolean containsMed, double preco) {
        this.nome = nome;
        this.numEnc = numEnc;
        this.data = data;
        this.dataEntrega = dataEntrega;
        this.peso = peso;
        this.elementos = elementos;
        this.transp = transp;
        this.loja = loja;
        this.containsMed = containsMed;
        this.preco = preco;
    }
    /**
     * Construtor de classe
     *
     * @param e A encomenda da qual se pretende extrair a informação
     */
    public Encomenda(Encomenda e) {
        nome = e.getNome();
        numEnc = e.getNumEnc();
        data = e.getData();
        dataEntrega = e.getDataEntrega();
        peso = e.getPeso();
        elementos = e.getElementos();
        transp = e.getTransporte();
        loja = e.getLoja();
        containsMed = e.getContainsMed();
        preco = e.getPreco();
    }

    /**
     * Devolve o preço de uma encomenda sem considerar o peso
     * @return preço
     */
    public double getPreco() {
        return preco;
    }

    /**
     * Calcula o preço de uma encomenda considerando o peso
     * @return preco final de uma encomenda consoante o peso
     */
    public double geraPreco(){
        preco = ValorTotal() * (1 + peso/100);
        return preco;
    }

    /**
     * Define o preço de uma encomenda
     * @param preco preco da Encomenda
     */
    public void setPreco(double preco) {
        this.preco = preco;
    }

    /**
     * Indica se uma encomenda contém medicamentos
     * @return True caso afirmativo e false caso contrário
     */
    public boolean getContainsMed() {
        return containsMed;
    }

    /**
     * Define se uma encomenda contém medicamentos
     * @param containsMed Indicação da presença ou não de medicamentos
     */
    public void setContainsMed(boolean containsMed) {
        this.containsMed = containsMed;
    }

    /**
     * Indica a data de entrega
     *
     * @return A data de entrega
     */
    public LocalDateTime getDataEntrega() {
        return dataEntrega;
    }

    /**
     * Define a data de entrega
     *
     * @param d A data de entrega a definir
     */
    public void setDataEntrega(LocalDateTime d) {
        this.dataEntrega = d;
    }

    /**
     * Indica a loja onde foi preparada a encomenda
     *
     * @return A loja onde foi preparada a encomenda
     */
    public String getLoja() {
        return loja;
    }

    /**
     * Define a loja onde foi preparada a encomenda
     *
     * @param loja A loja onde foi preparada a encomenda
     */
    public void setLoja(String loja) {
        this.loja = loja;
    }

    /**
     * Indica o transporte encarregue pela entrega
     *
     * @return O transporte encarregue pela entrega
     */
    public Transporte getTransporte() {
        if(transp == null)
            return null;
        return transp.clone();
    }

    /**
     * Define o transporte encarregue pela entrega
     *
     * @param transp O transporte encarregue pela entrega
     */
    public void setTransporte(Transporte transp) {
        if(transp != null)
            this.transp = transp.clone();
    }

    /**
     * Indica o código do utilizador
     *
     * @return O o código do utilizador
     */
    public String getNome() {
        return nome;
    }

    /**
     * Indica o número da encomenda
     *
     * @return O número da encomenda
     */
    public String getNumEnc() {
        return numEnc;
    }

    /**
     * Indica a data de criação da encomenda
     *
     * @return A data de criação da encomenda
     */
    public LocalDateTime getData() {
        return data;
    }

    /**
     * Indica o peso da encomenda
     *
     * @return O peso da encomenda
     */
    public double getPeso() {
        return peso;
    }

    /**
     * Indica os elementos de uma encomenda
     *
     * @return Os elementos de uma encomenda
     */
    public ArrayList<LinhaEncomenda> getElementos() {
        ArrayList<LinhaEncomenda> l = new ArrayList<>();
        for (LinhaEncomenda aux : elementos) l.add(aux.clone());
        return l;
    }

    /**
     * Transforma uma encomenda numa String
     *
     * @return A String correspondente à encomenda
     */
    public String toString() {
        StringBuffer sb = new StringBuffer();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy - HH:mm:ss");
        DecimalFormat df = new DecimalFormat("0.00");

        sb.append("Encomenda:\n\n   Cliente = ")
                .append(nome)
                .append("\n   Encomenda = ")
                .append(numEnc)
                .append("\n   Data = ")
                .append(data.format(formatter))
                .append("\n   Transporte = ")
                .append(transp.getCode())
                .append("\n   Peso = ")
                .append(df.format(peso)).append("Kg")
                .append("\n   Preço = ")
                .append(df.format(preco)).append("€")
                .append("\n   Encomenda Médica = ")
                .append(containsMed)
                .append("\n\n")
                .append(String.format("%20s", "Produtos"))
                .append(String.format("%10s", "Preços"))
                .append(String.format("%15s", "Quantidade"))
                .append("\n");

        for (LinhaEncomenda enc : elementos) {
            sb.append(enc).append("\n");
        }

        return sb.toString();
    }

    /**
     * Define o código do utilizador
     *
     * @param s O código do utilizador
     */
    public void setNome(String s) {
        nome = s;
    }

    /**
     * Define o número da encomenda
     *
     * @param s O número a ser definido
     */
    public void setNumEnc(String s) {
        numEnc = s;
    }

    /**
     * Define a data de entrega da encomenda
     *
     * @param s A data de entrega da encomenda
     */
    public void setData(LocalDateTime s) {
        data = s;
    }

    /**
     * Define o peso de uma encomenda
     *
     * @param p O peso a ser definido
     */
    public void setPeso(double p) {
        peso = p;
    }

    /**
     * Define os elementos de uma encomenda
     *
     * @param l Os elementos a serem definidos
     */
    public void setElementos(List<LinhaEncomenda> l) {
        this.elementos = new ArrayList<LinhaEncomenda>();
        for (LinhaEncomenda aux : l) elementos.add(aux.clone());
    }

    /**
     * Cria um clone da encomenda
     *
     * @return O clone da encomenda
     */
    public Encomenda clone() {
        return new Encomenda(this);
    }

    /**
     * Indica se um objeto é igual a uma encomenda
     *
     * @param o O objeto com o qual se pretende comparar a encomenda
     * @return True caso o objeto seja igual à encomenda e false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda encomenda = (Encomenda) o;
        return peso == encomenda.peso &&
                nome.equals(encomenda.nome) &&
                numEnc.equals(encomenda.numEnc);
    }

    /**
     * Indica o custo de uma encomenda
     *
     * @return O custo de uma encomenda
     */
    public double ValorTotal() {
        double r = 0;
        for (LinhaEncomenda aux : elementos) r += aux.calculaValorLinhaEnc();
        return r;
    }

    /**
     * Indica o desconto aplicado a uma encomenda
     *
     * @return O desconto aplicado a uma encomenda
     */
    public double DescontoTotal() {
        double r = 0;
        for (LinhaEncomenda aux : elementos) r += aux.calculaValorDesconto();
        return r;
    }

    /**
     * Indica a quantidade de produtos de uma encomenda
     *
     * @return A quantidade de produtos de uma encomenda
     */
    public int numProdutos() {
        int r = 0;
        for (LinhaEncomenda aux : elementos) r += aux.getQuantidade();
        return r;
    }

    /**
     * Verifica se um dado produto pertence à encomenda
     *
     * @param ref A referência do produto
     * @return True caso afirmativo e false caso contrário
     */
    public boolean existeProd(String ref) {
        for (LinhaEncomenda aux : elementos) if (aux.getReferencia().equals(ref)) return true;
        return false;
    }

    /**
     * Indica o número de elementos de uma encomenda
     *
     * @return O número de elementos de uma encomenda
     */
    public int numeroElementos() {
        return this.elementos.size();
    }

    /**
     * Adiciona uma linha de encomenda a uma encomenda
     *
     * @param l A linha de encomenda a ser adicionada
     */
    public void adicionaLinha(LinhaEncomenda l) {
        elementos.add(l.clone());
    }

    /**
     * Remove uma linha de encomenda a uma encomenda
     *
     * @param ref A linha de encomenda a ser removida
     */
    public void removeLinha(String ref) {
        for (LinhaEncomenda aux : elementos) {
            if (aux.getReferencia().equals(ref)) {
                elementos.remove(aux);
                break;
            }
        }
    }


}