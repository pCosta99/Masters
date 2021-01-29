package MVC.Models.BaseModels;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Representação de uma Encomenda.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */
public class Encomenda implements Serializable {
    private String codEnc;
    private String codUser;
    private String codLoja;
    private double peso;
    private List<LinhaEncomenda> linhas;
    private double duracao;
    private double distancia;
    private double preco;
    private int classificacao;
    private Boolean medica;
    private String codEntregador;

    /**
     * Construtor Encomenda por defeito.
     */
    public Encomenda(){
        this.codEnc = "";
        this.codUser = "";
        this.codLoja = "";
        this.codEntregador = "";
        this.peso = 0;
        this.linhas = new ArrayList<>();
        this.duracao = 0;
        this.distancia = 0;
        this.preco = 0;
        this.classificacao = 0;
        this.medica = false;
    }


    /**
     * Construtor de Encomenda parametrizado.
     * @param cenc Encomenda.
     * @param cuser Utilizador.
     * @param cloja Loja.
     * @param p Peso da Encomenda.
     * @param nLinhas Linhas de MEncomenda.
     */
    public Encomenda(String cenc, String cuser, String cloja, double p, List<LinhaEncomenda> nLinhas){
        this.codEnc = cenc;
        this.codUser = cuser;
        this.codLoja = cloja;
        this.codEntregador = "";
        this.peso = p;
        this.setLinhas(nLinhas);
        this.duracao = 0;
        this.distancia = 0;
        this.preco = 0;
        this.classificacao = 0;
        this.medica = false;
    }


    /**
     * Construtor de Encomenda por Cópia.
     * @param e Encomenda a copiar.
     */
    public Encomenda(Encomenda e){
        this.codEnc = e.getCodEnc();
        this.codUser = e.getCodUser();
        this.codLoja = e.getCodLoja();
        this.codEntregador = e.getCodEntregador();
        this.peso = e.getPeso();
        this.setLinhas(e.getLinhas());
        this.duracao = e.getDuracao();
        this.distancia = e.getDistancia();
        this.preco = e.getPreco();
        this.classificacao = e.getClassificacao();
        this.medica = e.getMedica();
    }

    /**
     * Método que devolve o Código da Encomenda.
     * @return Código da Encomenda.
     */
    public String getCodEnc() {
        return this.codEnc;
    }

    /**
     * Método que devolve o Código do Utilizador.
     * @return Código do Utilizador.
     */
    public String getCodUser() {
        return this.codUser;
    }

    /**
     * Método que devolve o Código da Loja.
     * @return Código da Loja.
     */
    public String getCodLoja() {
        return this.codLoja;
    }

    /**
     * Método que devolve o Peso da Encomenda.
     * @return Peso da Encomenda.
     */
    public double getPeso() {
        return peso;
    }

    /**
     * Metódo que devolve uma Lista com todas as LinhasEncomenda de uma Encomenda.
     * @return Lista resultante.
     */
    public List<LinhaEncomenda> getLinhas() {
        List<LinhaEncomenda> aux = new ArrayList <>();
        for (LinhaEncomenda ls : this.linhas)
            aux.add(ls.clone());
        return aux;
    }

    /**
     * Método que define a Lista de LinhaEncomenda de uma Encomenda.
     * @param linhas Lista com LinhaEncomenda.
     */
    public void setLinhas(List<LinhaEncomenda> linhas){
        this.linhas = new ArrayList<>();
        for (LinhaEncomenda ls : linhas) {
            this.linhas.add(ls);
        }
    }

    /**
     * Método que devolve a Duração de Entrega da Encomenda.
     * @return Duração da Entrega.
     */
    public double getDuracao() {
        return this.duracao;
    }

    /**
     * Método que define a Duração de Entrega da Encomenda.
     * @param duracao Duração da Entrega.
     */
    public void setDuracao(double duracao) {
        this.duracao = duracao;
    }

    /**
     * Método que devolve a distância entre a Loja e o Utilizador.
     * @return Distância resultante.
     */
    public double getDistancia() {
        return this.distancia;
    }

    /**
     * Método que define a distância entre a Loja e o Utilizador.
     * @param distancia Distância Resultante.
     */
    public void setDistancia(double distancia) {
        this.distancia = distancia;
    }

    /**
     * Método que devolve o Preço da Encomenda.
     * @return Preço da Encomenda.
     */
    public double getPreco() {
        return this.preco;
    }

    /**
     * Método que define o Preço da Encomenda.
     * @param preco Preço da Encomenda.
     */
    public void setPreco(double preco) {
        this.preco = preco;
    }

    /**
     * Método que devolve a Classificação da Encomenda.
     * @return Classificação da Encomenda.
     */
    public int getClassificacao() {
        return this.classificacao;
    }

    /**
     * Método que define a Classificação da Encomenda.
     * @param nota Classificação da Encomenda.
     */
    public void setClassificacao(int nota) {
        this.classificacao = nota;
    }

    /**
     * Método que devolve se a Encomenda é uma Encomenda médica.
     * @return True caso seja, false caso contrário.
     */
    public Boolean getMedica() {
        return this.medica;
    }

    /**
     * Método que define se a Encomenda é uma Encomenda Médica.
     * @param med True caso seja uma Encomenda Médica, false caso contrário.
     */
    public void setMedica(Boolean med) {
        this.medica = med;
    }

    /**
     * Método que devolve o Código do Entregador.
     * @return Código do Entregador.
     */
    public String getCodEntregador(){
        return this.codEntregador;
    }

    /**
     * Método que define o Código do Entregador.
     * @param ent Código do Entregador.
     */
    public void setCodEntregador(String ent){
        this.codEntregador = ent;
    }

    /**
     * Método que calcula o Valor Total da Encomenda.
     * @return Valor Total.
     */
    public double calculaValorTotal(){
        double valor = 0;
        for (LinhaEncomenda l : this.linhas) {
            valor += l.calculaValorLinhaEnc();
        }
        return valor;
    }

    /**
     * Método que calcula o número Total de Produtos presentes na Encomenda.
     * @return Número de Produtos.
     */
    public int numeroTotalProdutos(){
        int total = 0;
        for (LinhaEncomenda l : this.linhas) {
            total += l.getQuantidade();
        }
        return total;
    }

    /**
     * Verifica se um determinado Produto se encontra numa Encomenda.
     * @param refProduto Código do Produto.
     * @return True caso esteja na encomenda, false caso contrário.
     */
    public boolean existeProdutoEncomenda(String refProduto){
        boolean b = false;
        for (LinhaEncomenda l : this.linhas){
            if (refProduto.equals(l.getCodigo())){
                b = true;
            }
        }
        return b;
    }

    /**
     * Método que adiciona uma LinhaEncomenda à encomenda.
     * @param linha LinhaEncomenda a adicionar.
     */
    public void adicionaLinha(LinhaEncomenda linha){
        this.linhas.add(linha.clone());
    }

    /**
     * Método equals.
     * @param obj Object a comparar.
     * @return True caso sejam iguais, false caso contrário.
     */
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Encomenda e = (Encomenda) obj;
        return e.getCodEnc().equals(this.codEnc);
    }

    /**
     * Método toString.
     * @return String com todos os dados da Encomenda.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        int horas = (int) this.duracao;
        int min = (int) ((this.duracao - horas) * 60);
        sb.append("Encomenda{").append("\nCódigo Encomenda: ").append(this.codEnc).append("\nCódigo User: ").append(this.codUser)
          .append("\nCódigo Loja: ").append(this.codLoja).append("\nPeso: ").append(String.format("%.2f", this.peso)).append("\nDuracao: ")
          .append(horas).append("h :").append(min).append(" min")
          .append("\nPreco: ").append(String.format("%.2f", this.preco)).append("\nLinhas: ").append(this.linhas)
          .append("\nCodigo do Entregador: ").append(this.codEntregador)
          .append("\nClassificacao:").append(this.classificacao).append("}\n");
        return sb.toString();
    }

    /**
     * Método Clone.
     * @return Encomenda Clonada.
     */
    public Encomenda clone() {
        return new Encomenda (this);
    }
}