package TrazAqui;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
/**
 * Classe que representa uma Encomenda TrazAqui!
 */
public class Encomenda  implements Comparable, Serializable {
    /**
     * String código da Encomenda
     */
    private String cod;
    /**
     *  String código da Loja
     */
    private String loja;
    /**
     * String código do Utilizador
     */
    private String user;
    /**
     * Peso da Encomenda
     */
    private float peso;
    /**
     * Data da realização da encomenda
     */
    private LocalDateTime tempo;
    /**
     * Lista de linhas de Encomenda
     */
    private List<LinhaEncomenda> lstProdutos;

    /**
     * Construtor vazio de um objeto Encomenda
     */
    public Encomenda() {
        this.cod = "";
        this.loja = "";
        this.user = "";
        this.peso = 0.f;
        this.lstProdutos = new ArrayList<>();
        this.tempo = LocalDateTime.now();
    }
    /**
     * Construtor parametrizado de um objeto Encomenda
     * @param cod Codigo da Encomenda
     * @param loja Loja em que o objeto Encomenda se encontra
     * @param user User que realizou esta Encomenda
     * @param peso Peso da encomenda
     * @param lst Lista dos produtos que estão na Encomenda
     * @param tempo Dia e horas a que a Encomenda foi realizada
     */
    public Encomenda(String cod, String loja, String user, float peso, List<LinhaEncomenda> lst, LocalDateTime tempo) {
        this.cod = cod;
        this.loja = loja;
        this.user = user;
        this.peso = peso;
        this.tempo = tempo;
        this.lstProdutos = lst.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());
    }
    /**
     * Construtor copia de um objeto Encomenda
     * @param encomenda Encomenda
     */
    public Encomenda(Encomenda encomenda) {
        this.cod = encomenda.getCod();
        this.loja = encomenda.getLoja();
        this.user = encomenda.getUser();
        this.peso = encomenda.getPeso();
        this.lstProdutos = encomenda.getLstProdutos();
        this.tempo = encomenda.getTempo();
    }


    //Getters

    /**
     * Método para obter o Codigo
     * @return cod String
     */
    public String getCod() {
        return cod;
    }
    /**
     * Método para obter a Loja
     * @return loja String
     */
    public String getLoja() {
        return loja;
    }
    /**
     * Método para obter o User
     * @return user String
     */
    public String getUser() {
        return user;
    }
    /**
     * Método para obter o Peso
     * @return peso float
     */
    public float getPeso() {
        return peso;
    }
    /**
     * Método para obter a Lista de produtos
     * @return lstProdutos List produtos
     */
    public List<LinhaEncomenda> getLstProdutos() {
        return lstProdutos.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());
    }
    /**
     * Método para obter o LocalDateTime de quando a encomenda foi realizada
     * @return tempo LocalDateTime
     */
    public LocalDateTime getTempo() {
        return tempo;
    }

    //Setters

    /**
     * Método para dar set do cod
     * @param cod String
     * */
    public void setCod(String cod) {
        this.cod = cod;
    }
    /**
     * Método para dar set da Loja
     * @param loja String
     * */
    public void setLoja(String loja) {
        this.loja = loja;
    }
    /**
     * Método para dar set do User
     * @param user String
     * */
    public void setUser(String user) {
        this.user = user;
    }
    /**
     * Método para dar set do Peso
     * @param peso float
     * */
    public void setPeso(float peso) {
        this.peso = peso;
    }

    /**
     * Método para dar set do tempo
     * @param tempo LocalDateTime
     * */
    public void setTempo(LocalDateTime tempo) {
        this.tempo = tempo;
    }
    /**
     * Não existe método set da lstProdutos porque para nós, não faz sentido criar um set
     * de algo que vai estar sempre a ser atualizado
     * */


    /**
     * Método que cria uma String de um objeto em concreto (Neste caso de Endomenda)
     */
    public String toString() {
        final StringBuilder sb = new StringBuilder("Encomenda: ");
        sb.append(cod).append('\n');
        sb.append(" >Loja: ").append(loja).append('\n');
        sb.append(" >Utilizador: ").append(user).append('\n');
        sb.append(" >Peso: ").append(peso).append('\n');
        sb.append(" >Data de encomenda: ").append(tempo).append('\n');
        sb.append(" >Linhas de Encomenda:\n");
        for(LinhaEncomenda le : this.lstProdutos){
            sb.append(le.toString());
        }
        sb.append('\n');
        return sb.toString();
    }
    /**
     * Método clone
     */
    @Override
    public Encomenda clone(){
        return new Encomenda(this);
    }

    /**
     * Método para verificar se um certo objeto é igual a Encomenda
     * @return boolean
     * @param o Object
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Encomenda)) return false;

        Encomenda encomenda = (Encomenda) o;

        if (Float.compare(encomenda.getPeso(), getPeso()) != 0) return false;
        if (getCod() != null ? !getCod().equals(encomenda.getCod()) : encomenda.getCod() != null) return false;
        if (getLoja() != null ? !getLoja().equals(encomenda.getLoja()) : encomenda.getLoja() != null) return false;
        if (getUser() != null ? !getUser().equals(encomenda.getUser()) : encomenda.getUser() != null) return false;
        if (getLstProdutos() != null ? !getLstProdutos().equals(encomenda.getLstProdutos()) : encomenda.getLstProdutos() != null) return false;
        return getTempo() != null ? getTempo().equals(encomenda.getTempo()) : encomenda.getTempo() == null;
    }


    /**
     * Método que dado uma Encomenda vai calcular, através de uma stream, o total da sua lstProdutos
     * @return double Preço total de um Encomenda
     */
    public double getPreco(){
        return this.lstProdutos.stream().map(LinhaEncomenda::calculaValorLinhaEnc).reduce((double) 0, Double::sum);
    }

    /**
     * Método que vai comparar um certo objeto "o" com a encomenda e return em forma de int
     * qual deles foi realizado primeiro (comparando os seus LocalDateTime)
     * @param o Objeto
     *
     */
    @Override
    public int compareTo(Object o) {
        Encomenda x  =(Encomenda) o;
        return this.tempo.compareTo(x.getTempo());
    }
    /**
     * Método que adiciona a uma Encomenda uma LinhaEncomenda, isto é, add na sua lstProdutos
     * @param le LinhaEncomenda a ser adicionada a esta encomenda
     */
    public void addLinhaEncomenda(LinhaEncomenda le){
        this.lstProdutos.add(le);
    }
    /**
     * Método que através de uma stream, vai calcular o preco de toda a lstEncomenda, chamando tambem o
     * método calculaValorLinhaEnc em cada LinhaEncomenda da lstEncomenda
     * @return double valor total da lstEncomenda
     */
    public double calculaValorTotalEncomenda(){
         return this.lstProdutos.stream().map(LinhaEncomenda::calculaValorLinhaEnc).reduce((double) 0 ,Double::sum);
    }
}
