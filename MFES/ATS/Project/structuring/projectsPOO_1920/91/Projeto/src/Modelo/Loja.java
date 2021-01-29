/**
 * Classe que representa a loja, onde vendem os pordutos
 */
package Modelo;

import java.io.Serializable;
import java.util.*;

public class Loja implements Serializable {
    private String codLoja;
    private String nomeLoja;
    private Gps gps;
    private boolean disponivel;
    private String password;
    private Set<Produto> catalogo;
    private List<String> encomendas ;

    //CONTRUTORES

    /**
     * Construtor vazio
     */
    public Loja(){
        this.codLoja = "Invalid";
        this.nomeLoja = "Invalid";
        this.gps = new Gps();
        this.disponivel = false;
        this.password = "Invalid";
        this.catalogo = new TreeSet<>();
        this.encomendas = new ArrayList<>();
        }

    /**
     * Construtor parametrizado
     * @param codLoja codigo que representa a loja
     * @param nomeLoja nome da loja
     * @param gps cordenadas gps
     * @param bool disponiblidade da loja
     * @param pass password usada para aceder a loja
     * @param catalogo catalogo de produtos da loja
     * @param encomendas
     */
    public Loja(String codLoja, String nomeLoja, Gps gps , boolean bool, String pass, Set<Produto> catalogo,List<String>encomendas) {
        this.codLoja = codLoja;
        this.nomeLoja = nomeLoja;
        this.gps = gps.clone();
        this.disponivel = bool;
        this.password = pass;
        this.catalogo = new TreeSet<>();
        for (Produto s :
                catalogo ) {
            this.catalogo.add(s);
        }
        this.encomendas = new ArrayList<>();
        for (String s :
                encomendas ) {
            this.encomendas.add(s);
        }

    }

    /**
     * Contrutor cópia
     * @param outro
     */
    public Loja( Loja outro) {
        this.codLoja = outro.getCodLoja();
        this.nomeLoja = outro.getNomeLoja();
        this.gps = outro.getGps().clone();
        this.disponivel = outro.isDisponivel();
        this.password = outro.getPassword();
        this.catalogo = new TreeSet<>();
        for (Produto s :
                outro.getCatalogo() ) {
            this.catalogo.add(s);
        }
        this.encomendas = new ArrayList<>();
        for (String s :
                outro.getEncomendas() ) {
            this.encomendas.add(s);
        }
    }

    //GETTERS E SETTERS

    /**
     * Método que recebe o codigo da loja
     * @return
     */
    public String getCodLoja() {
        return this.codLoja;
    }

    /**
     * Método que modifica o codigo da loja
     * @param codLoja
     */
    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    /**
     * Método que devolve o nome da loja
     * @return nome da loja
     */
    public String getNomeLoja() {
        return this.nomeLoja;
    }

    /**
     * Método que modifica o nome da loja
     * @param nomeLoja novo nome da loja
     */
    public void setNomeLoja(String nomeLoja) {
        this.nomeLoja = nomeLoja;
    }

    /**
     * Método que devolve as cordenadas gps
     * @return gps
     */
    public Gps getGps(){
        return this.gps.clone();
    }

    /**
     * Método que modifica as cordenadas gps
     * @param gps novo gps
     */
    public void setGps(Gps gps){
        this.gps = gps.clone();
    }


    /**
     * Método que modifica a disponiblidade para o contrario
     */
    public void sinalizarDisponibilidade (){
        if(this.disponivel) this.disponivel = false;
        else this.disponivel = true;
    }

    /**
     * Método que devolve a password
     * @return pass
     */
    public String getPassword(){
        return this.password;
    }

    /**
     * Método que modifica a password
     * @param pass nova pass
     */
    public void setPassword(String pass){
        this.password = pass;
    }

    /**
     * Método que devolve a disponiblidade de uma loja
     * @return disponiblidade
     */
    public boolean isDisponivel() {
        return disponivel;
    }

    /**
     * Método que modifica a disponiblidade da loja
     * @param disponivel novo estado de disponivel
     */
    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    /**
     * Método que devolve a lista de encomendas
     * @return encomendas
     */
    public List<String> getEncomendas(){
        return new ArrayList<>(encomendas);
    }

    /**
     * Método que devolve o catalogo da loja
     * @return catalogo da loja
     */
    public Set<Produto> getCatalogo(){
        return new TreeSet<>(catalogo);
    }

    /**
     * Método equals
     * @param obj
     * @return
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj==null && obj.getClass() != this.getClass()) return false;
        Loja a = (Loja) obj;
        return this.nomeLoja.equals(a.getNomeLoja()) && this.codLoja.equals(a.getCodLoja()) && this.gps.equals(a.getGps()) && this.disponivel == a.disponivel;
    }

    /**
     * Método clone
     * @return clone
     */
    @Override
    public Loja clone() {
        return new Loja(this);
    }

    /**
     * Método toString
     * @return String
     */
    @Override
    public String toString() {
        return "Código da loja: " + this.codLoja + " Nome da loja: " + this.nomeLoja + "Coordenadas:\n X: " + this.gps.getX() + "\n Y: " + this.gps.getY() + "\n";
    }

    /**
     * Método que adiciona uma encomenda a lista de encomendas da loja
     * @param codEnc
     */
    public void addEncomenda(String codEnc){
        this.encomendas.add(codEnc);
    }

    /**
     * Método que adiciona um produto ao catalogo de uma loja
     * @param produto
     */
    public void addProduto (Produto produto){
        this.catalogo.add(produto);
    }

    /**
     * Método que adiciona produtos ao catalogo
     * @param produtos
     */
    public void addProdutos(Set<Produto> produtos){
        this.catalogo.addAll(produtos);
    }

}
