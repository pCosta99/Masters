package Model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Write a description of class Model.Loja here.
 *
 * @author (grupo 49)
 */

/**
 * Classe que guarda toda a informação de uma loja
 */
public class Loja implements Serializable
{
    private String pass;
    private String cod;
    private String nome;
    private GPS gps;
    private List<Encomenda> listaEnc;
    private List<Encomenda> prontas;
    private Set<Produto> stock;
    private int fila;

    /**
     * Construtor sem parametros
     */
    public Loja()
    {
        this.pass = new String();
        this.cod = new String();
        this.nome = new String();
        this.gps = new GPS();
        this.listaEnc = new ArrayList<>();
        this.prontas = new ArrayList<>();
        this.stock = new TreeSet<>();
        this.fila = -1;

    }

    /**
     * Construtor parametrizado
     * @param s         String com codigo de loja
     * @param n         String com nome
     * @param gps       String com gps
     * @param li        Lista com todas as encomendas feitas
     * @param prontas   Lista com todas as encomendas em espera para serem aceites
     */
    public Loja (String s, String n, GPS gps, ArrayList<Encomenda> li, ArrayList<Encomenda> prontas)
    {
        this.pass = s;
        this.cod = s;
        this.nome = n;
        this.gps = gps.clone();
        this.setListaEnc(li);
        this.setProntas(prontas);
        this.stock = new TreeSet<>();
        this.fila = -1;
    }

        /**
     * Construtor parametrizado
     * @param p     String com pass
     * @param s     String com codigo
     * @param n     String com nome
     * @param gps   GPS
     * @param stock TreeSet Geral com todos os produtos do sistema
     */

    public Loja (String p, String s, String n, GPS gps, TreeSet<Produto> stock, int fila)
    {
        this.pass = p;
        this.cod = s;
        this.nome = n;
        this.gps = gps.clone();
        this.listaEnc = new ArrayList<>();
        this.prontas = new ArrayList<>();
        this.setStock(stock);
        this.fila = fila;
    }

    /**
     * Construtor parametrizado
     * @param p     String com pass
     * @param s     String com codigo
     * @param n     String com nome
     * @param gps   GPS
     */
    public Loja (String p, String s, String n, GPS gps, int fila)
    {
        this.pass = p;
        this.cod = s;
        this.nome = n;
        this.gps = gps.clone();
        this.listaEnc = new ArrayList<>();
        this.prontas = new ArrayList<>();
        this.stock = new TreeSet<>();
        this.fila = -1;
    }

    /**
     * Construtor por copia
     * @param u     Loja a copiar
     */
    public Loja (Loja u)
    {
        this.pass = u.getPass();
        this.cod = u.getCod();
        this.nome = u.getNome();
        this.gps = new GPS(u.getGPS());
        this.setListaEnc(u.getListaEnc());
        this.setProntas(u.getProntas());
        this.setStock(u.getStock());
    }

    public int getFila() {
        return fila;
    }

    public void setFila(int fila) {
        this.fila = fila;
    }

    /**
     * Get da variavel listenc do objeto
     * @return      Lista com todas as encomendas
     */
    public ArrayList<Encomenda> getListaEnc() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for (Encomenda l : this.listaEnc)
            aux.add(l);
        return aux;
    }

    /**
     * Get da variavel prontas do objeto
     * @return      Lista com todas as encomendas prontas
     */
    public ArrayList<Encomenda> getProntas() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for (Encomenda l : this.prontas)
            aux.add(l);
        return aux;
    }

    /**
     * Get da variavel stock do objeto
     * @return      Set com todas os produtos disponiveis na loja
     */
    public TreeSet<Produto> getStock(){
        return new TreeSet<>(stock);
    }

    /**
     * Set da variavel stock do objeto
     * @param p     Set com todos os produtos
     */
    public void setStock(TreeSet<Produto> p){
        this.stock = new TreeSet<>();
        for (Produto pi: p){
            stock.add(pi);
        }
    }

    /**
     * Set da variavel stock do objeto
     * @param l     Lista com todas as encomendas
     */
    public void setListaEnc (ArrayList<Encomenda> l)
    {
        this.listaEnc = new ArrayList<>();
        for(Encomenda li : l)
            this.listaEnc.add(li);
    }

    /**
     * Set da variavel stock do objeto
     * @param l     Lista com todas as encomendas prontas
     */
    public void setProntas (ArrayList<Encomenda> l)
    {
        this.prontas = new ArrayList<>();
        for(Encomenda li : l)
            this.prontas.add(li);
    }


    /**
     * Set da variavel pass do objeto
     * @param p     String com pass
     */
    public void setPass(String p){this.pass = p;}

    /**
     * Get da variavel pass do objeto
     * @return      String com pass
     */
    public String getPass(){return this.pass;}

    /**
     * Get da variavel cod do objeto
     * @return      String com cod
     */
    public String getCod()
    {
        return this.cod;
    }
    /**
     * Get da variavel nome do objeto
     * @return      String com nome
     */

    public String getNome()
    {
        return this.nome;
    }

    /**
     * Get da variavel gps do objeto
     * @return      GPS
     */
    public GPS getGPS()
    {
        return this.gps;
    }

    /**
     * Set da variavel cod do objeto
     * @param s     String com cod
     */
    public void setCod(String s)
    {
        this.cod = s;
    }

    /**
     * Set da variavel nome do objeto
     * @param n     String com nome
     */
    public void setNome(String n)
    {
        this.nome = n;
    }

    /**
     * Set da variavel gps do objeto
     * @param l     double com latitude
     * @param lo    double com longitude
     */
    public void setGPS(double l, double lo)
    {
        this.gps.setGPS(l,lo);
    }

    /**
     * Método que clona este objeto
     * @return      clone do objeto
     */
    public Loja clone()
    {
        return new Loja(this);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Model.Loja{");
        sb.append("cod='").append(cod).append('\'');
        sb.append(", nome='").append(nome).append('\'');
        sb.append(", gps=").append(gps);
        sb.append(", listaEnc=").append(listaEnc);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Método equals do objeto
     * @param o     Objeto a comparar
     * @return      boolean
     */
    public boolean equals (Object o)
    {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Loja u = (Loja) o;
        return this.cod.equals(u.getCod()) &&
               this.nome.equals(u.getNome()) &&
               this.gps.equals(u.getGPS());
    }

    public void addEncomenda(String e, String u, String[]ps, int []qts){
        ArrayList<LinhaEncomenda> linhas = new ArrayList<>();
        double p=0;
        for(int i=0;i<ps.length;i++){
//            if(stock.contains(ps[i])){
//                Model.LinhaEncomenda linha = new Model.LinhaEncomenda(stock.get(ps[i]).getCod(), ps[i], qts[i],qts[i] * stock.get(ps[i]).getPeso(), stock.get(ps[i]).getPreçouni() * qts[i]);
//                linhas.add(linha);
//            }
            for(LinhaEncomenda li : linhas) {
                p += li.getPeso();
            }
            Encomenda enc = new Encomenda (e,this.cod,u,p,linhas);
            this.listaEnc.add(enc);

        }
    }

    /**
     * Método que adiciona ume encomenda a variavel listenc
     * @param e     Encomenda a adiconar
     */
    public void addEncomenda(Encomenda e){
        this.listaEnc.add(e);
    }

    /**
     * Método que aiciona uma encomenda a variavel listEnc atraves do parse de um ficheiro
     * @param e     Encomenda a adicionar
     */
    public void addEncomendaParse(Encomenda e) {
        if (!listaEnc.contains(e)) {
            listaEnc.add(e);
        }
    }

    /**
     * Método que altera a informação quando uma encomenda é entregue
     * @param e     Encomenda entregue
     * @param f     Data da entrega
     */
    public void setEntregue(Encomenda e, LocalDateTime f){
        Iterator<Encomenda> it = this.listaEnc.iterator();
        boolean r = false;
        while(it.hasNext() && !r){
            Encomenda enc = it.next();
            if(enc.equals(e)) {
                enc.setDataf(f);
                enc.setEntregue(true);
                r=true;
            }
        }
    }

    /**
     * Método que verifica se duas pass sao iguais
     * @param p     String com pass
     * @return      boolean
     */
    public boolean validaPass(String p){
        return this.pass.equals(p);
    }

    /**
     * Método que devolve uma encomenda da variavel listenc
     * @param e     String com codigo de encomenda
     * @return      Encomenda
     */
    public Encomenda getEncomenda(String e) {
        Encomenda enc = new Encomenda();
        Iterator<Encomenda> it = listaEnc.iterator();
        boolean found = false;
        while(it.hasNext() && !found){
            Encomenda en = it.next();
            if(en.getCodenc().equals(e)){
                found = true;
                enc = en.clone();
            }

        }
        return enc;
    }

    /**
     * Método que adiciona uma encomenda a variavel prontas
     * @param e     Encomenda a adicionar
     */
    public void addEncPronta(Encomenda e){
        this.prontas.add(e);
    }
    /**
     * Método que remove uma encomenda a variavel prontas
     * @param e     Encomenda a remover
     */
    public void rmEncPronta(Encomenda e){
        this.prontas.remove(e);
    }

    /**
     * Método que adiciona um set de produtos a variavel stock
     * @param p     Set a adicionar
     */
    public void addProdutos(TreeSet<Produto> p){
        stock.addAll(p);
    }

    /**
     * Método que adiciona um produto a variavel stock
     * @param p     produto a adicionar
     */
    public void addProduto(Produto p){stock.add(p);}

    /**
     * Método que verifica se um produto existe na variavel stock
     * @param cod       String com codigo de produtos
     * @return      boolean
     */
    public boolean existeProd(String cod){
        Iterator<Produto> it = stock.iterator();
        boolean r = false;
        while (it.hasNext() && !r){
            Produto aux = it.next();
            if (aux.getCod().equals(cod)){
                r = true;
            }
        }
        return r;
    }

    /**
     * Método que remove um produto so Set Stock
     * @param cod       String com codigo de produtos
     */
    public void remProdStock(String cod){
        Iterator<Produto> it = stock.iterator();
        boolean r = false;
        while (it.hasNext() && !r){
            Produto aux = it.next();
            if (aux.getCod().equals(cod)){
                stock.remove(aux);
                r = true;
            }
        }
    }


}