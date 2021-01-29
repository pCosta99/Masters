package Model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Write a description of class Model.Voluntario here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

/**
 * Classe responsavel por guardar toda a informação relativa a um voluntario
 */
public class Voluntario implements Serializable
{
    private String pass;
    private String cod;
    private String nome;
    private GPS gps;
    private double raio;
    private boolean livre;
    private List<Encomenda> pedidos;
    private List<Encomenda> listenc;
    private boolean medico;

    /**
     * Construtor sem parametros
     */
    public Voluntario()
    {
        this.pass = new String();
        this.cod = new String();
        this.nome = new String();
        this.gps = new GPS();
        this.raio = 0;
        this.livre = true;
        this.pedidos = new ArrayList<>();
        this.listenc = new ArrayList<>();
        this.medico = false;
    }

    /**
     * Construtor parametrizado
     * @param s     String com codigo de voluntario
     * @param n     String com nome
     * @param gps   GPS com localização
     * @param r     double com o raio de ação
     * @param b     boolean que verifica se esta livre para entregas
     * @param lista Lista com todas as encomendas entregues
     * @param med   boolean que verifica se pode efetuar entregas de encomendas medicas
     */
    public Voluntario (String s, String n, GPS gps, double r, boolean b, ArrayList<Encomenda> lista, boolean med)
    {
        this.pass = s;
        this.cod = s;
        this.nome = n;
        this.gps = gps.clone();
        this.raio =r;
        this.livre = b;
        this.pedidos = new ArrayList<>();
        this.setList(lista);
        this.medico = med;

    }

    /**
     * Construtor parametrizado
     * @param p     String com pass
     * @param s     String com codigo de voluntario
     * @param n     String com nome
     * @param gps   GPS com localização
     * @param r     double com o raio de ação
     * @param med   boolean que verifica se esta apto a fazer entregas de encomendas medicas
     */
    public Voluntario (String p, String s, String n, GPS gps, double r, boolean med)
    {
        this.pass = p;
        this.cod = s;
        this.nome = n;
        this.gps = gps.clone();
        this.raio =r;
        this.livre = true;
        this.pedidos = new ArrayList<>();
        this.listenc = new ArrayList<>();
        this.medico = med;

    }

    /**
     * Construtor por copia
     * @param u     Voluntario a copiar
     */
    public Voluntario (Voluntario u)
    {
        this.pass = u.getPass();
        this.cod = u.getCod();
        this.nome = u.getNome();
        this.gps = new GPS(u.getGPS());
        this.raio = u.getRaio();
        this.livre = u.getLivre();
        this.setPedidos(u.getPedidos());
        this.setList(u.getList());
        this.medico = u.aceitoTransporteMedicamentos();
    }

    /**
     * Get da variavel medico do objeto
     * @return      boolean
     */
    public boolean aceitoTransporteMedicamentos() {
        return medico;
    }

    /**
     * Set da variavel medico do objeto
     * @param medico    boolean
     */
    public void aceitaMedicamentos(boolean medico) {
        this.medico = medico;
    }

    /**
     * Set da variavel pass do objeto
     * @param p     String com a pass
     */
    public void setPass(String p){this.pass = p;}

    public String getPass(){return this.pass;}

    public String getCod()
    {
        return this.cod;
    }

    public String getNome()
    {
        return this.nome;
    }

    public GPS getGPS()
    {
        return this.gps;
    }

    public double getRaio()
    {
        return this.raio;
    }

    public boolean getLivre()
    {
        return this.livre;
    }


    public ArrayList<Encomenda> getList() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for (Encomenda l : this.listenc)
            aux.add(l);
        return aux;
    }

    public ArrayList<Encomenda> getPedidos() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for (Encomenda l : this.pedidos)
            aux.add(l);
        return aux;
    }

    /**
     * Set da variavel listenc do objeto
     * @param l     List com todas as encomendas feitas
     */
    public void setList (ArrayList<Encomenda> l)
    {
        this.listenc = new ArrayList<>();
        for(Encomenda li : l)
            this.listenc.add(li);
    }

    /**
     * Set da variavel pedidos do objeto
     * @param l     List com todos os pedidos para entregas
     */
    public void setPedidos (ArrayList<Encomenda> l)
    {
        this.pedidos = new ArrayList<>();
        for(Encomenda li : l)
            this.pedidos.add(li);
    }
    /**
     * Set da variavel cod do objeto
     * @param s     String com o codigo de voluntario
     */
    public void setCod(String s)
    {
        this.cod = s;
    }

    /**
     * Set da variavel nome do objeto
     * @param n     String com o nome
     */
    public void setNome(String n)
    {
        this.nome = n;
    }

    /**
     * Set da variavel GPS do objeto
     * @param l     double com a latitude
     * @param lo    double com a longitude
     */
    public void setGPS (double l, double lo)
    {
        this.gps.setGPS(l,lo);
    }

    /**
     * Set da variavel raio do objeto
     * @param r     String com o raio
     */
    public void setRaio (double r)
    {
        this.raio = r;
    }

    /**
     * Set da variavel livre do objeto
     * @param b     boolean
     */
    public void setLivre(boolean b)
    {
        this.livre = b;
    }

    /**
     * Método que clona este objeto
     * @return      clone deste objeto
     */
    public Voluntario clone()
    {
        return new Voluntario(this);
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
        Voluntario u = (Voluntario) o;
        return this.cod.equals(u.getCod()) &&
               this.nome.equals(u.getNome()) &&
               this.gps.equals(u.getGPS()) &&
               this.raio == u.getRaio() &&
               this.pedidos == u.getPedidos() &&
               this.livre == u.getLivre();
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Model.Voluntario{");
        sb.append("cod='").append(cod).append('\'');
        sb.append(", nome='").append(nome).append('\'');
        sb.append(", gps=").append(gps);
        sb.append(", raio=").append(raio);
        sb.append(", livre=").append(livre);
        sb.append(", listenc=").append(listenc);
        sb.append('}');
        return sb.toString();
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
     * Método que faz todas as alterações respetivas a aceitar uma encomenda para entrega
     * @param e     Encomenda aceite
     * @param i     LocalDateTime com data e hora da aceitação
     */
    public void aceitaPedido(Encomenda e, LocalDateTime i){
        this.pedidos.remove(e);
        this.listenc.add(e);
        this.livre = false;
        Iterator<Encomenda> it = listenc.iterator();
        boolean f = false;
        while(it.hasNext() && !f){
            Encomenda enc = it.next();
            if(enc.equals(e)){
                enc.setDatai(i);
                enc.setTransp(this.cod);
                f=true;
            }
        }
    }

    /**
     * Método que remove uma Encomenda da variavel pedidos
     * @param e     Encomenda a retirar
     */
    public void rejeitaPedido(Encomenda e){
        this.pedidos.remove(e);
    }


    /**
     * Método que adiciona ume Encomenda a variavel pedidos
     * @param e     Encomenda a adicionar
     */
    public void addPedido(Encomenda e){ this.pedidos.add(e);}

    /**
     * Método que remove uma Encomenda da variavel pedidos
     * @param e     Encomenda a retirar
     */
    public void rmPedido(Encomenda e){ this.pedidos.remove(e);}

    /**
     * Set da variavel classificação de uma encomenda
     * @param e     Encomenda a classificar
     * @param cl    int com classificação
     */
    public void setCl(Encomenda e, int cl){
        Iterator<Encomenda> it = pedidos.iterator();
        boolean f = false;
        while(it.hasNext() && !f){
            Encomenda enc = it.next();
            if(enc.equals(e)){
                enc.setClassificacao(cl);
                f=true;
            }
        }
    }

    /**
     * Método que devolve a média das classificações das encomendas entregues
     * @return      double com média
     */
    public double getClGeral() {
        double r = 0;
        double size = 0;
        if (this.listenc.size() <= 0) return 0;
        else {
            for (Encomenda e : this.listenc){
                if (e.getClassificacao() != -1 && e.getEntregue()) {
                    r += e.getClassificacao();
                    size++;
                }
            }
            if (size == 0) return 0;
            else return r / size;
        }
    }

    /**
     * Método que faz todas as alterações respetivas a entregar ume encomenda
     * @param e     Encomenda entregue
     * @param fi    LocalDateTime com data e hora da entrega
     */
    public void encEntregue(Encomenda e, LocalDateTime fi) {
        Iterator<Encomenda> it = this.listenc.iterator();
        this.livre = true;
        boolean f = false;
        while (it.hasNext() && !f) {
            Encomenda e1 = it.next();
            if (e1.equals(e)) {
                e1.setDataf(fi);
                e1.setEntregue(true);
                f = true;
            }
        }
    }

}