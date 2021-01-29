package Models;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe que guarda informação sobre um agente
 *
 */
public abstract class Agente implements Serializable {

    private final boolean aceitoTransporteMedicamento;
    private double classificacao; //classificao do agente
    private int totalclassifs;
    private boolean disponibilidade;

    private double coordenadaX;
    private double coordenadaY;
    private double raio_acao;
    private Map<Encomenda, StatsEntrega> registo_encomendas;  ///registo das encomendas já entregues
    private String codigo, nome;


    public Agente(String nome, String cod, double clas, int t, double x, double y, double r, boolean med, boolean d) {
        this.classificacao = clas;
        this.totalclassifs = t;
        this.coordenadaX = x;
        this.coordenadaY = y;
        this.nome = nome;
        this.codigo = cod;
        this.raio_acao = r;
        this.registo_encomendas = new HashMap<>();
        this.aceitoTransporteMedicamento = med;
        this.disponibilidade = d;
    }

    public boolean getDisponibilidade() {
        return this.disponibilidade;
    }

    public void setDisponibilidade(boolean disp) {
        this.disponibilidade = disp;
    }

    public boolean getMed() {
        return this.aceitoTransporteMedicamento;
    }

    public double getClassificacao() {
        return classificacao;
    }

    public void setClassificacao(double classificacao) {
        this.classificacao = classificacao;
    }

    public int getTotalClassifs() {
        return totalclassifs;
    }

    public void setTotalClassifs(int t) {
        this.totalclassifs = t;
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

    public double getRaio_acao() {
        return raio_acao;
    }

    public void setRaio_acao(double raio_acao) {
        this.raio_acao = raio_acao;
    }

    public Map<Encomenda, StatsEntrega> getRegisto_encomendas() {
        HashMap<Encomenda, StatsEntrega> res = new HashMap<>();
        for (Map.Entry<Encomenda, StatsEntrega> e : this.registo_encomendas.entrySet()) {
            res.put(e.getKey().clone(), e.getValue().clone());
        }

        return res;
    }

    public void setRegisto_encomendas(Map<Encomenda, StatsEntrega> registo_encomendas) {
        HashMap<Encomenda, StatsEntrega> res = new HashMap<>();
        for (Map.Entry<Encomenda, StatsEntrega> e : registo_encomendas.entrySet()) {
            res.put(e.getKey().clone(), e.getValue().clone());
        }
        this.registo_encomendas = res;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getR() {
        return this.raio_acao;
    }

    public String getCodigo() {
        return this.codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    /**
     * Verica-se se umas dadas coodenadas estão dentro do raio do agente
     *
     * @param x Latitude
     * @param y Longitude
     * @return True se dentro no raio do agente, False caso contrario
     */
    public boolean in_range(Double x, Double y) {
        double distancia;
        distancia = Math.sqrt(Math.pow(x - this.coordenadaX, 2) + Math.pow(y - this.coordenadaY, 2));

        return !(distancia > raio_acao);
    }

    /**
     * Devolve a distância entre o agente e as dadas coordenadas
     *
     * @param x Latitude
     * @param y Longitude
     * @return Distância calculada
     */
    public double get_distance(Double x, Double y) {

        return Math.sqrt(Math.pow(x - this.coordenadaX, 2) + Math.pow(y - this.coordenadaY, 2));
    }

    /**
     * Verifica se o utilizador e a loja ficam dentro do raio de ação do agente
     *
     * @param u Models.Utilizador
     * @param l Models.Loja
     * @return True se isto se vericar, false caso contrário
     */
    public boolean encomenda_in_range(Utilizador u, Loja l) {
        return this.in_range(u.getCoordenadaX(), u.getCoordenadaY()) && this.in_range(l.getCoordenadaX(), l.getCoordenadaY());
    }

    /**
     * Devolve a distancia percorrida entre por um agente ate uma dada loja
     * e de mesma ate ao utilizador.
     *
     * @param u Models.Utilizador
     * @param l Loba
     * @return True se isto se vericar, false caso contrário
     */
    public double distanticia_entrega(Utilizador u, Loja l) {
        return this.get_distance(u.getCoordenadaX(), u.getCoordenadaX()) +
                Math.sqrt(Math.pow(u.getCoordenadaX() - l.getCoordenadaX(), 2) + Math.pow(u.getCoordenadaY() - l.getCoordenadaY(), 2));
    }

    /**
     * Solicia uma entrega de uma dada encomenda atraves do agente atual
     *
     * @param x        Models.Encomenda a ser pedida
     * @param u        Models.Utilizador que pediu a encomenda
     * @param l        Models.Loja a processar a encomenda
     * @return True se for aceite, False caso contrário
     */
    public abstract boolean solicitar_transporte(Encomenda x, Utilizador u, Loja l);

    /**
     * Adiciona uma classificação ao agente atual
     *
     * @param classif Classificação dada
     */
    public void rate(double classif) {

        this.classificacao = ((this.classificacao * this.totalclassifs)+classif) / (this.totalclassifs+1);
        this.totalclassifs++;
    }

    public void add_encomenda_to_historico(Encomenda e,StatsEntrega x) { this.registo_encomendas.put(e,x);
    }

    public abstract boolean has_order_in_name(String user_code);

    public abstract void remove_encomenda(String user_code, LocalDateTime hora_chegada);
}
