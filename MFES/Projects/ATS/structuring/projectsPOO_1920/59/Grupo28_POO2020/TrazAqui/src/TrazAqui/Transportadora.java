package TrazAqui;

import java.util.List;

public class Transportadora extends Estafeta {
    /**
     * Variaveis de instancia
     */
    private double numKms;
    private String NIF;
    private double precoKM;

    /**
     * Construtor vazio
     */
    public Transportadora() {
        this.numKms = 0;
        this.NIF = "";
        this.precoKM = 0;
    }

    /**
     * Construtor parametrizado
     * @param cod String
     * @param nome String
     * @param localizacao GPS
     * @param raio double
     * @param encomendasEntregues List<Encomenda>
     * @param pedidosEncomenda List<Encomenda>
     * @param classificacao int[]
     * @param disponivel boolean
     * @param certificada boolean
     * @param numKms double
     * @param NIF String
     * @param precoKM double
     */
    public Transportadora(String cod, String nome, GPS localizacao, double raio, List<Encomenda> encomendasEntregues, List<Encomenda> pedidosEncomenda, int[] classificacao, boolean disponivel, boolean certificada, double numKms, String NIF, double precoKM) {
        super(cod, nome, localizacao, raio, encomendasEntregues, pedidosEncomenda, classificacao, disponivel, certificada);
        this.numKms = numKms;
        this.NIF = NIF;
        this.precoKM = precoKM;
    }

    /**
     * Construtor por copio
     * @param a Transportadora
     */
    public Transportadora(Transportadora a) {
        super(a);
        this.numKms = a.getNumKms();
        this.NIF = a.getNIF();
        this.precoKM = a.getPrecoKM();
    }

    /**
     * Retorna o numero de kilometros percorridos
     * @return double
     */
    public double getNumKms() {
        return numKms;
    }

    /**
     * Define o numero de kilometros percorridos
     * @param numKms double
     */
    public void setNumKms(double numKms) {
        this.numKms = numKms;
    }

    /**
     * Retorna o NIF
     * @return String
     */
    public String getNIF() {
        return NIF;
    }

    /**
     * Define o NIF
     * @param NIF String
     */
    public void setNIF(String NIF) {
        this.NIF = NIF;
    }

    /**
     * Retorna o preco por km
     * @return double
     */
    public double getPrecoKM() {
        return precoKM;
    }

    /**
     * Define o preco por km
     * @param precoKM double
     */
    public void setPrecoKM(double precoKM) {
        this.precoKM = precoKM;
    }

    /**
     * Calcula o preco duma encomenda em funcao do peso e distancia
     * @param peso double
     * @param dist double
     * @return double
     */
    public double precoEncomenda(double peso, double dist) {
        double total;
        if (peso > 10) total = this.precoKM*dist;
        else total = this.precoKM*dist+2.5;
        return total;
    }

    /**
     * Retorna uma copia da class que a chama
     * @return Transportadora
     */
    public Transportadora clone() {
        return new Transportadora(this);
    }

    /**
     * Retorna o nome
     * @return String
     */
    public String toStringNome() {
        return "Transportadora";
    }

    /**
     * Aumenta o numero de kilometros
     * @param e GPS
     */
    public void aumentaKms(GPS e) {
        this.numKms += e.distancia(this.getLocalizacao());
    }

}
