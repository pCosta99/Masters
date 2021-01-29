import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe com informações do voluntário
 */
public class Voluntario extends RegistoTULV {
    private static final double velocidade = 15.0;
    private boolean ocup;
    private boolean certMed;
    private double raio;
    private List<Double> classificacao;

    /**
     * Construtor vazio
     */
    public Voluntario() {
        super();
        this.ocup = false;
        this.certMed = false;
        this.raio = 0;
        this.classificacao = new ArrayList<>();
    }

    /**
     * Construtor com argumentos
     * @param c Código
     * @param n Nome
     * @param p GPS
     * @param pass Password
     * @param m Encomendas
     * @param s Ocupação
     * @param med Certificado médico
     * @param r Raio
     * @param mail Email
     * @param cl Classificação
     */
    public Voluntario(String c, String n, Ponto p, String pass, TreeMap<String, EncDistr> m, boolean s, boolean med, double r, String mail,ArrayList<Double> cl) {
        super(c,n,p,pass,m,mail);
        this.ocup = s;
        this.certMed = med;
        this.raio = r;
        this.classificacao = new ArrayList<>(cl);
    }

    /**
     * Construtor que recebe um objeto Voluntario
     * @param v
     */
    public Voluntario(Voluntario v) { super(v); this.ocup = v.ocup; this.certMed = v.certMed; this.raio = v.raio; this.classificacao = new ArrayList<>(v.classificacao);}

    /**
     * Devolve ocupação
     * @return Ocupação
     */
    public boolean getOcup () { return this.ocup; }

    /**
     * Devolve certificado médico
     * @return Certificado médico
     */
    public boolean getCertMed () { return this.certMed; }

    /**
     * Devolve raio
     * @return Raio
     */
    public double getRaio () { return this.raio; }

    /**
     * Devolve classificação
     * @return Classificação
     */
    public List<Double> getClassificacao() {
        return new ArrayList<>(this.classificacao);
    }

    /**
     * Devolve velocidade
     * @return Velocidade
     */
    public double getVelocidade() { return velocidade;}

    /**
     * Introduz ocupaçãp
     * @param c Ocupação
     */
    public void setOcup (boolean c) { this.ocup = c; }

    /**
     * Introduz certificado médico
     * @param c Certificado médico
     */
    public void setCertMed (boolean c) { this.certMed = c; }

    /**
     * Introduz raio
     * @param r Raio
     */
    public void setRaio (double r) { this.raio = r; }

    /**
     * Introduz lista de classificações
     * @param classificacao Classificação
     */
    public void setClassificacao(List<Double> classificacao) {
        this.classificacao = new ArrayList<>(classificacao);
    }

    /**
     * Método equals
     * @param o Voluntario
     * @return true se os objetos forem iguais ou false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Voluntario that = (Voluntario) o;
        return ocup == that.ocup &&
                certMed == that.certMed &&
                Double.compare(that.raio, raio) == 0;
    }

    /**
     * Método clone
     * @return Cópia de um objeto da classe Voluntario
     */
    public Voluntario clone () {
        return new Voluntario(this);
    }

    /**
     * Método toString
     * @return String com informações do voluntário
     */
    public String toString() { return
            "V\n" +
            super.toString()
            + "Ocup: " + this.ocup + "\n"
            + "Certificado Médico: " + this.certMed + "\n" + "Raio: " + this.raio + "\n" + this.classificacao.toString() + "\n"; }

    /**
     * Verifica se tem certificado médico e se não se encontra ocupado
     * @return true se aceitar transporte de medicamentos ou 0 caso contrário
     */
    public boolean aceitoTransporteMedicamentos() { return (!ocup && certMed); }

    /**
     * Calcula taxa de trânsito de acordo com a data
     * @param h Data
     * @return Taxa de trânsito
     */
    public double calculaTransito (LocalDateTime h) {
        Random rs = new Random();
        double taxa;
        if (h.getHour() > 9 && h.getHour() < 19) taxa = 1.50 + (0.50) * rs.nextDouble();
        else taxa =  1.0 + (0.30) * rs.nextDouble();
        return taxa;
    }

    /**
     * Calcula tempo de viagem
     * @param l Loja
     * @param u Utilizador
     * @return Tempo
     */
    public double calculoTempo (Loja l , Utilizador u){
        double tempoAteLoja = this.getGps().distancia(l.getGps()) / velocidade;
        double tempoAteUser = l.getGps().distancia(u.getGps()) / velocidade;
        double tempoNaLoja = l.getTempoMedioEspera();
        return tempoNaLoja + tempoAteLoja + tempoAteUser;
    }

    /**
     * Calcula tempo até recolher encomenda da loja
     * @param l Loja
     * @param h Data
     * @return Tempo
     */
    public double calculoTempoAteRecolher (Loja l, LocalDateTime h){
        double tempoAteLoja = this.getGps().distancia(l.getGps()) / velocidade;
        double tempoNaLoja = l.getTempoMedioEspera();
        double taxa = calculaTransito(h);
        return (tempoNaLoja + tempoAteLoja)*taxa;
    }

    /**
     * Calcula tempo da loja ao utilizador
     * @param l Loja
     * @param u Utilizador
     * @param h Data
     * @return Tempo
     */
    public double calculoTempoAteUser (Loja l , Utilizador u, LocalDateTime h){
        double taxa = calculaTransito(h);
        return (l.getGps().distancia(u.getGps()) / velocidade)*taxa;
    }

    /**
     * Verifica se loja está no raio do voluntário
     * @param l Loja
     * @return true se estiver no raio ou false caso contrário
     */
    public boolean inRaio (Loja l) {
        double dist = this.getGps().distancia(l.getGps());
        return (dist<=this.getRaio());
    }

    /**
     * Adiciona classificação
     * @param a Classificação
     */
    public void addClassi (double a) {
        this.classificacao.add(a);
    }

    /**
     * Calcula a média das classificações
     * @return Média das calssificações
     */
    public double classificacaoMedia() {
        return (this.classificacao.stream().reduce(0.0, Double::sum)) / (this.classificacao.size());
    }

    /**
     * Devove data de entrega da última encomenda
     * @return Data de entrega da última encomenda
     */
    public LocalDateTime dataUltEncomenda() {
        List<Encomenda> l = this.getEncomendas().values().stream()
                .sorted(new ComparatorData()).map(EncDistr::getEncomenda).collect(Collectors.toList());
        if (l.size() != 0) return l.get(0).getDataE();
        return null;
    }
}
