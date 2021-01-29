package Model;

import java.io.Serializable;
import java.text.DecimalFormat;
import java.util.*;

public class Transportadora implements Serializable, ITransportadora{
    private String id;
    private ArrayList<Integer> rating;
    private int n_encomendas;
    private double range;
    private double localizacaoX;
    private double localizacaoY;
    private Double distancia;
    private boolean disponivel;

    private String nif;
    private String email;
    private String pwd;

    private String tipo;
    private String nome;

    private double preco_km;
    private double preco_transporte;
    private int n_max;
    private int n_ativo;

    private List<String> historico;
    private List<Double> faturacao;

    public boolean isDisponivel() {
        return disponivel;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPwd() {
        return pwd;
    }

    public void setPwd(String pwd) {
        this.pwd = pwd;
    }

    public double getDistancia() {
        return distancia;
    }

    public void setDistancia(Double distancia) {
        this.distancia = distancia;
    }

    public Transportadora (){
        this.id = null;
        this.disponivel = true;
        this.n_encomendas = 0;
        this.range = 0;
        this.distancia = 0.0;
        this.rating = new ArrayList<>();

        this.nif = null;
        this.tipo= null;
        this.preco_km = 0;
        Random r = new Random();
        this.n_max = r.nextInt((10 - 3) + 1) + 3;;
        this.n_ativo = 0;
        this.nome = null;
        this.localizacaoX = 0;
        this.localizacaoY = 0;
        this.faturacao = new ArrayList<>();
        this.historico = new ArrayList<>();
        this.pwd = null;
        this.email = null;
        this.preco_transporte = 0;
    }

    public  Transportadora (int n_ativo, String id, ArrayList<Integer> rating, double distancia, int n_encomendas, int range, double localizacaoX, double localizacaoY, String nif, boolean disponivel, String tipo, String nome, double preco_km, int n_max, List<String> historico, List<Double> faturacao){
        this.id = id;
        this.disponivel = disponivel;
        this.localizacaoX = localizacaoX;
        this.localizacaoY = localizacaoY;
        this.n_encomendas = n_encomendas;
        this.distancia = distancia;
        this.range = range;
        this.rating = rating;
        this.tipo= tipo;
        this.nome = nome;
        this.nif = nif;
        this.preco_km = preco_km;
        this.n_max = n_max;
        this.n_ativo = n_ativo;
        this.faturacao = faturacao;
        this.historico = historico;
    }

    public String getNome() {
        return nome;
    }

    public ArrayList<Integer> getRating() {
        return rating;
    }

    public String getTipo() {
        return tipo;
    }

    public double getRange() {
        return range;
    }

    public int getN_encomendas() {
        return n_encomendas;
    }

    public double getLocalizacaoX() {
        return localizacaoX;
    }

    public double getLocalizacaoY() {
        return localizacaoY;
    }

    public void setLocalizacaoY(double localizacaoY) {
        this.localizacaoY = localizacaoY;
    }

    public void setLocalizacaoX(double localizacaoX) {
        this.localizacaoX = localizacaoX;
    }

    public String getId() {
        return id;
    }

    public double getPreco_km() {
        return preco_km;
    }

    public void setTipo(String tipo) {
        this.tipo = tipo;
    }

    public double getPreco_transporte() {
        return preco_transporte;
    }

    public void setPreco_transporte(double preco_transporte) {
        this.preco_transporte = preco_transporte;
    }

    public void setRange(double range) {
        this.range = range;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setN_max(int n_max) {
        this.n_max = n_max;
    }

    public void setPreco_km(double preco_km) {
        this.preco_km = preco_km;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public List<String> getHistorico() {
        return new ArrayList<>(historico);
    }

    public void addHistorico(String s){
        historico.add(s);
    }

    public void addFaturacao(Double d){
        faturacao.add(d);
    }

    @Override
    public Double fat() {
        Double total = 0.0;
        for(Double d : faturacao){
            total+=d;
        }
        return total;
    }

    public boolean check_available() {
        if(n_ativo == n_max) {
            disponivel = false;
        }
        else {
            n_ativo++;
            disponivel = true;
        }

        return disponivel;
    }

    public void available() {
        disponivel = true;
    }

    @Override
    public String toString() {
        return "Transportadora{" +
                "id='" + id + '\'' +
                ", rating=" + rating +
                ", n_encomendas=" + n_encomendas +
                ", range =" + range +
                ", localizacaoX=" + localizacaoX +
                ", localizacaoY=" + localizacaoY +
                ", disponivel=" + disponivel +
                ", nif='" + nif + '\'' +
                ", tipo='" + tipo + '\'' +
                ", nome='" + nome + '\'' +
                ", preco_km=" + preco_km +
                ", n_max=" + n_max +
                ", historico=" + historico +
                ", faturacao=" + faturacao +
                '}';
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Transportadora that = (Transportadora) o;
        return rating == that.rating &&
                n_encomendas == that.n_encomendas &&
                Double.compare(that.range, range) == 0 &&
                Double.compare(that.localizacaoX, localizacaoX) == 0 &&
                Double.compare(that.localizacaoY, localizacaoY) == 0 &&
                disponivel == that.disponivel &&
                Double.compare(that.preco_km, preco_km) == 0 &&
                n_max == that.n_max &&
                Objects.equals(id, that.id) &&
                Objects.equals(nif, that.nif) &&
                Objects.equals(tipo, that.tipo) &&
                Objects.equals(nome, that.nome) &&
                Objects.equals(historico, that.historico);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, rating, n_encomendas, range, localizacaoX, localizacaoY, disponivel, nif, email, pwd, tipo, nome, preco_km, n_max, historico, faturacao);
    }

    public Double estrela(){
        double sum = 0;
        if(!this.rating.isEmpty()) {
            for (double mark : this.rating) {
                sum += mark;
            }
            return sum / this.rating.size();
        }
        return sum;
    }

/*
    public double fazFat(){
        double r;
        double p;
        double peso;

        for (Encomenda e: this.ativas) {

            p=+e.getPreco();
            peso = e.getPesoTot()
        }

        r=this.faturacao + p + this.getPreco_transporte() * this.distancia + 0.2*peso;

        return r;
    }*/
}
