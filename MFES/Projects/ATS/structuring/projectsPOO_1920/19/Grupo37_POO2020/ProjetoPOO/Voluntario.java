import java.util.ArrayList;
import java.util.Objects;
import java.time.LocalDateTime;

public class Voluntario extends Utilizador {
    private String codigoV;
    private double raio;
    private boolean disponivel;
    private boolean medicamentos;
    private ArrayList<Integer> classificacao;
    private int velocidadeEst;
    private Encomendas encomenda;


    public Voluntario (String nome, GPS localizacao, String email, String password, double raio, String codigoV, boolean disponivel, boolean medicamentos, ArrayList<Integer> classificacao, int velocidadeEst, Encomendas encomenda){
        super(nome,localizacao,email ,password);
        this.raio = raio ;
        this.codigoV = codigoV;
        this.disponivel = disponivel;
        this.medicamentos = medicamentos;
        this.classificacao = classificacao;
        this.velocidadeEst = velocidadeEst;
        this.encomenda = encomenda;
    }

    public Voluntario(){
        super();
        this.codigoV = "n/a";
        this.raio = 0;
        this.disponivel = false;
        this.medicamentos = false;
        this.classificacao = new ArrayList<>();
        this.velocidadeEst = 0;
        this.encomenda = null;
    }

    public Voluntario(Voluntario v){
        super(v);
        this.codigoV= v.getCodigoV();
        this.raio= v.getRaio();
        this.medicamentos = v.aceitoTransporteMedicamentos();
        this.disponivel = v.isDisponivel();
        this.classificacao = v.getClassificacao();
        this.velocidadeEst = v.getVelocidadeEst();
        this.encomenda = v.getEncomenda();
    }

    public String getCodigoV() {
        return this.codigoV;
    }

    public void setCodigoV(String codigoV) {
        this.codigoV = codigoV;
    }

    public double getRaio() {
        return this.raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public boolean isDisponivel() {
        return this.disponivel;
    }

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    public ArrayList<Integer> getClassificacao() {
        return this.classificacao;
    }

    public void setClassificacao(ArrayList<Integer> classificacao) {
        this.classificacao = classificacao;
    }

    public int getVelocidadeEst() {
        return this.velocidadeEst;
    }

    public void setVelocidadeEst(int velocidadeEst) {
        this.velocidadeEst = velocidadeEst;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Voluntario)) return false;
        Voluntario that = (Voluntario) o;
        return super.equals(that);
    }

    public boolean aceitoTransporteMedicamentos(){
        return this.medicamentos;
    }

    public void aceitaMedicamentos(boolean state){
        this.medicamentos = state;
    }

    public Encomendas getEncomenda() {
        return this.encomenda;
    }

    public void setEncomenda(Encomendas encomenda) {
        this.encomenda = encomenda;
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }

    public void addClassificacao(int c){
        this.classificacao.add(c);
    }

    public float calculaClassificacao(){
        float media = 0;
        int i = 0;
        for(Integer d : this.classificacao){
            media = media + d;
            i+=1;
        }
        return media/i;
    }
    
    public String toString(){
        return this.getNome() + " classificação: " + calculaClassificacao();
    }
}