import java.util.ArrayList;

public class Transportadora extends Utilizador {
    private String codigoT;
    private double kms;
    private boolean medicamentos;
    private double raio;
    private int aceite;
    private float precokms;
    private double precoTemp;
    private int velocidadeEst;
    private ArrayList<Encomendas> encAtuais;

    public Transportadora(String nome, GPS localizacao, String email, String password, double kms, double raio, String codigoT, boolean medicamentos, int aceite, float precokms, double precoTemp, int velocidadeEst,ArrayList<Encomendas> encAtuais) {
        super(nome, localizacao, email, password);
        this.raio = raio;
        this.kms = kms;
        this.codigoT = codigoT;
        this.medicamentos = medicamentos;
        this.aceite = aceite;
        this.precokms = precokms;
        this.precoTemp = precoTemp;
        this.velocidadeEst = velocidadeEst;
        this.encAtuais = encAtuais;
    }

    public Transportadora(){
        super();
        this.codigoT = "n/a";
        this.raio = 0;
        this.medicamentos = false;
        this.kms = 0;
        this.aceite = 0;
        this.precokms = 1;
        this.precoTemp = 1;
        this.velocidadeEst = 0;
        this.encAtuais = new ArrayList<Encomendas>();
    }

    public Transportadora(Transportadora t) {
        super(t);
        this.codigoT = t.getCodigoT();
        this.raio = t.getRaio();
        this.medicamentos = t.aceitoTransporteMedicamentos();
        this.kms = t.getKms();
        this.aceite = t.getAceite();
        this.precokms = t.getPrecokms();
        this.precoTemp = t.getPrecoTemp();
        this.velocidadeEst = t.getVelocidadeEst();
        this.encAtuais= t.getEncAtuais();
    }

    public String getCodigoT() {
        return this.codigoT;
    }

    public void setCodigoT(String codigoT) {
        this.codigoT = codigoT;
    }

    public double getKms() {
        return this.kms;
    }

    public void setKms(double kms) {
        this.kms = kms;
    }

    public boolean aceitoTransporteMedicamentos(){
        return this.medicamentos;
    }

    public void aceitaMedicamentos(boolean state){
        this.medicamentos = state;
    }

    public double getRaio() {
        return this.raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public int getAceite() {
        return this.aceite;
    }

    public void setAceite(int a) {
        this.aceite = a;
    }

    public float getPrecokms() {
        return this.precokms;
    }

    public void setPrecokms(float precokms) {
        this.precokms = precokms;
    }

    public double getPrecoTemp() {
        return this.precoTemp;
    }

    public void setPrecoTemp(double precoTemp) {
        this.precoTemp = precoTemp;
    }

    public int getVelocidadeEst() {
        return this.velocidadeEst;
    }

    public void setVelocidadeEst(int velocidadeEst) {
        this.velocidadeEst = velocidadeEst;
    }

    public ArrayList<Encomendas> getEncAtuais() {
        return this.encAtuais;
    }

    public void setEncAtuais(ArrayList<Encomendas> encAtuais) {
        this.encAtuais = encAtuais;
    }

    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Transportadora t = (Transportadora) obj;
        return (super.equals(obj) && t.getCodigoT().equals(this.codigoT));
    }

    public void addEncomenda(Encomendas e){
        this.encAtuais.add(e);
    }

    public void removeEncomenda(Encomendas e){
        this.encAtuais.remove(e);
    }
    
    public double calculaPreco(Lojas j, Cliente c) {
        return ((j.getAtendimentohabitual() * this.precoTemp) + (this.distEntre(j.getLocalizacao()) + j.distEntre(c.getLocalizacao())) * this.precokms);
    }

    public Transportadora clone() {
        return new Transportadora(this);
    }
    
    public String toString(){
        return this.getNome() + " com " + this.kms + "kms";
    }
}