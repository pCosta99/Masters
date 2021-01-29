import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.time.LocalDate;

public class Transportadora extends Utilizador implements Serializable {

    private String  codEmpresa;
    private int     nif;
    private int     raio;
    private double  precoKm;
    private double  precoKilo;
    private double  taxa; 
    private boolean disponivel;
    private double avaliacao;
    private double totalKms;
    private List<Integer> avaliacoes;

    public Transportadora() {
        super();
        this.codEmpresa = new String();
        this.nif = 0;
        this.raio = 0;
        this.precoKm = 0.0;
        this.precoKilo=0.0;
        this.taxa=0.0;
        this.disponivel = true;
        this.avaliacao=0;
        this.totalKms=0.0;
        this.avaliacoes = new ArrayList<>();
    }

    public Transportadora(String codEmpresa, String nome, String email, String password, double posX, double posY, int nif, int raio, double precoKm,
                          double precoKilo, double taxa, boolean disponivel, double avaliacao, double totalKms,List<Integer> avaliacoes) {
                              
        super(nome, email, password, posX, posY);
        this.codEmpresa = codEmpresa;
        this.nif = nif;
        this.raio = raio;
        this.precoKm = precoKm;
        this.precoKilo =precoKilo;
        this.taxa= taxa;
        this.disponivel = disponivel;
        this.avaliacao=avaliacao;
        this.totalKms=totalKms;
        this.setAvaliacoes(avaliacoes);

    }

    public Transportadora(Transportadora t) {
        super(t);
        this.codEmpresa = t.getCodEmpresa();
        this.nif = t.getNif();
        this.raio = t.getRaio();
        this.precoKm = t.getPrecoKm();
        this.precoKilo = t.getPrecoKilo();
        this.taxa = t.getTaxa();
        this.disponivel = t.getDisponivel();
        this.avaliacao = t.getAvaliacao();
        this.totalKms = t.getTotalKms();
        this.setAvaliacoes(t.getAvaliacoes());
    }
    
    public double getTaxa(){
        return this.taxa;
    }
    
    public double getPrecoKilo(){
        return this.precoKilo;
    }
    
    public String getCodEmpresa() {
        return this.codEmpresa;
    }

    public int getNif() {
        return this.nif;
    }

    public int getRaio() {
        return this.raio;
    }

    public double getPrecoKm() {
        return this.precoKm;
    }

    public boolean getDisponivel() {
        return this.disponivel;
    }
    public double getAvaliacao() {
        return this.avaliacao;
    }
    
     public double getTotalKms() {
        return this.totalKms;
    }
    
    public List<Integer> getAvaliacoes(){
        List<Integer> nova= new ArrayList<>();
        for(Integer i: this.avaliacoes){
            nova.add(i);
        }
        return nova;
    }
            

    public void setCodEmpresa(String codEmpresa) {
        this.codEmpresa = codEmpresa;
    }

    public void setNif(int nif) {
        this.nif = nif;
    }

    public void setRaio(int raio) {
        this.raio = raio;
    }

    public void setPrecoKm(double precoKm) {
        this.precoKm = precoKm;
    }
    public void setPrecoKilo(double precoKilo){
        this.precoKilo=precoKilo;
    }
    public void setTaxa(double taxa){
        this.taxa= taxa;
    }

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }
    
    public void setAvaliacao(double avaliacao){
        this.avaliacao=avaliacao;
    }

     public void setTotalKms(double totalKms){
        this.totalKms=totalKms;
    }
  
    public void setAvaliacoes(List<Integer> ava) {
        this.avaliacoes = new ArrayList<>(ava.size());
        for(Integer i: ava){
            this.avaliacoes.add(i);
        }
            
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append(super.toString());
        s.append("\n\tCodigo transportadora: ");
        s.append(this.codEmpresa);
        s.append("\n\tNif: ");
        s.append(this.nif);
        s.append("\n\tRaio: ");
        s.append(this.raio);
        s.append("\n\tPreço por km: ");
        s.append(this.precoKm);
        s.append("\n\tPreço por kilo: ");
        s.append(this.precoKilo);
        s.append("\n\tTaxa: ");
        s.append(this.taxa);
        s.append("\nDisponivel: ");
        s.append(this.disponivel);
        s.append("\n\tAvaliacao: ");
        s.append(this.avaliacao);
        s.append("\n\tTotal Kms: ");
        s.append(this.totalKms);
        s.append("\n\tAvaliacoes: ");
        s.append(this.avaliacoes.toString());
        return s.toString();
    }

    public Transportadora clone() {
        return new Transportadora(this);
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj) ||getClass() != obj.getClass())
            return false;
        Transportadora t = (Transportadora) obj;
        return super.equals(t) &&
                this.codEmpresa.equals(t.getCodEmpresa()) &&
                this.nif == t.getNif() &&
                this.raio == t.getRaio() &&
                this.precoKm == t.getPrecoKm() &&
                this.precoKilo == t.getPrecoKilo() &&
                this.taxa == t.getTaxa() &&
                this.disponivel == t.getDisponivel() &&
                this.avaliacao == t.getAvaliacao() &&
                this.totalKms == t.getTotalKms() &&
                this.avaliacoes.equals(t.getAvaliacoes());
    }

    public Viagem fazerTransporte(Cliente u,Loja l, Encomenda e) throws ForaDeAlcanceException {

        double xv = this.getPosX();
        double yv = this.getPosY();

        double xu = u.getPosX();
        double yu = u.getPosY();
        
        double xl = l.getPosX();
        double yl = l.getPosY();

        Calculos calculos = new Calculos();
        double distancia1 = calculos.distancia(xv,yv,xl,yl);//Distancia voluntario ->loja;
        double distancia2 = calculos.distancia(xl,yl,xu,yu);//Distancia loja -> cliente;
        double distancia3 = calculos.distancia(xv,yv,xu,yu);//Distancia voluntario -> cliente;
        if(distancia1 > this.raio){
            throw new ForaDeAlcanceException(Constantes.LOJA_FORA_DE_ALCANCE);
        }
        if(distancia3 > this.raio){
            throw new ForaDeAlcanceException(Constantes.CLIENTE_FORA_DE_ALCANCE);
        }
        double distanciaF = distancia1+distancia2;

        double custo = calculos.custo(distanciaF, e.getPeso(), this.precoKm, this.precoKilo, this.taxa);
        Viagem viagem = new Viagem(this.codEmpresa, this.getNome(), xv, yv, xu, yu, LocalDate.now(), distanciaF, custo, 0);
        return viagem;
        
    }
    
    public Transportadora registarAvaliacao (int valor) {
        int soma=0;
        List<Integer> novo = this.getAvaliacoes();
        novo.add(valor);
        for(Integer in : novo){
           soma += in;
        }
        
        double avaliacao = soma/novo.size();
        this.setAvaliacoes(novo);
        this.setAvaliacao(avaliacao);
        return this;
    }
    
    public Transportadora atualizaKms(double kms){
        double sum =0;
        sum = this.totalKms + kms;
        this.setTotalKms(sum);
        return this;
    }
    
  
    
}
