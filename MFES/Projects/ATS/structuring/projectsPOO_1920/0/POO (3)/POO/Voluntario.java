import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.time.LocalDate;

public class Voluntario extends Utilizador implements Serializable {

    private String  codVoluntario;
    private int     raio;
    private boolean disponivel;
    private double avaliacao;
    private double totalKms;
    private List<Integer> avaliacoes;
    

    // Construtores
    public Voluntario() {
        super();
        this.codVoluntario = new String();
        this.raio = 0;
        this.disponivel = true;
        this.avaliacao=0;
        this.totalKms=0;
        this.avaliacoes = new ArrayList<>();
    }

    public Voluntario(String codVoluntario, String nome, String email, String password, double posX, double posY, int raio,  double avaliacao, double totalKms,List<Integer> avaliacoes) {
        super(nome, email, password, posX, posY);
        this.codVoluntario = codVoluntario;
        this.raio = raio;
        this.disponivel = true;
        this.avaliacao=avaliacao;
        this.totalKms=totalKms;
        this.setAvaliacoes(avaliacoes);
    }

    public Voluntario(Voluntario v) {
        super(v);
        this.codVoluntario = v.getCodVoluntario();
        this.raio = v.getRaio();
        this.disponivel = v.getDisponivel();
        this.avaliacao = v.getAvaliacao();
        this.totalKms = v.getTotalKms();
        this.setAvaliacoes(v.getAvaliacoes());
    }

    // Metodos de Instancia
    public String getCodVoluntario() {
        return this.codVoluntario;
    }

    public int getRaio() {
        return this.raio;
    }

    public boolean getDisponivel() {
        return this.disponivel;
    }

    public void setVoluntario(String cod) {
        this.codVoluntario = cod;
    }

    public void setRaio(int raio) {
        this.raio = raio;
    }

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
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
        s.append("\n\tIdentificador: Voluntario");
        s.append("\n\tCodigo: ");
        s.append(codVoluntario);
        s.append("\n\tRaio: ");
        s.append(raio);
        s.append("\n\tDisponivel: ");
        s.append(disponivel);
        s.append("\n\tAvaliacao: ");
        s.append(this.avaliacao);
        s.append("\n\tTotal Kms: ");
        s.append(this.totalKms);
        s.append("\n\tAvaliacoes: ");
        s.append(this.avaliacoes.toString());
        return s.toString();
    }

    public boolean equals(Object obj) {

        if (this == obj)
            return true;
        if (!this.equals(obj) || this.getClass() != obj.getClass())
            return false;
        Voluntario v = (Voluntario) obj;
        return super.equals(v) &&
                this.codVoluntario.equals(v.getCodVoluntario()) &&
                this.raio == v.getRaio() &&
                this.disponivel == v.getDisponivel() &&
                this.avaliacao == v.getAvaliacao() &&
                this.totalKms == v.getTotalKms() &&
                this.avaliacoes.equals(v.getAvaliacoes());
    }

    public Voluntario clone() {
        return new Voluntario(this);
    }
    
    
    
    public Viagem fazerTransporte(Cliente u,Loja l) throws ForaDeAlcanceException {

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

        double custo = 0;
        Viagem viagem = new Viagem(this.codVoluntario, this.getNome(), xv, yv, xu, yu, LocalDate.now(), distanciaF, custo, 0);
        return viagem;
    }
    
    public Voluntario registarAvaliacao (int i) {
        int soma=0;
        List<Integer> novo = this.getAvaliacoes();
        for(Integer in : novo){
           soma += in;
        }
        
        double avaliacao = soma/novo.size();
        this.setAvaliacoes(novo);
        this.setAvaliacao(avaliacao);
        return this;
    }
    
    public Voluntario atualizaKms(double kms){
        double sum =0;
        sum = this.totalKms + kms;
        this.setTotalKms(sum);
        return this;
    }
    
    

}