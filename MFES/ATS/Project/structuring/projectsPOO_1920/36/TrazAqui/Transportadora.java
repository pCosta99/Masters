
/**
 *
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.List;
import java.util.ArrayList;

public class Transportadora extends Entidade implements TransportadorMedicamentos, Classificavel, Transportador {
    
    private double raio;
    private boolean aceitoTransporteMedicamentos;
    private List<Integer>classificacoes;
    private boolean disponivel;
    private double precoKm;
    private double precoHora;
    
    public Transportadora() {
        super();
        this.raio = 100;
        this.classificacoes = new ArrayList<>();
        this.precoKm = 1;
        this.precoHora = 1;
    }
    
    public Transportadora(String codigo, String nome, GPS gps, String email, String password, double raio, double precoKm, double precoHora) {
        super(codigo, nome, gps, email, password);
        this.raio = raio;
        this.classificacoes = new ArrayList<>();
        this.precoKm = precoKm;
        this.precoHora = precoHora;
    }
    
    public Transportadora(Transportadora t) {
        super(t);
        this.raio = t.getRaio();
        this.classificacoes = t.getClassificacoes();
        this.precoKm = t.getPrecoKm();
        this.precoHora = t.getPrecoHora();
    }
    
    public double getRaio() {
        return this.raio;
    }
    
    public List<Integer> getClassificacoes() {
        return new ArrayList<>(this.classificacoes);
    }
    
    public double getPrecoKm() {
        return this.precoKm;
    }
    
    public double getPrecoHora() {
        return this.precoHora;
    }
    
    public void setRaio(double raio) {
        this.raio = raio;
    }
    
    public void setPrecoKm(double precoKm) {
        this.precoKm = precoKm;
    }
    
    public void setPrecoHora(double precoHora) {
        this.precoHora = precoHora;
    }
    
    public boolean equals(Object obj) {
       if(this == obj) return true;
       if((obj == null) || (this.getClass() != obj.getClass())) return false;
       Transportadora t = (Transportadora) obj;
       return super.equals(t) &&
              this.raio == t.getRaio() && 
              this.aceitoTransporteMedicamentos == t.aceitoTransporteMedicamentos() &&
              this.classificacoes.equals(t.getClassificacoes());
   }
    
    public String toString() {
        return "Transportadora: " + "\n" +
               " - Código: " + this.getCodigo() + "\n" +
               " - Nome: " + this.getNome() + "\n" +
               " - GPS: " + this.getGPS().toString() + "\n" + 
               " - Raio: " + this.raio + "\n" + 
               " - Aceita transporte de medicamentos " + this.aceitoTransporteMedicamentos + "\n" +
               " - Preço por km " + this.precoKm + "\n" +
               " - Preço por hora " + this.precoHora + "\n" +
               " - Classificações: " + this.classificacoes.toString();
    }
    
    public Transportadora clone() {
        return new Transportadora(this);
    }
    
    public boolean aceitoTransporteMedicamentos() {
        return this.aceitoTransporteMedicamentos;
    }
    
    public void aceitaMedicamentos(boolean state) {
        this.aceitoTransporteMedicamentos = state;
    }
    
    public void classifica(int classificacao) {
        this.classificacoes.add(classificacao);
    }
    
    public boolean disponivelParaTransporte() {
        return this.disponivel;
    }
    
    public void setDisponibilidade(boolean disponivel) {
        this.disponivel = disponivel;
    }
    
    public double calculaCustoTransporte(double distancia, double duracao) {
        return precoKm * distancia + precoHora * duracao;
    }
    
}
