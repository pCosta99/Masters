/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.List;
import java.util.ArrayList;

public class Voluntario extends Entidade implements TransportadorMedicamentos, Classificavel, Transportador {
    
    private double raio;
    private boolean aceitoTransporteMedicamentos;
    private List<Integer>classificacoes;
    private boolean disponivel;
    
    public Voluntario() {
        super();
        this.raio = 0;
        this.classificacoes = new ArrayList<>();
    }
    
    public Voluntario(String codigo, String nome, GPS gps, String email, String password, double raio) {
        super(codigo, nome, gps, email, password);
        this.raio = raio;
        this.aceitoTransporteMedicamentos = false;
        this.classificacoes = new ArrayList<>();
    }
    
    public Voluntario(Voluntario v) {
        super(v);
        this.raio = v.getRaio();
        this.aceitoTransporteMedicamentos = false;
        this.classificacoes = v.getClassificacoes();
    }
    
    public double getRaio() {
        return this.raio;
    }
    
    public void setRaio(double raio) {
        this.raio = raio;
    }
    
    public List<Integer> getClassificacoes() {
        return new ArrayList<>(this.classificacoes);
    }
    
    public boolean equals(Object obj) {
       if(this == obj) return true;
       if((obj == null) || (this.getClass() != obj.getClass())) return false;
       Voluntario v = (Voluntario) obj;
       return super.equals(v) && 
              this.raio == v.getRaio() && 
              this.aceitoTransporteMedicamentos == v.aceitoTransporteMedicamentos() &&
              this.classificacoes.equals(v.getClassificacoes());
    }
    
    public String toString() {
        return "Voluntario: " + "\n" +
               " - Código: " + this.getCodigo() + "\n" +
               " - Nome: " + this.getNome() + "\n" +
               " - GPS: " + this.getGPS().toString() + "\n" +
               " - Raio: " + this.raio + "\n" +
               " - Aceita transporte de medicamentos " + this.aceitoTransporteMedicamentos + "\n" +
               " - Classificações: " + this.classificacoes.toString();
    }
    
    public Voluntario clone() {
        return new Voluntario(this);
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
        return 0;
    }
    
}
