import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Transportadora extends Empresa implements Serializable {
    /** variaveis de instancia */
    private String codTransportadora;
    private double raio;
    private double precoPorKm;
    private double precoPorMin;
    private boolean disponibilidade;
    private List<DadosEntrega> historico;
    //double custoEuros(w,dist);
    //int	quantEncomendas (1 ou N rota – otimizar entregas a vários clientes);

    /** variaveis de classe */
    private static int totalTransportadoras = 0; //necessario para ir incrementando o codigo da transportadora

    /** constructores de classe */
    /** vazio */
    public Transportadora(){
        super();
        this.codTransportadora = "";
        this.raio = 0.0;
        this.precoPorKm = 0.0;
        this.precoPorMin = 0.0;
        this.disponibilidade = true;
        this.historico = new ArrayList<>();
    }

    /** parametrico */
    public Transportadora(String newCodTransportadora, String newNome, double newGpsX, double newGpsY,
                          String newEmail, String newPassword, String newNif, double newRaio,
                          double newPrecoPorKm, double newPrecoPorMin, boolean newDisponibilidade,List<DadosEntrega> newHistorico){
        super(newNome, newGpsX, newGpsY, newEmail, newPassword, newNif);
        this.codTransportadora = newCodTransportadora;
        this.raio = newRaio;
        this.precoPorKm = newPrecoPorKm;
        this.precoPorMin = newPrecoPorMin;
        this.disponibilidade = newDisponibilidade;
        this.setHistorico(newHistorico);
    }

    //unico construtor que dá codTransportadora
    public Transportadora(String email, String password){
        super();
        this.setEmail(email);
        this.setPassword(password);
        Transportadora.totalTransportadoras++;
        this.codTransportadora = "t" + Transportadora.totalTransportadoras;
        this.raio = 0.0;
        this.precoPorKm = 0.0;
        this.precoPorMin = 0.0;
        this.disponibilidade = true;
    }

    /** copia */
    public Transportadora(Transportadora newTransportadora){
        super(newTransportadora);
        this.codTransportadora = newTransportadora.getCodTransportadora();
        this.raio = newTransportadora.getRaio();
        this.precoPorKm = newTransportadora.getPrecoPorKm();
        this.precoPorMin = newTransportadora.getPrecoPorMin();
        this.disponibilidade = newTransportadora.getDisponibilidade();
    }

    /** gets/sets das variaveis de instancia */
    public String getCodTransportadora(){ return this.codTransportadora; }
    public void setCodTransportadora(String newCodTransportadora){ this.codTransportadora = newCodTransportadora; }

    public double getRaio(){ return this.raio; }
    public void setRaio(double newRaio){ this.raio = newRaio; }

    public double getPrecoPorKm(){ return this.precoPorKm; }
    public void setPrecoPorKm(double newPrecoPorKm){ this.precoPorKm = newPrecoPorKm; }

    public double getPrecoPorMin(){
        return this.precoPorMin;
    }
    public void setPrecoPorMin(double newPrecoPorMin){
        this.precoPorMin = newPrecoPorMin;
    }

    public boolean getDisponibilidade() { return this.disponibilidade; }
    public void setDisponibilidade(boolean newDisponibilidade){ this.disponibilidade = newDisponibilidade; }

    public List<DadosEntrega> getHistorico() {
        List<DadosEntrega> res = new ArrayList<>();

        for(DadosEntrega de : this.historico){
            res.add(de.clone());
        }

        return res;
    }
    public void setHistorico(List<DadosEntrega> historico) {
        for(DadosEntrega de : historico){
            this.historico.add(de.clone());
        }
    }

    /** metodos override */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Transportadora passed = (Transportadora) o;
        return (super.equals(passed) &&
                this.codTransportadora.equals(passed.getCodTransportadora()) &&
                this.raio == passed.getRaio() &&
                this.precoPorKm == passed.getPrecoPorKm() &&
                this.precoPorMin == passed.getPrecoPorMin() &&
                this.disponibilidade == passed.getDisponibilidade());
    }

    public String toString(){
        return "Transportadora:" + this.codTransportadora + "," + super.toString() + "," +
                Double.toString(this.raio) + "," + Double.toString(this.precoPorKm) +
                "," + Double.toString(this.precoPorMin) + "," + Boolean.toString(this.disponibilidade);
    }

    public Transportadora clone(){
        return new Transportadora(this);
    }

    /** metodos especificos */

    /**
     * sinalizar que estão dispostos para recolher encomendas
     */
    public void insereHistorico(DadosEntrega historico) {
        this.historico.add(historico);
    }

    /**
     * determinar o preço de transporte de uma encomenda em função da distância e do tempo
     * de espera na loja (estimado ou fornecido pela loja)
     */
    public double precoEntrega(Loja loja,Utilizador utilizador,double tempo, double distancia){
        double res;
        res = (this.precoPorKm * distancia + this.precoPorMin * tempo);
        return res;
    }

    public boolean podeEntregar(Encomenda encomenda){
        boolean res = true;
        Utilizador user = encomenda.getUtilizador();
        Loja loja = encomenda.getLoja();
        if(super.getGps().distancia(loja.getGps())>this.raio) res = false;
        if(super.getGps().distancia(user.getGps())>this.raio) res = false;

        return false;
    }

    public double distanciaEntrega(Loja loja,Utilizador utilizador){
        double res;
        res = (super.getGps().distancia(loja.getGps()) + loja.getGps().distancia(utilizador.getGps()));
        return res;
    }

    /**
     * fazer o transporte da encomenda e registar quanto tempo demorou e o custo associado
     */
    // Com isto feito assim penso que será possivel guardar as informações relativas á entrega
    // e se o utilizador aceitar é so necessario remover a entrega da lista e fornecer os dados,

    public DadosEntrega proporEntrega(Encomenda encomenda){
        DadosEntrega res = new DadosEntrega();

        Loja loja = encomenda.getLoja();
        Utilizador user = encomenda.getUtilizador();

        double tempo = loja.tempoDeEspera();
        double distancia = this.distanciaEntrega(loja,user);

        res.setCodEntrega(encomenda.getCodEncomenda());
        res.setCodTransportadora(this.getCodTransportadora());
        res.setCusto(this.precoEntrega(loja,user,tempo,distancia));
        res.setTempo(tempo);
        res.setKms(distancia);

        return res;
    }

    public double totalKms(){
        double total = 0.0;
        for(DadosEntrega de : this.historico){
            total += de.getKms();
        }
        return total;
    }
    //todo

}
