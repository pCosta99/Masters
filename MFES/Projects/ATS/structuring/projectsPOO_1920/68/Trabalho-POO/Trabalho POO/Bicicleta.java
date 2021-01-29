import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.TreeMap;

public class Bicicleta extends Transporte implements Serializable {
    Encomenda encomenda;

    ///////////////////////////////////// Construtor ///////////////////////////////////////////

    public Bicicleta() {
        super(0, 0, 20);
    }

    ///////////////////////////////////// Outros métodos ///////////////////////////////////////////

    public double tempoViagem(double distancia) {
        return distancia / this.getVelocidadeMedia();
    }

    public void addEncomenda(Encomenda e) {
        this.encomenda = e;
        this.setDisponibilidade(false);
    }

    public void libertaEncomenda() {
        this.encomenda = null;
        this.setDisponibilidade(true);
    }

    ////////////////////////////////////// Funções que para já não são usadas //////////////////////////////////////

    public String toString(){
        return "Bicicleta";
    }

    /**
     * Retorna o custo de transportar uma encomenda.
     * @param origem coordenadas da transportadora
     * @param destino coordenadas do utilizador que solicitou a encomenda
     * @return o custo do transporte
     */
    public double custo(Coordenadas origem, Coordenadas destino) {
        return this.encomenda.getPeso() * origem.distancia(destino) * super.getTaxaKm();
    }

    public double custo(double distancia) {
        return this.getTaxaKm() * distancia;
    }

    /**
     * Tempo médio do transporte da encomenda.
     * @param origem coordenadas da transportadora
     * @param destino  coordenadas do utilizador que solicitou a encomenda
     * @return o tempo médio do transporte
     */
    public double tempoViagem(Coordenadas origem, Coordenadas destino) {
        return origem.distancia(destino) / super.getVelocidadeMedia();
    }

    /**
     * Retorna a hora prevista da entrega da encomenda.
     * @param origem coordenadas da transportadora
     * @param destino coordenadas do utilizador que solicitou a encomenda
     * @return a hora prevista da entrega da encomenda
     */
    public LocalDateTime horaPrevistaEntrega(Coordenadas origem, Coordenadas destino) {
        long minutos = (long) (tempoViagem(origem,destino) * 60);
        return LocalDateTime.now().plusMinutes(minutos);
    }

    public Bicicleta clone(){
        Bicicleta res = new Bicicleta();
        if(this.encomenda==null){res.encomenda=null;}
        else {
            res.encomenda = this.encomenda.clone();
        }
        return res;
    }

    public Encomenda getEncomenda() {
        return this.encomenda.clone();
    }

    public Map<Encomenda, TempoCustoEncomenda> transporta() {
        Map<Encomenda, TempoCustoEncomenda> encomendasEntregues = new TreeMap<>();
        encomendasEntregues.put(this.encomenda, new TempoCustoEncomenda(0, 0.0, LocalDateTime.now()));
        return encomendasEntregues;
    }



}