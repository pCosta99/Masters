import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;


/**
 * Classe que representa uma carrinha.
 */

public class Carrinha extends Transporte implements Serializable {
    private List<Encomenda> encomendas;


    ///////////////////////////////////// Construtor ///////////////////////////////////////////

    /**
     * Construtor de 'Carrinha'. 
     * É possível variar-se o preço por km, o preço por kg e a velocidade média do veículo.
     */
    public Carrinha(double taxaKm, double taxaKg, double velocidadeMedia) {
        super(taxaKm, taxaKg, 50);
        this.encomendas = new ArrayList<>();
    }

    ///////////////////////////////////// Encomendas ///////////////////////////////////////////

    /**
     * Devolve a lista de encomendas que a carrinha transporta.
     * @return
     */
    public List<Encomenda> getEncomendas(){
        return encomendas;
    }

    /**
     * Adiciona uma encomenda à carrinha.
     */
    public void addEncomenda(Encomenda e) {
        this.encomendas.add(e);
    }

    /**
     * Entregou todas as encomendas. A carrinha passa a estar disponível, com nenhuma encomenda por entregar.
     */
    public void libertaEncomendas() {
        this.encomendas.clear();
        this.setDisponibilidade(true);
    }

    ///////////////////////////////////// Outros métodos ///////////////////////////////////////////

    public double tempoViagem(double distancia) {
        return distancia / super.getVelocidadeMedia();
    }

    ///////////////////////////////////// Métodos que não estáo a ser usados ///////////////////////////////////////////


    public LocalDateTime horaPrevistaEntrega(Coordenadas origem, Coordenadas destino) {
        long minutos = (long) (tempoViagem(origem,destino) * 60);
        return LocalDateTime.now().plusMinutes(minutos);
    }

    public double tempoViagem(Coordenadas origem, Coordenadas destino) {
        return origem.distancia(destino) / super.getVelocidadeMedia();
    }


}
