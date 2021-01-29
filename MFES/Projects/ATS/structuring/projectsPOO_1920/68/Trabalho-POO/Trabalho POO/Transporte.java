import java.io.Serializable;
import java.time.LocalDateTime;

public abstract class Transporte implements Serializable {
    private boolean disponibilidade;
    private double taxa_por_km;
    private double taxa_por_kg;
    private double velocidade_media;


    ////////////////////////////////////// Construtores //////////////////////////////////////

    public Transporte() {
        this(0.75, 1.0, 30);
    }

    public Transporte(double taxa_por_km, double taxa_por_kg, double velocidade) {
        this.disponibilidade = true;
        this.taxa_por_km = taxa_por_km;
        this.taxa_por_kg = taxa_por_kg;
        this.velocidade_media = velocidade;
    }

    ////////////////////////////////////// Getters e setters //////////////////////////////////////


    public boolean isDisponivel() {
        return this.disponibilidade;
    }

    public void setDisponibilidade(boolean disponibilidade) {
        this.disponibilidade = disponibilidade;
    }

    public double getTaxaKm() {
        return this.taxa_por_km;
    }

    public void setTaxaKm(double taxa) {
        this.taxa_por_km = taxa;
    }

    public double getTaxaKg() {
        return this.taxa_por_kg;
    }

    public void setTaxaKg(double taxa) {
        this.taxa_por_kg = taxa;
    }

    public double getVelocidadeMedia() {
        return this.velocidade_media;
    }

    public void setVelocidadeMedia(double velocidade) {
        this.velocidade_media = velocidade;
    }

    ////////////////////////////////////// Outros métodos //////////////////////////////////////

    public abstract LocalDateTime horaPrevistaEntrega(Coordenadas origem, Coordenadas destino);

    public abstract void addEncomenda(Encomenda e);

    public abstract double tempoViagem(Coordenadas c1, Coordenadas c2);

}