import java.io.Serializable;

public class DadosEntrega implements Serializable {
    // Classe que guarda todos os dados respetivos a uma encomenda, será usada ao realizar/propor encomenda
    /** variaveis de instancia */
    private String codEntrega;
    private String codTransportadora;
    private double custo;
    private double tempo;
    private double kms;

    /** constructores de classe */
    /** vazio */
    public DadosEntrega(){
        this.codEntrega = "";
        this.codTransportadora = "";
        this.custo = 0.0;
        this.tempo = 0.0;
        this.kms = 0.0;
    }

    /** Parametrico */
    public DadosEntrega(String codEntrega,String codTransportadora,double custo, double tempo, double kms){
        this.codEntrega = codEntrega;
        this.codTransportadora = codTransportadora;
        this.custo = custo;
        this.tempo = tempo;
        this.kms = kms;
    }

    /** Copia */
    public DadosEntrega(DadosEntrega de){
        this.codEntrega = de.getCodEntrega();
        this.codTransportadora = de.getCodTransportadora();
        this.custo = de.getCusto();
        this.tempo = de.getTempo();
        this.kms = de.getKms();
    }



    /** gets/sets das variaveis de instancia */
    public String getCodEntrega() {
        return codEntrega;
    }
    public void setCodEntrega(String codEntrega) {
        this.codEntrega = codEntrega;
    }

    public String getCodTransportadora() {
        return codTransportadora;
    }
    public void setCodTransportadora(String codTransportadora) {
        this.codTransportadora = codTransportadora;
    }

    public double getCusto() {
        return custo;
    }
    public void setCusto(double custo) {
        this.custo = custo;
    }

    public double getTempo() {
        return tempo;
    }
    public void setTempo(double tempo) {
        this.tempo = tempo;
    }

    public double getKms() {
        return kms;
    }
    public void setKms(double kms) {
        this.kms = kms;
    }

    /** metodos override */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        DadosEntrega aux = (DadosEntrega) o;
        return (this.codEntrega.equals(aux.getCodEntrega()) &&
                this.codTransportadora.equals(aux.getCodTransportadora()) &&
                this.custo == aux.getCusto() &&
                this.tempo == aux.getTempo() &&
                this.kms == aux.getKms());
    }

    public DadosEntrega clone(){
        return new DadosEntrega(this);
    }

    public String toString(){
        return "Dados da Entrega: "+ "Encomenda:"+ this.codEntrega + ", Custo: " + Double.toString(this.custo) + ", Tempo: " + Double.toString(this.tempo) + ", Kms: " + Double.toString(this.kms);
    }
}
