import java.io.Serializable;

/**
 * Classe que lida com a informação de uma localização.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class Localizacao implements Serializable {
    // Instance Variables
    private double latitude;
    private double longitude;

    // Constructors

    /**
     * Construtor de uma localização.
     */
    public Localizacao() {
        this.latitude = 0;
        this.longitude = 0;
    }

    /**
     * Construtor de uma localização.
     * @param latitude, Latitude da localização a construir.
     * @param longitude, Longitude da localização a construir.
     */
    public Localizacao(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * Construtor de uma localização.
     * @param l, Localização a construir.
     */
    public Localizacao(Localizacao l) {
        this.latitude = l.getLatitude();
        this.longitude = l.getLongitude();
    }

    // Gets

    /**
     * Função que retorna a latitude.
     * @return Latitude
     */
    public double getLatitude() {
        return this.latitude;
    }

    /**
     * Função que retorna a longitude.
     * @return Longitude.
     */
    public double getLongitude() {
        return this.longitude;
    }

    // Sets

    /**
     * Função que modifica a latitude.
     * @param latitude, Nova latitude.
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Função que modifica a longitude.
     * @param longitude, Nova longitude.
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    //

    /**
     * Função que converte os parametros da localização para String.
     * @return String com os parametros da localização.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Latitude: ").append(this.latitude).append(" | ");
        sb.append("Longitude: ").append(this.longitude);
        return sb.toString();
    }

    /**
     * Função que cria um clone de uma Localização.
     * @return Localização clonada.
     */
    public Localizacao clone() {
        return new Localizacao(this);
    }

    /**
     * Função que compara parametros da Localização.
     * @param o, Objeto a comparar.
     * @return 'true' se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Localizacao l = (Localizacao) o;
        return this.latitude == l.getLatitude() &&
               this.longitude == l.getLongitude();
    }

    //

    /**
     * Função que calcula a distância.
     * @param l, Localização a partir da qual se irá retirar alguns parametros para calcular a distância.
     * @return Distância.
     */
    public double distance(Localizacao l) {
        double x = this.latitude - l.getLatitude();
        double y = this.longitude - l.getLongitude();
        return Math.sqrt(x * x + y * y);
    }

    /**
     * Função que verifica se uma localização está a uma distância menor ou igual a um máximo fornecido.
     * @param l, Localização a partir da qual se irá determinar a distância.
     * @param max, Máximo.
     * @return 'true' se estiver a uma distância menor ou igual.
     */
    public boolean closerOrEqual(Localizacao l, double max) {
        return this.distance(l) <= max;
    }
}
