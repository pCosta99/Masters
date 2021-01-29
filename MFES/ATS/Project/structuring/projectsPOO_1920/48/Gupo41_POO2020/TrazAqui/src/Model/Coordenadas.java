package Model;

import java.util.Collection;

public class Coordenadas implements java.io.Serializable{

    /**
     * Coordenada latitude
     */
    private double latitude;
    /**
     * Coordenada longitude
     */
    private double longitude;

    /**
     * Construtor vazio de Coordenadas
     */
    public Coordenadas(){
        this.latitude = 0;
        this.longitude = 0;
    }

    /**
     * Contrutor parametrizado de Coordenadas
     * @param latitude
     * @param longitude
     */
    public Coordenadas(double latitude, double longitude){
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * Construtor por cópia de Coordenadas
     * @param coordenadas
     */
    public Coordenadas (Coordenadas coordenadas){
        setLatitude(coordenadas.getLatitude());
        setLongitude(coordenadas.getLongitude());
    }

    /**
     * Devolve o valor de latitude
     * @return
     */
    public double getLatitude() {
        return this.latitude;
    }

    /**
     * Atualiza o valor de latitude
     * @param latitude
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Devolve o valor de longitude
     * @return
     */
    public double getLongitude() {
        return this.longitude;
    }

    /**
     * Atualiza o valor de longitude
     * @param longitude
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Verifica se umas coordenadas estão dentro de um raio de um utilizador
     * @param cord
     * @param raio
     * @return
     */
    public boolean dentroRaio(Coordenadas cord, double raio){
        if(this.latitude > cord.getLatitude()){
            if(this.latitude - raio > cord.getLatitude()) return false;
        }
        if(this.latitude < cord.getLatitude()){
            if(this.latitude + raio < cord.getLatitude()) return false;
        }

        if(this.longitude > cord.getLongitude()){
            if(this.latitude - raio > cord.getLongitude()) return false;
        }
        if(this.longitude < cord.getLongitude()){
            if(this.longitude + raio < cord.getLongitude()) return false;
        }

        return true;
    }

    /**
     * Calcula a distância a outro par de coordenadas
     * @param cord
     * @return
     */
    public double distanciaKm(Coordenadas cord){
        return Math.sqrt( Math.pow(this.latitude - cord.getLatitude(), 2) + Math.pow(this.longitude - cord.getLongitude(),2));
    }

    /**
     * Devolve uma cópia da instância
     * @return
     */
    @Override
    protected Coordenadas clone() {
        return new Coordenadas(this);
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o
     * @return
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Coordenadas coordenadas = (Coordenadas) o;
        return this.latitude == coordenadas.getLatitude() &&
               this.longitude == coordenadas.getLongitude();
    }

    /**
     * Devolve uma representação textual de Coordenadas
     * @return
     */
    @Override
    public String toString() {
        return "Model.Coordenadas{" +
                "latitude=" + latitude +
                ", longitude=" + longitude +
                '}';
    }


}
