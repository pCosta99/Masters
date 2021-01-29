import java.io.Serializable;
import java.util.Objects;

/**
 * Classe que representa a localização de uma entidade num plano euclidiano.
 */
public class GPS implements Serializable {

    private double latitude;
    private double longitude;

    public GPS() {
        this.latitude = 0;
        this.longitude = 0;
    }

    public GPS(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    public GPS(GPS o){
        this.latitude = o.getLatitude();
        this.longitude = o.getLongitude();
    }

    public double getLatitude() {
        return latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GPS gps = (GPS) o;
        return  Double.compare(gps.getLatitude(), latitude) == 0 &&
                Double.compare(gps.getLatitude(), longitude) == 0;
    }

    public GPS clone(){
        return new GPS(this);
    }


    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append(latitude);
        sb.append(" ,").append(longitude);
        return sb.toString();
    }

    /**
     * Método que calcula a distancia  a outro ponto gps.
     * @param g O outro ponto gps.
     * @return Retorna a distancia euclidiana entre os dois pontos.
     */
    public double distancia(GPS g){
        double a = Math.abs(this.latitude-g.getLatitude());
        double b = Math.abs(this.longitude-g.getLongitude());
        return Math.sqrt(a*a+b*b);
    }
}
