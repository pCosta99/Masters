import static java.lang.StrictMath.sqrt;
import java.io.Serializable;

public class GPS implements Serializable{

    private double latitude;
    private double longitude;

    public GPS(){
        this.latitude = 0;
        this.longitude = 0;
    }

    public GPS(double latitude,double longitude){
        this.latitude= latitude;
        this.longitude= longitude;
    }

    public GPS(GPS coordenadas){
        this.latitude = coordenadas.getLatitude();
        this.longitude = coordenadas.getLongitude();
    }

    public double getLatitude() {
        return this.latitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public double getLongitude() {
        return this.longitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GPS coordenadas = (GPS) o;
        return this.latitude == coordenadas.getLatitude() && this.longitude == coordenadas.getLongitude();
    }

    public GPS clone(){
        return new GPS(this);
    }

    public double distEntre(GPS l){
        return sqrt(Math.pow((l.getLatitude() - this.getLatitude()), 2) + Math.pow((l.getLongitude() - this.getLongitude()), 2));
    }

    public boolean inRangeT(Transportadora t){
        return t.getRaio() >= this.distEntre(t.getLocalizacao());
    }

    public boolean inRangeV(Voluntario v) {
        return v.getRaio() >= this.distEntre(v.getLocalizacao());
    }
}
