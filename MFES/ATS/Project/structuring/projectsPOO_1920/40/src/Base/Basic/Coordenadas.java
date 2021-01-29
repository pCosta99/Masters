package Base.Basic;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

public class Coordenadas implements Serializable{
    private double latitude;
    private double longitude;


    public Coordenadas() {
    }

    public Coordenadas(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    public Coordenadas(Coordenadas x) {
        this.latitude = x.getLatitude();
        this.longitude = x.getLongitude();
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

    public Coordenadas latitude(double latitude) {
        this.latitude = latitude;
        return this.clone();
    }

    public Coordenadas longitude(double longitude) {
        this.longitude = longitude;
        return this.clone();
    }

    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Coordenadas)) {
            return false;
        }
        Coordenadas coordenadas = (Coordenadas) o;
        return latitude == coordenadas.latitude && longitude == coordenadas.longitude;
    }

    @Override
    public int hashCode() {
        return Objects.hash(latitude, longitude);
    }

    @Override
    public String toString() {
        return "{" +
            " latitude='" + getLatitude() + "'" +
            ", longitude='" + getLongitude() + "'" +
            "}";
    }

    public Coordenadas clone(){
        return new Coordenadas(this);
    }

    public double distance(Coordenadas c) {
        double x = this.latitude - c.latitude;
        double y = this.longitude - c.longitude;
        return Math.sqrt(Math.pow(x,2) + Math.pow(y,2));
    }

    public List<Double> distance(List<Coordenadas> cs) {
        List<Double> ret = new ArrayList<>();
        for (Coordenadas c : cs) {
            ret.add(this.distance(c));
        }
        return ret;
    }

    public double distanceSequencial(List<Coordenadas> cs) {
        double ret = 0.0;
        Coordenadas buf = this;
        for (Coordenadas c : cs) {
            ret += buf.distance(c);
            buf = c;
        }
        return ret;
    }

    public double distanceSequencial(Set<Coordenadas> cs) {
        double ret = 0.0;
        Coordenadas buf = this;
        for (Coordenadas c : cs) {
            ret += buf.distance(c);
            buf = c;
        }
        return ret;
    }

    public boolean isNextTo(Coordenadas c, double raio) {
        return (this.distance(c) > raio ? false : true);
    }
} 