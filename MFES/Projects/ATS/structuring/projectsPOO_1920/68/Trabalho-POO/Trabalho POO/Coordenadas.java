import java.io.Serializable;

public class Coordenadas implements Serializable {
    private double longitude;
    private double latitude;

    public static double distancia(Coordenadas c1, Coordenadas c2) {
        return Math.sqrt(Math.pow(c1.longitude - c2.getLongitude(), 2.0) + Math.pow(c1.latitude - c2.getLatitude(), 2.0));
    }

    public static double distanciaRota(Coordenadas c1, Coordenadas c2, Coordenadas c3) {
        return distancia(c1, c2) + distancia(c2, c3);
    }


    public Coordenadas() {
        this.longitude = 0.0;
        this.latitude = 0.0;
    }

    public Coordenadas(double longitude, double latitude) {
        this.longitude = longitude;
        this.latitude = latitude;
    }

    public Coordenadas(Coordenadas og) {
        this.longitude = og.getLongitude();
        this.latitude = og.getLatitude();
    }

    public double getLongitude() {
        return longitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    public double getLatitude() {
        return latitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        long temp;
        temp = Double.doubleToLongBits(latitude);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(longitude);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Coordenadas other = (Coordenadas) obj;
        if (Double.doubleToLongBits(latitude) != Double.doubleToLongBits(other.latitude))
            return false;
        if (Double.doubleToLongBits(longitude) != Double.doubleToLongBits(other.longitude))
            return false;
        return true;
    }

    public double distancia(Coordenadas c) {
        return Coordenadas.distancia(this, c);
    }

    public Coordenadas clone() {
        return new Coordenadas(this);
    }

}