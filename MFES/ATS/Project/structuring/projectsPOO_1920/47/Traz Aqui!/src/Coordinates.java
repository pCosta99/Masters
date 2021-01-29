import java.io.Serializable;

public class Coordinates implements Serializable {
    private double latitude;
    private double longitude;

    //Object Coordinates Constructor.
    public Coordinates(){
        latitude=0;
        longitude=0;
    }
    public Coordinates(double latitude, double longitude){
        this.latitude =latitude;
        this.longitude=longitude;
    }
    public Coordinates(Coordinates coord){
        this.latitude=coord.getLatitude();
        this.longitude=coord.getLongitude();
    }

    //Getters e Setters.
    public double getLatitude() {
        return latitude;
    }
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }
    public double getLongitude() {
        return longitude;
    }
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }


    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Coordinates that = (Coordinates) o;
        return Double.compare(that.latitude, latitude) == 0 &&
                Double.compare(that.longitude, longitude) == 0;
    }

    public String toString() {
        return "Coordinates{" +
                "latitude=" + latitude +
                ", longitude=" + longitude +
                '}';
    }

    protected Coordinates clone(){
        return new Coordinates(this);
    }
}
