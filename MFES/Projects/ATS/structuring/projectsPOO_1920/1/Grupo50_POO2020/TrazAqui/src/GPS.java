import java.io.Serializable;

public class GPS  implements Serializable {
    private double longitude;
    private double latitude;

    public GPS(){
        this.latitude = 0;
        this.longitude = 0;
    }

    public GPS(double umLongitude, double umLatitude){
        this.latitude = umLongitude;
        this.longitude = umLatitude;
    }

    public GPS(GPS umGPS){
        this.latitude = umGPS.getLongitude();
        this.longitude = umGPS.getLatitude();
    }

    public GPS clone(){
        return new GPS(this);
    }

    public double getLongitude() {
        return this.latitude;
    }

    public double getLatitude(){
        return this.longitude;
    }

    public void setLongitude(double novaLongitude){
        this.latitude = novaLongitude;
    }

    public  void setLatitude(double novaLatitude){
        this.longitude = novaLatitude;
    }

    public boolean equals(Object o){
        if(this == o)
            return true;
        if(o == null || this.getClass() != o.getClass())
            return false;
        GPS g = (GPS) o;
        return this.latitude == g.getLatitude() &&
                this.longitude == g.getLongitude();
    }

    public String toString(){
        return "(Latitude = " + this.latitude + '|' + "Longitude = " + this.longitude + ')';
    }

    public String toCSV(){
        return this.latitude + "," + this.longitude;
    }

    public String toLog(){
        return this.latitude + "," + this.longitude;
    }

    public double distancia(GPS ponto){
        double ac = Math.abs(ponto.getLatitude() - this.getLatitude());
        double cb = Math.abs(ponto.getLongitude() - this.getLongitude());
        return Math.hypot(ac, cb);
    }

    public boolean dentroRaio(GPS localizacao, double raio){
        return this.distancia(localizacao) <= raio;

    }
}
