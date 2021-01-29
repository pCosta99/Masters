package utils;

public class Coordinate {

    private String latitude;
    private String longitude;

    public Coordinate(String latitude, String longitude){
        this.latitude = latitude;
        this.longitude = longitude;
    }

    public String getLatitude(){
        return this.latitude;
    }

    public String getLongitude(){
        return this.latitude;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(latitude);
        sb.append("x");
        sb.append(longitude);
        return sb.toString();
    }
}
