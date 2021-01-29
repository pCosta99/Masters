import java.io.Serializable;

public class Location implements Serializable
{
    private double longitude;
    private double latitude;   
   
    public Location(){
        this.longitude = 0;
        this.latitude = 0;
    }
    
    public Location(double latitude, double longitude) {
        this.latitude  = latitude;
        this.longitude = longitude;
    }
    
    public Location(Location l){
        this.longitude = l.getLongitude();
        this.latitude = l.getLatitude();
    }
    
    public double getLongitude(){return this.longitude;}
    
    public double getLatitude(){return this.latitude;}

    public double distanceTo(Location that) {
        double STATUTE_MILES_PER_NAUTICAL_MILE = 1.15077945;
        double NAUTICAL_MILE_TO_KM = 1.609344;
        double lat1 = Math.toRadians(this.latitude);
        double lon1 = Math.toRadians(this.longitude);
        double lat2 = Math.toRadians(that.latitude);
        double lon2 = Math.toRadians(that.longitude);

        double angle = Math.acos(Math.sin(lat1) * Math.sin(lat2)
                               + Math.cos(lat1) * Math.cos(lat2) * Math.cos(lon1 - lon2));
                               
        double nauticalMiles = 60 * Math.toDegrees(angle);
        double statuteMiles = STATUTE_MILES_PER_NAUTICAL_MILE * nauticalMiles;
        double kms = statuteMiles * NAUTICAL_MILE_TO_KM;
        return kms;
    }  

    public String toString() {
        return "(" + latitude + ", " + longitude + ")";
    }
    
    public Location clone(){
        return new Location(this);
    }
}
