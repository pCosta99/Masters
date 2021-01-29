package Model;

import java.io.Serializable;

/** Declaration of Class Location used to treat GPS coordinates.
 *
 * double longitude - The longitude coordinate (x in a 2D space such as the one we're working on).
 * double latitude - The latitude coordinate (y in a 2D space such as the one we're working on).
 */
public class Location implements Serializable {
    private double longitude;
    private double latitude;

    /* Class constructors*/
    public Location(){
        this.longitude = 0.0;
        this.latitude = 0.0;
    }
    public Location(double lon, double lat){
        this.longitude = lon;
        this.latitude = lat;
    }
    public Location(Location l){
        this.longitude = l.getLongitude();
        this.latitude = l.getLatitude();
    }

    /* Getters and Setters*/
    double getLongitude(){
        return this.longitude;
    }
    void setLongitude(double lon){
        this.longitude = lon;
    }

    double getLatitude(){
        return this.latitude;
    }
    void setLatitude(double lat){
        this.latitude = lat;
    }

    @Override
    public String toString(){
        return "(" + latitude + ", " + longitude + ")";
    }
    @Override
    public Location clone(){
        return new Location(this);
    }
    public boolean equals(Location l){
        return ((this.longitude == l.getLongitude()) && (this.latitude == l.getLatitude()));
    }

    /** Measures the distance between the current Location in scope and the one given as a parameter.
     * This is done as if we were looking at these coordinates in a 2D space instead of the GPS system that works on a spheric space.
     *
     * @param l - Distance to which we'll measure the distance starting on the Location in scope.
     * @return - the length of a straight line connecting the two Locations.
     */
    public double distanceTo(Location l){
        return Math.sqrt(Math.pow(l.getLongitude(),2) + Math.pow(l.getLatitude(),2));
    }
}
