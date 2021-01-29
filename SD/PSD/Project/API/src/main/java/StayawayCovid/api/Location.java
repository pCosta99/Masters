package StayawayCovid.api;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.hibernate.validator.constraints.Length;

public class Location {

    private String district;
    private Coordinate coordinate;
    private Integer users;          // Count of users currently in this location
    private Integer maxUsers;       // Count of the máx user

    public Location(String district, Coordinate coordinate) {
        this.district = district;
        this.coordinate = coordinate;
        users = 0;
        maxUsers = 0;
    }

    public String getDistrict() {
        return district;
    }

    public Coordinate getCoordinate() {
        return coordinate;
    }

    public Integer getUsers() {
        return users;
    }

    public Integer getMaxUsers() {
        return maxUsers;
    }
    
    // Add a new user to this location and update maxUsers if a new record is established
    public void addUser() {
        users++;
        if(users > maxUsers) maxUsers = users;
    }

    public void removeUser() {
        users--;
    }
}