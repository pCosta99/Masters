package StayawayCovid.api;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.hibernate.validator.constraints.Length;
import javax.ws.rs.NotFoundException;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

public class District {

    private String name;
    private Map<String, Location> locations;		        // Map with all the locations of this district
    private Integer usersCount;								// Count of users from this district
    private Integer infectedCount;							// Count of users that declared being infected in this district
    private Integer infectedContacts;						// Sum of the number of contacts the reported infected users in this district had

    public District(String name, Integer size) {
        this.name = name;
        this.usersCount = 0;
        infectedCount = 0;
        infectedContacts = 0;
        locations = new HashMap<>();
    	for(int i=0; i<size; i++) 
    		for(int j=0; j<size; j++)
    			locations.put((new Coordinate(i,j)).toString(),new Location(name, new Coordinate(i,j)));
    }

    @JsonProperty
    public String getName() {
        return name;
    }

    @JsonProperty
    public Map<String,Location> getLocations() {
        return locations;
    }

    @JsonProperty
    public Integer getUsersCount() {
        return usersCount;
    }

    @JsonProperty
    public Integer getInfectedCount() {
        return infectedCount;
    }

    @JsonProperty
    public Integer getInfectedContacts() {
        return infectedContacts;
    }

    @JsonProperty 
    public Float getInfectedContactsRatio() {
        if(infectedCount == 0) return 0;
    	return (float)infectedContacts/(float)infectedCount;
    }

    // Increment the user count of the district and updates its location with a new user
    @JsonProperty
    public void addUser(Integer latitude, Integer longitude) {
    	usersCount++;
    	Coordinate coordinate = new Coordinate(latitude,longitude);
    	Location location = locations.get(coordinate.toString());
        if(location == null) System.out.println("Error obtaining location!");
    	location.addUser();
    	locations.put(coordinate.toString(),location);
    }

    @JsonProperty
    public void addInfected(Integer contacts) {
    	infectedCount++;
    	infectedContacts += contacts;
    }

    // Moves a user from one location to another inside the same district
    @JsonProperty
    public void move(Integer initialLatitude, Integer initialLongitude, Integer finalLatitude, Integer finalLongitude) throws NotFoundException {
    	Coordinate initialCoord = new Coordinate(initialLatitude, initialLongitude);
    	Coordinate finalCoord = new Coordinate(finalLatitude, finalLongitude);
    	
    	Location initialLocal = locations.get(initialCoord.toString());
    	if(initialLocal == null) 
    		throw new NotFoundException("Invalid initial location");

    	Location finalLocal = locations.get(finalCoord.toString());
    	if(finalLocal == null) 
    		throw new NotFoundException("Invalid final location");

    	initialLocal.removeUser();
    	locations.put(initialCoord.toString(),initialLocal);

    	finalLocal.addUser();
    	locations.put(finalCoord.toString(),finalLocal);
    }
}
