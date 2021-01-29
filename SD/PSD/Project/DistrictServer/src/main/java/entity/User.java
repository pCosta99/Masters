package entity;

import util.Coordinates;

import java.util.HashSet;
import java.util.Set;

public class User {

    // The username of the user
    private final Username username;

    // Current coordinates of the user
    private Coordinates coordinates;

    // Place where the user have been
    private final Set<Coordinates> places;

    private boolean isInfected;
    private boolean hasAlreadyBeenInfected;

    public User(Username username, Coordinates coordinates) {
        this.username = username;
        this.places = new HashSet<>();

        // Add the received location as a place where this user have been
        saveNewLocation(coordinates);

        this.coordinates = coordinates;
        this.isInfected = false;
        this.hasAlreadyBeenInfected = false;
    }

    public Username getUsername() {
        return username;
    }

    public Coordinates getCurrentCoordinates() {
        return coordinates;
    }

    /**
     * Method that updates the user location and saves his new location
     * so whe can know where the user have been.
     */
    public void updateLocation(Coordinates location){
        this.coordinates = location;

        saveNewLocation(location);
    }

    /**
     * Add a new location where this user have been.
     * @param location The location to add.
     */
    private void saveNewLocation(Coordinates location){
        this.places.add(location);
    }

    /**
     * Returns a set of coordinates, that represents all of the locations
     * where this user passed.
     * @return The locations where user passed.
     */
    public Set<Coordinates> locationsWhereUserPassed(){
        return this.places;
    }

    public boolean isInfected() {
        return isInfected;
    }

    public void setInfected(boolean infected) {
        isInfected = infected;
    }

    /**
     * Set's the current user status to infected, and mark the user as previously infected.
     */
    public void infected() {
        isInfected = true;
        hasAlreadyBeenInfected = true;
    }

    public boolean getHasAlreadyBeenInfected() {
        return hasAlreadyBeenInfected;
    }

    public void setHasAlreadyBeenInfected(boolean hasAlreadyBeenInfected) {
        this.hasAlreadyBeenInfected = hasAlreadyBeenInfected;
    }
}
