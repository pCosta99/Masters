import entity.User;
import entity.Username;
import util.Coordinates;

import java.util.*;

public class DistrictStore {

    // Map where the key is the username from the user, and the value all user info (places where he as been, his current location, etc)
    private final Map<Username, User> usersStatus;

    // Map that for each coordinate stores a list of users that have been in that location
    private final Map<Coordinates, Set<Username>> usersPerCoordinatesUntilNow;

    // Map that for each coordinate stores the number of people in that location
    private final Map<Coordinates, Integer> numberOfPeopleInLocation;

    // Value that represents the district size
    private final int districtSize;

    public DistrictStore(int size){
        this.usersStatus = new HashMap<>();
        this.usersPerCoordinatesUntilNow = new HashMap<>();
        this.numberOfPeopleInLocation = new HashMap<>();
        this.districtSize = size;

        initializeCoordinatesMaps(size);
    }

    /**
     * Method that updates the user coordinates of a given user.
     * Also it verifies if any new notification should be sent.
     * @param username The username of the user which coordinates will be updated.
     * @param coordinates The new coordinates.
     * @return List of updates info.
     */
    public List<UpdateInfo> updateUserCoordinates(Username username, Coordinates coordinates){

        List<UpdateInfo> updateInfo = new ArrayList<>();

        // In case it's a new user
        if (!usersStatus.containsKey(username)){
            User user = new User(username, coordinates);
            this.usersStatus.put(username, user);

            // Is new user;
            updateInfo.add(UpdateInfo.NEW_USER);
        } else {
            User user = usersStatus.get(username);

            // Get the old coordinates of the user (not updated yet)
            Coordinates oldCoordinates = user.getCurrentCoordinates();

            // Get the number of people in the previous location of user
            int previousNrPeopleInPastLocation = this.numberOfPeopleInLocation.get(oldCoordinates);

            int newNrPeopleInPastLocation = previousNrPeopleInPastLocation - 1;

            // Verify if when the user leaves this location that the concentration will decrease
            if ((!coordinates.equals(user.getCurrentCoordinates())) &&
                    (previousNrPeopleInPastLocation > Math.round(Config.CONCENTRATION_PERCENTAGE * districtSize) && // If before it was with high concentration
                            newNrPeopleInPastLocation <= Math.round(Config.CONCENTRATION_PERCENTAGE * districtSize))) { // Verify if the new nr of people in the location will cause a decrease

                // In this case there is a concentration decrease
                updateInfo.add(UpdateInfo.CONCENTRATION_DECREASE);
            }

            // Update the number of people in location by removing this user from his previous location
            if (!coordinates.equals(user.getCurrentCoordinates()))
                this.numberOfPeopleInLocation.replace(oldCoordinates, newNrPeopleInPastLocation);

            // If the location doesn't have more people
            if (this.numberOfPeopleInLocation.get(oldCoordinates) == 0)
                updateInfo.add(UpdateInfo.NO_PEOPLE);

            // Update the user location
            user.updateLocation(coordinates);
        }

        // Previous number of people in new location + 1
        int newNrPeopleInNewLocation = this.numberOfPeopleInLocation.get(coordinates) + 1;

        // Update the number of people in the new location
        this.numberOfPeopleInLocation.replace(coordinates, newNrPeopleInNewLocation);

        if (newNrPeopleInNewLocation > Math.round(Config.CONCENTRATION_PERCENTAGE * districtSize))
            updateInfo.add(UpdateInfo.CONCENTRATION_RAISE);

        // Add the user to the list that contains the users that have been a specific location
        usersPerCoordinatesUntilNow.get(coordinates).add(username);

        return updateInfo;
    }

    /**
     * Method used when a user wants to report that he is infected.
     * @param username The username of the infected.
     * @return It returns a list of people that he has been in contact with.
     */
    public List<Username> infectedReport(Username username){

        // Get the infected user
        User infectedUser = this.usersStatus.get(username);

        // Mark the user as infected
        infectedUser.infected();

        // Obtain a list of location where the infected user passed
        Set<Coordinates> infectedUserLocations = infectedUser.locationsWhereUserPassed();
        Set<Username> contactWithInfectedUsers = new HashSet<>();

        // Obtain all the users that had contact with the infected
        for (Coordinates coordinates : infectedUserLocations){
            contactWithInfectedUsers.addAll(this.usersPerCoordinatesUntilNow.get(coordinates));
        }

        // Remove from the list the user that is infected (we don't need to notify him)
        contactWithInfectedUsers.remove(username);

        return new ArrayList<>(contactWithInfectedUsers);
    }

    /**
     * Method that given a location returns the number of users in that current location.
     * @return The number of people in a given location
     */
    public int getNumberOfPeopleInLocation(Coordinates coordinates){
        return this.numberOfPeopleInLocation.get(coordinates);
    }

    /**
     * Get current user coordinates.
     */
    public Coordinates getCurrentUserCoordinates(Username username){
        if (this.usersStatus.containsKey(username))
            return new Coordinates(this.usersStatus.get(username).getCurrentCoordinates());
        else
            return null;
    }

    /**
     * Method that initializes the maps with the coordinates
     * @param N size.
     */
    private void initializeCoordinatesMaps(int N){
        for (int i = 0; i <= N; i++){
            for (int j = 0; j <= N; j++){

                // Generate a new pair of coordinates
                Coordinates coordinates = new Coordinates(i, j);

                usersPerCoordinatesUntilNow.put(coordinates, new HashSet<>());
                numberOfPeopleInLocation.put(coordinates, 0);
            }
        }
    }

    public enum UpdateInfo {

        CONCENTRATION_RAISE(1),     // When updating the user location and because of that update the concentration raises in one location
        CONCENTRATION_DECREASE(2),  // When updating the user location and because of that update the concentration decreases in one location
        NO_PEOPLE(3),               // When user updates to a new location and the previous one stays without people
        NEW_USER(4);                // When it's a new user

        private final int updateType;

        UpdateInfo(int updateType) {
            this.updateType = updateType;
        }

        public int getUpdateType() {
            return updateType;
        }

        private final static Map<Integer, UpdateInfo> map = new HashMap<>();

        static {
            for (UpdateInfo updateType : UpdateInfo.values()) {
                map.put(updateType.getUpdateType(), updateType);
            }
        }

        static UpdateInfo getUpdateType(int type) {
            return map.get(type);
        }
    }
}
