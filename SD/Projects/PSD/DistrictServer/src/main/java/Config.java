public class Config {

    // This value represents that when the number of people in a district location
    // is superior to Round(0.05 * DistrictSize) then there is a concentration raise
    // the opposite represents that there is a concentration decrease
    public static final double CONCENTRATION_PERCENTAGE = 0.05;

    /******************************************************************
     * PRIVATE NOTIFICATIONS
     ******************************************************************/

    // When the user has been in contact with a infected
    public static final String CONTACT_WITH_INFECTED_TOPIC = "infected-c";

    /******************************************************************
     * PUBLIC NOTIFICATIONS
     ******************************************************************/

    // When there is no people in a given location of a district
    public static final String NO_PEOPLE_DISTRICT_TOPIC = "no-ppl";

    // When the concentration of people raise in a given location of a district
    public static final String CONCENTRATION_OF_PEOPLE_RAISE_TOPIC = "cop-r";

    // When the concentration of people decrease in a given location of a district
    public static final String CONCENTRATION_OF_PEOPLE_DECREASE_TOPIC = "cop-d";

    // When there is a new infected in a district
    public static final String NEW_INFECTED_DISTRICT_TOPIC = "new-infected";
}
