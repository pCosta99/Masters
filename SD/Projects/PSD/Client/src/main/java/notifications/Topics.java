package notifications;

import entity.Topic;

public class Topics {

    /**
     * Thread comunication (inproc) syntax, to subscribe or unsubscribe for a public notification
     */
    public static final String SUBSCRIBE = "sub ";
    public static final String UNSUBSCRIBE = "unsub ";

    /******************************************************************
                            PRIVATE NOTIFICATIONS
     ******************************************************************/

    // When the user has been in contact with a infected
    public static final Topic CONTACT_WITH_INFECTED = new Topic(-1, "infected-c");


    /******************************************************************
                            PUBLIC NOTIFICATIONS
     ******************************************************************/

    // When there is no people in a given location of a district
    public static final Topic NO_PEOPLE_DISTRICT = new Topic(0, "no-ppl");

    // When the concentration of people raise in a given location of a district
    public static final Topic CONCENTRATION_OF_PEOPLE_RAISE = new Topic(1, "cop-r");

    // When the concentration of people decrease in a given location of a district
    public static final Topic CONCENTRATION_OF_PEOPLE_DECREASE = new Topic(2, "cop-d");

    // When there is a new infected in a district
    public static final Topic NEW_INFECTED_DISTRICT = new Topic(3, "new-infected");

    public static Topic getTopicByContext(String context){
        switch (context) {
            case "no-ppl":
                return NO_PEOPLE_DISTRICT;
            case "cop-r":
                return CONCENTRATION_OF_PEOPLE_RAISE;
            case "cop-d":
                return CONCENTRATION_OF_PEOPLE_DECREASE;
            default:
                return NEW_INFECTED_DISTRICT;
        }
    }

    public static Topic getTopicById(int id){
        switch(id){
            case 0: return NO_PEOPLE_DISTRICT;
            case 1: return CONCENTRATION_OF_PEOPLE_RAISE;
            case 2: return CONCENTRATION_OF_PEOPLE_DECREASE;
            default: return NEW_INFECTED_DISTRICT;
        }
    }
}
