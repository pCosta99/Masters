package notifications;

import entity.AuthenticatedUser;
import entity.Topic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class NotificationsStrings {

    public static final String[] notificationsOptions = new String[]
            {
                    "When no one is at a given location of a district",
                    "When the concentration of people raises at a given location of a district",
                    "When the concentration of people decreases at a given location of a district",
                    "When there is a new infected in a district"
            };

    private static final String[] notificationsAnswers = new String[]
            {
                    "No one",
                    "Raise of people concentration",
                    "Decrease of people concentration",
                    "New infected"
            };

    public static String createAnswer(Topic topic, String district, String location, String type){
        String s;
        if(type.equals("sub")){
            if(topic.id() != 3) s = "Activated with success the notification: " + notificationsAnswers[topic.id()] + " in district " + district + " and location " + location + "!";
            else s = "Activated with success the notification: " + notificationsAnswers[topic.id()] + " in district " + district + "!";
        } else {
            if(topic.id() != 3) s = "Deactivated with success the notification: " + notificationsAnswers[topic.id()] + " for district " + district + " and location " + location + "!";
            else s = "Deactivated with success the notification: " + notificationsAnswers[topic.id()] + " for district " + district + "!";
        }
        return s;
    }

    public static final int numberOfNotificationsOptions = notificationsOptions.length;

    public static List<String> generateActivatedDeactivatedNotificationsMenuOptions(AuthenticatedUser authenticatedUser){

        assert authenticatedUser != null;

        String[] options = NotificationsStrings.notificationsOptions.clone();

        for (int i = 0; i < NotificationsStrings.numberOfNotificationsOptions; i++){
            if (authenticatedUser.isNotificationActive(Topics.getTopicById(i).context())){
                options[i] += " [✓]";
            }else{
                options[i] += " [✘]";
            }
        }

        return new ArrayList<>(Arrays.asList(options));
    }
}
