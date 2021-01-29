package entity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import notifications.Notifications;

import static notifications.Topics.getTopicByContext;

public class AuthenticatedUser {

    // User username, used to subscribe when he has been in contact with an infected
    public final String username;

    // Notifications manager, where we change our interest in notifications
    private final Notifications notifications;

    // Represents subscriptions for each district and location. Actual type is Map<District, Map<Location, [Topic]>>.
    private final Map<String, Map<String, List<String>>> subscriptions;

    public AuthenticatedUser(String username, Notifications notifications, Map<String, Map<String, List<String>>> subscriptions){
        this.username = username;
        this.notifications = notifications;
        this.subscriptions = subscriptions;
    }

    public boolean isNotificationActive(String notification){
        return this.subscriptions.values().stream().anyMatch(m -> m.values().stream().anyMatch(l -> l.contains(notification)));
    }

    private boolean lessThan3Districts(){
        return this.subscriptions.keySet().size() < 3;
    }

    public void reactivateNotifications(Map<String, Map<String, List<String>>> subscriptions){
        subscriptions.forEach((district, map) -> {
            map.forEach((loc, tl) -> tl.forEach(topic -> activateNotification(getTopicByContext(topic), district, loc, false)));
        });
    }

    public void deactivateNotification(Topic topic, String district, String location){
        // Inform the subscriber about the intent to unsub the notification
        this.notifications.unsubscribe((topic.context() + " " + district + " " + location));
        // Update the subscriptions.
        subscriptions.get(district).get(location).remove(topic.context());
    }

    public boolean activateNotification(Topic topic, String district, String location, boolean update){
        // If the notification is only for a newly infected in the district, then we just need the argument district
        this.notifications.subscribe((topic.context() + " " + district + " " + location));

        // Update the subscriptions
        if(update) {
            if (!subscriptions.containsKey(district)) {
                if(lessThan3Districts()) subscriptions.put(district, new HashMap<>());
                else return false;
            }
            if (!subscriptions.get(district).containsKey(location))
                subscriptions.get(district).put(location, new ArrayList<>());
            subscriptions.get(district).get(location).add(topic.context());
        }

        return true;
    }

    /**
     * Method that stops all notifications from being received.
     */
    public void stop(){
        notifications.close();
    }
}



