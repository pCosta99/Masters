import com.google.gson.Gson;
import entity.AuthenticatedUser;
import entity.Topic;
import notifications.Notifications;
import notifications.NotificationsStrings;
import notifications.Topics;
import org.zeromq.ZMQ;
import protocol.*;
import services.HttpService;
import utils.*;

import java.io.*;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import static protocol.Protocol.*;
import static protocol.Protocol.LocationOperationType.*;
import static protocol.Protocol.OperationType.*;

public class RequestsHandler implements Runnable {

    private final Socket socket;
    private final ClientConfig config;
    private AuthenticatedUser authenticatedUser = null;
    private final ZMQ.Context context; // ZMQ Context
    private final AtomicBoolean running = new AtomicBoolean(false);
    private final OutputStream out;
    private final InputStream in;
    private final Menu menu;
    private final HttpService httpService;
    private Map<String,Pair<Integer,Integer>> districts; // Map<Name, Pair<ID,Size>>
    private Map<Integer, Pair<String,Integer>> distByID;
    private int current_district;
    private Map<String, Map<String, List<String>>> dist_loc_topic;

    public RequestsHandler(Socket socket, ZMQ.Context context, ClientConfig cfg) throws IOException {
        this.socket = socket;
        this.context = context;
        this.config = cfg;
        this.out = socket.getOutputStream();
        this.in = socket.getInputStream();
        this.menu = new Menu();
        this.httpService = new HttpService(cfg);
    }

    // Stop thread safely since .stop is deprecated
    public void stop() {
        running.set(false);
    }

    public void run() {
        running.set(true);

        EntryMessage em = MessageReader.readEntryMessage(in);
        districts = em.getDistrictsMap().entrySet().stream().collect(Collectors.toMap(e -> e.getValue().getName(), e -> new Pair<>(e.getKey(), e.getValue().getSize())));
        distByID = em.getDistrictsMap().entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> new Pair<>(e.getValue().getName(), e.getValue().getSize())));

        Menu.OPERATION option = Menu.OPERATION.NOP;

        while(option != Menu.OPERATION.INVALID && running.get()){
            try {
                option = menu.showMenu();
                ScreenCleaner.cls();
                switch (option){
                    case LOGIN: login(); break;
                    case SIGNUP: createAccount(); break;
                    case STATISTICS: this.menu.nextMenu(); break;
                    case QUIT: option = Menu.OPERATION.INVALID; quit(); break;
                    case UPDATE_LOCATION: updateLocation(); break;
                    case NUMBER_OF_PEOPLE_IN_SPOT: getNumberOfPeopleInSpot(); break;
                    case IM_SICK: sick(true); break;
                    case ACTIVATE_DEACTIVATE_NOTIFICATIONS: this.menu.nextMenu(); break;

                    // Public Notifications
                    case NO_PEOPLE_IN_DISTRICT_LOCATION_NOTIFICATION: publicNotifications(Topics.NO_PEOPLE_DISTRICT); break;
                    case CONCENTRATION_OF_PEOPLE_RAISE_IN_DISTRICT_LOCATION_NOTIFICATION: publicNotifications(Topics.CONCENTRATION_OF_PEOPLE_RAISE); break;
                    case CONCENTRATION_OF_PEOPLE_DECREASE_IN_DISTRICT_LOCATION_NOTIFICATION: publicNotifications(Topics.CONCENTRATION_OF_PEOPLE_DECREASE); break;
                    case NEW_INFECTED_IN_DISTRICT: publicNotifications(Topics.NEW_INFECTED_DISTRICT); break;
                    case BACK_NOTIFICATION_MENU: this.menu.nextMenu(); break;

                    // Statistics
                    case GET_DISTRICT_USERS: getDistrictUsers(); break;  // numero de users de um dado distrito
                    case GET_DISTRICT_INFECTED: getDistricInfected(); break;
                    case GET_TOP_RATIO: getTopRatio(); break;
                    case GET_TOP_LOCATION: getTopLocation(); break;
                    case GET_AVG_CONTACTS: getAvgContacts(); break;

                    case LOGOUT: logout(); break;
                    case SICK_WARNING: System.out.println("Stay home then!"); break;
                    case SICK_UNDO: sick(false); break;
                    default: System.out.println("Invalid option!"); break;
                }
            } catch (IOException e){
                e.printStackTrace();
            }
        }
    }

    /**
     * Returns the typed username, so if the login is successful then it can subscribe for
     * private notifications when the user has been in contact with an infected.
     */
    private String auth_commons(AuthType type) throws IOException {
        String user = menu.readString("Username: ");
        String pass = menu.readString("Password: ");
        String district;
        int districtId = 0;

        // Read the district if it's a signup
        if(type == AuthType.SIGNUP) {
            district = menu.readUntilValid("District", districts.keySet());
            districtId = districts.get(district).getFst();
            current_district = districtId;
        }

        // Create the message and transform it into binary information
        User u = User.newBuilder().setUsername(user).setPassword(pass).setDistrictId(districtId).build();
        AuthMessage m = AuthMessage.newBuilder().setUser(u).setType(type).build();
        byte[] request = m.toByteArray();

        // Send the request to the frontend server
        this.out.write(request);

        return user;
    }

    private void login() throws IOException {
        String username = auth_commons(AuthType.LOGIN);

        // Read the reply and return it
        LoginReply reply = MessageReader.readLoginReply(in);

        // If it was successful then we are logged in and we can proceed.
        // Otherwise react to the info provided.
        if(reply.getSuccess()){
            // Update the district of the logged user
            current_district = reply.getDistrict();

            // Prepare to re-sub everything that the user was following
            dist_loc_topic = new HashMap<>();
            reply.getSubbed().getSubsMap().forEach((district, map) -> {
                dist_loc_topic.put(district, new HashMap<>());
                map.getLocmapMap().forEach((loc, tl) -> dist_loc_topic.get(district).put(loc, tl.getTopicsList()));
            });

            // Create a authenticated user, his publisher and subscriber sockets and start receiving private notifications
            initializeAuthenticatedUser(username, dist_loc_topic);

            // Update the user location
            location_common(UPDATE, "Where are you at?");

            System.out.println("Login succeeded, proceeding to main menu!");

            if(!reply.getSick()) this.menu.nextMenu();
            else this.menu.overrideMenu(Menu.PHASE.BLOCKED);
        } else {
            switch(reply.getError()){
                case INVALID_USERNAME: System.out.println("Username doesn't exist!"); break;
                case INVALID_PASSWORD: System.out.println("Invalid password! Please try again."); break;
                case USER_ALREADY_LOGGED: System.out.println("This account is already logged in. \nMultiple connections are not allowed."); break;
                default: System.out.println("Unrecognized error!"); break;
            }
        }
    }

    private void createAccount() throws IOException {
        auth_commons(AuthType.SIGNUP);

        // Read the reply and return it
        SignupReply reply = MessageReader.readSignupReply(in);

        // If it was successful then we are signed up and must login.
        // Otherwise react to the info provided.
        if(reply.getSuccess()){
            System.out.println("Signup succeeded, please login now!");
            this.menu.nextMenu();
        } else {
            switch (reply.getError()){
                case USER_EXISTS: System.out.println("The username provided already exists!"); break;
                default: System.out.println("Unrecognized error!"); break;
            }
        }
    }

    private void menu_send_op(OperationType opType) throws IOException {
        // Sends an operation of the specified type
        Operation op = Operation.newBuilder().setUsername(authenticatedUser.username).setType(opType).build();
        this.out.write(op.toByteArray());
        this.menu.nextMenu();
    }

    private void logout() throws IOException{
        menu_send_op(LOGOUT);

        // Clear all the logged in user info
        this.authenticatedUser.stop();
        this.authenticatedUser = null;
        this.menu.setAuthenticatedUser(null);
        System.out.println("See ya later!");
    }

    private void location_common(LocationOperationType type, String query) throws IOException{
        int max_size = distByID.get(current_district).getSnd();
        String location = menu.readCoordinates(query,max_size);

        // Send the location somewhere to be used
        LocationOperation lop = LocationOperation.newBuilder().setType(type).setLocation(location).build();
        Operation op = Operation.newBuilder().setType(LOCATION).setUsername(authenticatedUser.username).setLop(lop).build();
        this.out.write(op.toByteArray());
    }

    private void updateLocation() throws IOException {
        location_common(UPDATE, "Where you at?");
        System.out.println("Updated your location successfully!");
    }

    private void getNumberOfPeopleInSpot() throws IOException {
        location_common(GET, "What location are you curious about?");

        // React to server reply
        OperationReply reply = MessageReader.readOperationReply(in);

        System.out.println("There are " + reply.getLogr().getNumber() + " persons in that location!");
    }

    private void sick(boolean isSick) throws IOException{
        if(isSick) System.out.println("Stay home then!");
        else System.out.println("Great! Welcome back!");

        menu_send_op(FLIP_SICK);
    }

    /******************************************************************
                        PUBLIC NOTIFICATIONS
     ******************************************************************/

    private void publicNotificationsSideEffects(NotificationOperationType type, String district, String location, Topic topic) throws IOException {
        // Inform frontend server of the change to the subscriptions
        Protocol.NotificationOperation notificationOP = NotificationOperation.newBuilder().setDistrict(district).setTopic(topic.context()).setType(type).setLocation(location).build();
        Protocol.Operation op = Operation.newBuilder().setNop(notificationOP).setType(NOTIFICATION).build();
        this.out.write(op.toByteArray());
    }

    private void publicNotifications(Topic topic) throws IOException {
        String type = "sub";
        // If there are already some subscriptions ask if it's a sub or an unsub
        if(authenticatedUser.isNotificationActive(topic.context()))
            type = menu.readUntilValid("Type", Arrays.asList("sub", "unsub"));
        // Proceed accordingly to the type of operation we are doing
        if(type.equals("sub")){
            // Ask for the district and location (if needed)
            String district = menu.readUntilValid("District", districts.keySet());
            String location = "all";
            if(topic.id() != 3) location = menu.readCoordinates("What's the location of your interest?", districts.get(district).getSnd());

            // Inform the publish about the intent to sub the notification
            if(!authenticatedUser.activateNotification(topic, district, location, true)){
                System.out.println("You are already subscribing 3 districts and that's the limit!");
            } else System.out.println(NotificationsStrings.createAnswer(topic, district, location, "sub"));
            publicNotificationsSideEffects(NotificationOperationType.SUB, district, location, topic);
        } else {
            // Ask for the district and location (if needed) displaying only the available possibilities
            Map<String, Set<String>> valid_districts_locs = dist_loc_topic.entrySet().stream().map(e -> {
                Map<String, List<String>> loc_topics = e.getValue().entrySet().stream().filter(e1 -> e1.getValue().contains(topic.context())).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
                return Map.entry(e.getKey(), loc_topics);
            }).collect(Collectors.toMap(Map.Entry::getKey, e3 -> e3.getValue().keySet()));
            valid_districts_locs = valid_districts_locs.entrySet().stream().filter(e -> e.getValue().size() > 0).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

            String district = menu.readUntilValid("District", valid_districts_locs.keySet());
            String location = "all";
            if(topic.id() != 3) location = menu.readUntilValid("Location", valid_districts_locs.get(district));

            // Inform the publish about the intent to unsub the notification
            authenticatedUser.deactivateNotification(topic, district, location);
            System.out.println(NotificationsStrings.createAnswer(topic, district, location, "pub"));
            publicNotificationsSideEffects(NotificationOperationType.UNSUB, district, location, topic);
        }
    }

    private void quit() throws IOException {
        // Close everything and leave
        this.in.close();
        this.out.close();
        this.socket.close();

        System.out.println("Bye bye!");

        // Stop this thread
        stop();
    }

    /**
     * Method that given a authenticated username, invokes notifications and
     * subscribes on the topics that the local thread will publish on when the interest
     * in the public notifications changes.
     * @param username The authenticated user username.
     * @param dist_loc_topic
     */
    private void initializeAuthenticatedUser(String username, Map<String, Map<String, List<String>>> dist_loc_topic) {

        Notifications notifications = new Notifications(this.context, config.frontendServer.publisher_broker_port, config.frontendServer.private_notifications_publisher);

        // Defining authenticated user to receive notifications when he has been in contact with an infected
        notifications.subscribe((Topics.CONTACT_WITH_INFECTED.context() + " " + username));

        // Create a authenticated user with the ZeroMQ Sockets
        authenticatedUser = new AuthenticatedUser(username, notifications, dist_loc_topic);

        // Populate him with the previous subscriptions
        authenticatedUser.reactivateNotifications(dist_loc_topic);

        // Set the authenticated user in the menu
        menu.setAuthenticatedUser(authenticatedUser);

        // Start receiving notifications
        notifications.start();
    }


    /******************************************************************
                        STATISTICS
     ******************************************************************/

    public void goBackOrInitial() throws IOException {
        String op = this.menu.readString("\n Go back: insert 1 \t\t Initial menu: insert 2");
        if(op.equals("2")){
            this.menu.overrideMenu(Menu.PHASE.AUTH);
        }
        else if(op.equals("1")){
            this.menu.overrideMenu(Menu.PHASE.STATISTICS_MENU);
        }
        ScreenCleaner.cls();
    }

    /***
     * Method that given the number of users from a given district
     */

    private void getDistrictUsers() throws IOException {
        String district = menu.readString("District: ");
        ScreenCleaner.cls();

        System.out.println("\tSTATISTICS\n");

        String title = this.menu.viewStatsFormat("1. Get number of users from a given district: " + district.toUpperCase());
        System.out.println(title);

        String reply = this.httpService.getRequest(district+"/Users");

        System.out.println(this.menu.viewStatsFormat("Number of users: " + reply));

        this.goBackOrInitial();
    }


    /***
     * Method that gives the number of infected in a given district.
     */
    private void getDistricInfected() throws IOException {
        String district = menu.readString("District: ");
        ScreenCleaner.cls();

        System.out.println("\tSTATISTICS\n");

        String title = this.menu.viewStatsFormat("2. Get number of infected people from a given district: " + district.toUpperCase());
        System.out.println(title);

        String reply = this.httpService.getRequest(district+"/Infecteds");

        System.out.println(this.menu.viewStatsFormat("Number of users: " + reply));

        this.goBackOrInitial();
    }

    public void separate(String s, String delim){
        StringTokenizer st = new StringTokenizer(s, delim);
        while (st.hasMoreTokens()) {
            System.out.println("\t\t\t"+st.nextToken());
        }
    }

    /**
     * Method that gives Top 5 of districts with highest ratio of infected / users
     * @throws IOException
     */
    private void getTopRatio() throws IOException {
        ScreenCleaner.cls();

        System.out.println("\tSTATISTICS\n");

        String title = this.menu.viewStatsFormat("3. Top 5 of districts with highest ratio of infected / users: ");
        System.out.println(title);

        String reply = this.httpService.getRequest("TopRatio");

        reply = reply.substring(2, reply.length() - 2);

        separate(reply, "\",\"");

        this.goBackOrInitial();
    }


    /**
     * Method that gives op 5 of the locations that had the most people simultaneously
     * @throws IOException
     */
    private void getTopLocation() throws IOException {
        ScreenCleaner.cls();

        System.out.println("\tSTATISTICS\n");

        String title = this.menu.viewStatsFormat("4. Top 5 of the locations that had the most people simultaneously");
        System.out.println(title);

        String reply = this.httpService.getRequest("Location/MostUsers");

        TopLocation[] response = new Gson().fromJson(reply,TopLocation[].class);

        System.out.println("\t\t\tLOCATION\tDISTRICT\tUSERS\tMAXUSERS");

        for (TopLocation s : response) {
            System.out.println("\t\t\t"+s.toString());
        }

        this.goBackOrInitial();
    }

    /**
     * Method that gives Average number of users that contacted with sick users.
     * @throws IOException
     */
    private void getAvgContacts() throws IOException {
        ScreenCleaner.cls();

        System.out.println("\tSTATISTICS\n");

        String title = this.menu.viewStatsFormat("5. Average number of users that contacted with sick users");
        System.out.println(title);

        String reply = this.httpService.getRequest("AvgInfectedContacts");

        System.out.println(this.menu.viewStatsFormat("Average number: " + reply));


        this.goBackOrInitial();
    }

}
