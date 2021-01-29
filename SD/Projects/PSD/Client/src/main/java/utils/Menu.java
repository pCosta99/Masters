package utils;

import entity.AuthenticatedUser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static notifications.NotificationsStrings.generateActivatedDeactivatedNotificationsMenuOptions;

public class Menu {
    /** This module controls everything the user will see.
     * It abstracts this whole problem from the Writer class. **/

    public enum PHASE{
        AUTH,
        MAIN_MENU,
        NOTIFICATIONS,
        STATISTICS_MENU,
        BLOCKED
    }

    public enum OPERATION {
        ACTIVATE_DEACTIVATE_NOTIFICATIONS,
        NO_PEOPLE_IN_DISTRICT_LOCATION_NOTIFICATION,
        CONCENTRATION_OF_PEOPLE_RAISE_IN_DISTRICT_LOCATION_NOTIFICATION,
        CONCENTRATION_OF_PEOPLE_DECREASE_IN_DISTRICT_LOCATION_NOTIFICATION,
        NEW_INFECTED_IN_DISTRICT,
        BACK_NOTIFICATION_MENU,
        IM_SICK,
        INVALID,
        LOGIN,
        LOGOUT,
        NUMBER_OF_PEOPLE_IN_SPOT,
        QUIT,
        SICK_UNDO,
        SICK_WARNING,
        SIGNUP,
        NOP,
        UPDATE_LOCATION,
        STATISTICS,
        GET_DISTRICT_USERS,
        GET_DISTRICT_INFECTED,
        GET_TOP_RATIO,
        GET_TOP_LOCATION,
        GET_AVG_CONTACTS
    }

    private PHASE phase;
    private final BufferedReader in;
    private OPERATION last_op;
    private AuthenticatedUser authenticatedUser = null;

    public void setAuthenticatedUser(AuthenticatedUser u){
        this.authenticatedUser = u;
    }

    public Menu(){
        this.phase = PHASE.AUTH;
        this.in = new BufferedReader(new InputStreamReader(System.in));
    }

    /**
     * Reader methods for different types information
     */

    private int readOption(String info) throws IOException {
        System.out.println(info);
        int opt = -1;
        try {
            opt = Integer.parseInt(this.in.readLine());
        } catch (NumberFormatException e){
            System.out.println("Invalid value!");
        }
        return opt;
    }

    public String readString(String info) throws IOException {
        System.out.println(info);
        return this.in.readLine();
    }

    public String readUntilValid(String keyword, Collection<String> values) throws IOException {
        displayList(keyword + "s available:", values);
        String value = readString(keyword + ": ");
        while(!values.contains(value)){
            System.out.println("Invalid " + keyword + ", please retype!");
            value = readString(keyword + ": ");
        }
        return value;
    }

    public String readCoordinates(String info, int maxSize) throws IOException {
        System.out.println(info);
        System.out.println("Provide coordinates like: [fst, snd]");
        System.out.println("Your district is " + maxSize + "x" + maxSize + " long.");

        boolean isOk = false;
        String coordinates = "";

        while (!isOk){
            coordinates = this.in.readLine();

            if (coordinates.matches("\\[[0-9]+,[0-9]+]")){
                String[] coords = coordinates.substring(1,coordinates.length()-1).split(",");
                int x = Integer.parseInt(coords[0]);
                int y = Integer.parseInt(coords[1]);
                if(x > maxSize || y > maxSize) {
                    System.out.println("The place you indicated doesn't exist.");
                } else isOk = true;
            }
            else System.out.println("Invalid coordinates. Provide coordinates like: [fst, snd].");
        }
        return coordinates;
    }

    /**
     * Prints a pretty menu along with the options for the very same.
     * @param options Options to print
     * @return The pretty menu ofc.
     */
    private String menuBox(String[] options, boolean numericly){
        StringBuilder sb = new StringBuilder();
        int biggest_option = Arrays.stream(options).sequential().max(Comparator.comparing(String::length)).get().length();

        // 200 is the max console can handle
        int line_length = Math.min(3 * biggest_option, 200);
        String marker = "#".repeat(line_length);
        sb.append(marker).append("\n");

        // This counter is used to automatically prefix the options with their selectable number.
        AtomicInteger counter = new AtomicInteger(1);
        Arrays.stream(options).sequential().forEach(s -> {
            int l_leftover = (line_length - biggest_option) / 2;
            int r_leftover = line_length - (l_leftover + s.length() + 5);
            if(!numericly) r_leftover += 3;
            String lmarker = "#" + " ".repeat(l_leftover);
            String rmarker = " ".repeat(r_leftover) + "#";
            sb.append(lmarker);
            if(numericly) sb.append(counter.get()).append(") ");
            sb.append(s).append(rmarker).append("\n");
            counter.getAndIncrement();
        });
        sb.append(marker).append("\n");
        return sb.toString();
    }

    /**
     * shows the title of the statistics
     */
    public String viewStatsFormat(String title){
        StringBuilder sb = new StringBuilder();
        sb.append("\t\t").append(title).append("\n");
        return sb.toString();
    }

    /**
     * Displays a list
     * @param info The introducting info to be displayed
     * @param list The list to display
     */
    public void displayList(String info, Collection<String> list) {
        System.out.println(info);
        System.out.println(menuBox(list.toArray(String[]::new), false));
    }

    /**
     * Method we want to offer
     * Uniformize the options so we can react to them easily on the implementing classes.
     * Unfortunately, to specify a jump in the menu (ex: on logout go back to AUTH phase) we must add an entry to the method nextMenu
     */

    public OPERATION showMenu() throws IOException {
        OPERATION op = OPERATION.NOP;

        switch(this.phase){
            case AUTH:
                switch (readOption(menuBox(new String[]{"Login","Register","Statistics","Quit"}, true))){
                    case 1: op = OPERATION.LOGIN; break;
                    case 2: op = OPERATION.SIGNUP; break;
                    case 3: op = OPERATION.STATISTICS; break;
                    case 4: op = OPERATION.QUIT; break;
                    default: break;
                }
                break;
            case MAIN_MENU:
                switch (readOption(menuBox(new String[]{"Update location","Get number of people in location","I'm sick","Activate / Deactivate notifications","Logout"}, true))){
                    case 1: op = OPERATION.UPDATE_LOCATION; break;
                    case 2: op = OPERATION.NUMBER_OF_PEOPLE_IN_SPOT; break;
                    case 3: op = OPERATION.IM_SICK; break;
                    case 4: op = OPERATION.ACTIVATE_DEACTIVATE_NOTIFICATIONS; break;
                    case 5: op = OPERATION.LOGOUT; break;
                    default: break;
                }
                break;
            case NOTIFICATIONS:
                List<String> menuOptions = generateActivatedDeactivatedNotificationsMenuOptions(authenticatedUser);
                menuOptions.add("Back to main menu");
                switch (readOption(menuBox(menuOptions.toArray(new String[0]), true))){
                    case 1: op = OPERATION.NO_PEOPLE_IN_DISTRICT_LOCATION_NOTIFICATION; break;
                    case 2: op = OPERATION.CONCENTRATION_OF_PEOPLE_RAISE_IN_DISTRICT_LOCATION_NOTIFICATION; break;
                    case 3: op = OPERATION.CONCENTRATION_OF_PEOPLE_DECREASE_IN_DISTRICT_LOCATION_NOTIFICATION; break;
                    case 4: op = OPERATION.NEW_INFECTED_IN_DISTRICT; break;
                    case 5: op = OPERATION.BACK_NOTIFICATION_MENU; break;
                    default: break;
                }
                break;
            case STATISTICS_MENU:
                switch (readOption(menuBox(new String[]{"Get number of users from a given district","Get number of infected people from a given district","Top 5 of districts with highest ratio of infected / users","Top 5 of the locations that had the most people simultaneously","Average number of users that contacted with sick users"}, true))){
                    case 1: op = OPERATION.GET_DISTRICT_USERS; break;
                    case 2: op = OPERATION.GET_DISTRICT_INFECTED; break;
                    case 3: op = OPERATION.GET_TOP_RATIO; break;
                    case 4: op = OPERATION.GET_TOP_LOCATION; break;
                    case 5: op = OPERATION.GET_AVG_CONTACTS; break;
                    default: break;
                }
                break;
            case BLOCKED:
                switch (readOption(menuBox(new String[]{"I'm still sick!", "I'm great now!", "Logout"}, true))){
                    case 1: op = OPERATION.SICK_WARNING; break;
                    case 2: op = OPERATION.SICK_UNDO; break;
                    case 3: op = OPERATION.LOGOUT; break;
                    default: break;
                }
                break;
            default: System.out.println("Something went wrong."); break;
        }

        last_op = op;

        return op;
    }

    public void overrideMenu(PHASE phase){
        this.phase = phase;
    }

    public void nextMenu(){
        switch(this.phase){
            case AUTH:
                if (last_op == OPERATION.LOGIN) this.phase = PHASE.MAIN_MENU;
                else if(last_op == OPERATION.STATISTICS) this.phase = PHASE.STATISTICS_MENU;
                break;
            case MAIN_MENU:
                switch (last_op){
                    case LOGOUT: this.phase = PHASE.AUTH; break;
                    case IM_SICK: this.phase = PHASE.BLOCKED; break;
                    case ACTIVATE_DEACTIVATE_NOTIFICATIONS: this.phase = PHASE.NOTIFICATIONS; break;
                    default: break;
                }
                break;
            case NOTIFICATIONS:
                if (last_op == OPERATION.BACK_NOTIFICATION_MENU) {
                    this.phase = PHASE.MAIN_MENU;
                }
                break;
            case STATISTICS_MENU:
                if (last_op == OPERATION.GET_DISTRICT_USERS) {
                    this.phase = PHASE.AUTH;
                }
                break;
            case BLOCKED:
                if (last_op == OPERATION.SICK_UNDO) this.phase = PHASE.MAIN_MENU;
                else if (last_op == OPERATION.LOGOUT) this.phase = PHASE.AUTH;
                break;
        }
    }
}
