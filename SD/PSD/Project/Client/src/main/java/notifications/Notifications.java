package notifications;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;

public class Notifications {

    // Context
    private final ZMQ.Context context;

    // Sub socket that receives public notifications from the district server and private notifications from frontend server
    private final ZMQ.Socket notificationsSocket;

    // Receives commands from the client (like close, subscribe, unsubscribe, etc...)
    private final ZMQ.Socket commandsSocketReader;

    // Receives commands from the client (like close, subscribe, unsubscribe, etc...)
    private final ZMQ.Socket commandsSocketWriter;

    public Notifications (ZMQ.Context context, int brokerPort, int privateNotifications) {
        this.context = context;
        this.notificationsSocket = context.socket(SocketType.SUB);
        this.commandsSocketReader = context.socket(SocketType.PAIR);
        this.commandsSocketWriter = context.socket( SocketType.PAIR);

        this.notificationsSocket.connect("tcp://localhost:" + brokerPort);
        this.notificationsSocket.connect("tcp://localhost:" + privateNotifications);

        this.commandsSocketReader.bind("inproc://notifications");
        this.commandsSocketWriter.connect( "inproc://notifications");
    }

    public void subscribe (String topic) {
        this.commandsSocketWriter.send(Topics.SUBSCRIBE + (topic.contains(" all") ? topic.replace(" all", "") : topic));
    }

    public void unsubscribe (String topic) {
        this.commandsSocketWriter.send(Topics.UNSUBSCRIBE + topic );
    }

    private boolean onCommand (String command) {
        if (command.equals("close")) {
            return false;
        }

        if (command.startsWith(Topics.SUBSCRIBE)) {
            this.notificationsSocket.subscribe(command.substring(Topics.SUBSCRIBE.length()));
        } else if (command.startsWith(Topics.UNSUBSCRIBE)) {
            this.notificationsSocket.unsubscribe(command.substring(Topics.UNSUBSCRIBE.length()));
        } else {
            System.err.println("Unknown command: " + command);
        }

        return true;
    }

    private void eventLoop () {
        ZMQ.Poller poller = this.context.poller(2);

        poller.register(this.notificationsSocket);
        poller.register(this.commandsSocketReader);

        boolean running = true;

        while (running) {
            poller.poll();

            if (poller.pollin(0)) {
                byte[] bytes = this.notificationsSocket.recv();

                if (bytes != null) {
                    handleNotification(new String(bytes));
                }
            }


            if (poller.pollin(1)) {
                byte[] bytes = this.commandsSocketReader.recv();

                if (bytes != null) {
                    running = this.onCommand(new String(bytes));
                }
            }
        }

        poller.close();
    }

    private void handleNotification(String notification){
        System.out.println("---------------------------------------------------------------");
        System.out.println("---> NOTIFICATION: ");

        String[] split = notification.split(" ");

        if (notification.contains(Topics.CONTACT_WITH_INFECTED.context())){
            System.out.println("You have been in contact with an infected!");
        } else if (notification.contains(Topics.NO_PEOPLE_DISTRICT.context())){
            System.out.println("There is no people at the district " + split[1] + " at location: " + split[2] + "!");
        } else if (notification.contains(Topics.CONCENTRATION_OF_PEOPLE_RAISE.context())){
            System.out.println("The concentration of people increased in the district " + split[1] + " at location: " + split[2] + "!");
        } else if (notification.contains(Topics.CONCENTRATION_OF_PEOPLE_DECREASE.context())){
            System.out.println("The concentration of people decreased in the district " + split[1] + " at location: " + split[2] + "!");
        } else if (notification.contains(Topics.NEW_INFECTED_DISTRICT.context())){
            System.out.println("There is a new infected at the district " + split[1] + "!");
        }

        System.out.println("---------------------------------------------------------------");
    }

    public void start () {
        Thread notificationsThread = new Thread(this::eventLoop);

        notificationsThread.start();
    }

    public void close () {
        this.commandsSocketWriter.send("close");
        this.notificationsSocket.close();
        this.commandsSocketReader.close();
        this.commandsSocketWriter.close();
    }
}