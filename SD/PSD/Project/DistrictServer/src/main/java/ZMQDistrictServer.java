import com.google.protobuf.InvalidProtocolBufferException;
import entity.Username;
import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import protocol.Protocol;
import service.Directory;
import util.Coordinates;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class ZMQDistrictServer {

    // District
    private final District district;

    // Directory
    private final Directory directory;

    // Zero MQ Socket Context
    private final ZMQ.Context context;

    // Socket used to receive requests from the frontend server
    private final ZMQ.Socket pullSocket;

    // Socket used to send messages to the frontend server
    private final ZMQ.Socket pushSocket;

    // Socket used to publish notifications to active users
    private final ZMQ.Socket publisher;

    // Default binding address
    private final String BINDING_ADDRESS = "tcp://localhost:";

    public ZMQDistrictServer(DistrictConfig districtConfig){

        this.district = districtConfig.getDistrict();

        this.directory = districtConfig.getDirectory();

        this.context = ZMQ.context(1);

        ZMQ.Socket pull  = context.socket(SocketType.PULL);
        ZMQ.Socket push  = context.socket(SocketType.PUSH);
        ZMQ.Socket pub   = context.socket(SocketType.PUB);

        this.pullSocket = pull;
        this.pushSocket = push;
        this.publisher = pub;

        this.pullSocket.bind(BINDING_ADDRESS + districtConfig.getPullPort());
        this.pushSocket.connect(BINDING_ADDRESS + districtConfig.getPushPort());
        this.publisher.connect(BINDING_ADDRESS + districtConfig.getBrokerPort());

        System.out.println( "> District Server Pulling on Port " + districtConfig.getPullPort() +
                ", Pushing on Port " + districtConfig.getPushPort() + " and " + "Publishing on Port " +
                districtConfig.getBrokerPort() );
    }

    /**
     * Receive frontend requests.
     */
    public void run(){

        while (true) {
            try {
                // Receive messages from the frontend
                byte[] bytes = this.pullSocket.recv();

                Protocol.Operation message = Protocol.Operation.parseFrom(bytes);

                System.out.println("> Received message with type: " + message.getType().toString());
                if (message.getType() == Protocol.OperationType.LOCATION){
                    System.out.println("\t-> " + message.getLop().getType().toString());
                }

                if (!message.hasUsername()){
                    handleError();
                    System.err.println("> Username required!");
                } else {
                    switch (message.getType()){
                        case LOCATION:
                            handleLocationMessage(new Username(message.getUsername()), message.getLop());
                            break;
                        case SICK:
                            handleSickMessage(new Username(message.getUsername()));
                            break;
                        default:
                            handleError();
                            break;
                    }
                }
            } catch (InvalidProtocolBufferException e) {
                System.err.println("> Error while parsing message!");
                handleError();
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
                System.err.println("> Error while processing message!");
                handleError();
            }
        }
    }

    /**
     * Handle a location update or location request.
     * @param username The username of the user who sent a location message.
     * @param locationMessage The location message.
     */
    private void handleLocationMessage(Username username, Protocol.LocationOperation locationMessage) throws IOException {
        String location = locationMessage.getLocation();
        Coordinates coordinates = Coordinates.parseFrom(location);

        switch (locationMessage.getType()){
            case UPDATE:
                Coordinates currentCoordinates = this.district.getDistrictStore().getCurrentUserCoordinates(username);
                List<DistrictStore.UpdateInfo> updates = this.district.getDistrictStore().updateUserCoordinates(username, coordinates);
                processUpdates(updates, currentCoordinates, coordinates);
                System.out.println("> LOCATION UPDATE Successfully!");
                break;
            case GET:
                int number = this.district.getDistrictStore().getNumberOfPeopleInLocation(coordinates);
                System.out.println("> The number of people at the requested location is: " + number);
                this.pushSocket.send(
                        Protocol.OperationReply.newBuilder()
                                .setUsername(username.getUser())
                                .setType(Protocol.OperationReplyType.GET_LOCATION_REPLY)
                                .setLogr(Protocol.LocationOperationGetReply.newBuilder().setNumber(number).build())
                                .build().toByteArray()
                );
                System.out.println("> LOCATION GET, Reply Sent!");
                break;
        }
    }

    /**
     * Handle a sick message, send notifications to all possibly infected users.
     * @param username The infected user.
     */
    private void handleSickMessage(Username username){
        // Obtain the list of contacts that this user made before
        List<Username> infectedContacts = district.getDistrictStore().infectedReport(username);

        // Inform the directory server of a new infected and pass it the number of contacts
        // that the user made before getting infected
        try {
            this.directory.getDirectoryService().newInfected(district.getDistrictName(), infectedContacts.size());
        } catch (IOException e) {
            handleError();
            e.printStackTrace();
        }

        // Iterable needed to pass to frontend
        List<String> users = new ArrayList<>();
        infectedContacts.forEach(u -> users.add(u.getUser()));

        // Pass the list of all the contacts to the frontend server
        this.pushSocket.send(Protocol.OperationReply.newBuilder()
                .setType(Protocol.OperationReplyType.INFECTED_CONTACTS)
                .setUsername(username.getUser())
                .setContacts(Protocol.InfectedContacts.newBuilder().addAllUsernames(users).build())
                .build()
                .toByteArray());

        // Publish a message to inform that there is a new infected in this district
        this.publisher.send(Config.NEW_INFECTED_DISTRICT_TOPIC + " " + district.getDistrictName());

        System.out.println("> Notified all possibly infected contacts!");
    }

    /**
     * Method that receives a list of updates and handles the info provided by them
     * to send notifications for users.
     * @param updates The updates list.
     * @param oldCoordinates Old location of user.
     * @param newCoordinates New location of user.
     */
    private void processUpdates(List<DistrictStore.UpdateInfo> updates, Coordinates oldCoordinates, Coordinates newCoordinates) throws IOException {

        if (updates.contains(DistrictStore.UpdateInfo.NEW_USER)){ // POST to directory service
            this.directory.getDirectoryService().newUser(district.getDistrictName(), newCoordinates.getFirstCoordinate(), newCoordinates.getSecondCoordinate());
        } else if (updates.contains(DistrictStore.UpdateInfo.CONCENTRATION_RAISE)){
            System.out.println("> Sending CONCENTRATION OF PEOPLE RAISE Notification!");
            this.publisher.send(Config.CONCENTRATION_OF_PEOPLE_RAISE_TOPIC + " " + district.getDistrictName() + " [" +
                    newCoordinates.getFirstCoordinate() + "," + newCoordinates.getSecondCoordinate() + "]");
            this.directory.getDirectoryService().updateUserLocation(district.getDistrictName(), oldCoordinates.getFirstCoordinate(), oldCoordinates.getSecondCoordinate(),
                    newCoordinates.getFirstCoordinate(), newCoordinates.getSecondCoordinate());
        } else if (updates.contains(DistrictStore.UpdateInfo.CONCENTRATION_DECREASE)){
            System.out.println("> Sending CONCENTRATION OF PEOPLE DECREASE Notification!");
            this.publisher.send(Config.CONCENTRATION_OF_PEOPLE_DECREASE_TOPIC + " " + district.getDistrictName() + " [" +
                    oldCoordinates.getFirstCoordinate() + "," + oldCoordinates.getSecondCoordinate() + "]");
            this.directory.getDirectoryService().updateUserLocation(district.getDistrictName(), oldCoordinates.getFirstCoordinate(), oldCoordinates.getSecondCoordinate(),
                    newCoordinates.getFirstCoordinate(), newCoordinates.getSecondCoordinate());
        } else if (updates.contains(DistrictStore.UpdateInfo.NO_PEOPLE)){
            System.out.println("> Sending NO PEOPLE Notification!");
            this.publisher.send(Config.NO_PEOPLE_DISTRICT_TOPIC + " " + district.getDistrictName() + " [" +
                    oldCoordinates.getFirstCoordinate() + "," + oldCoordinates.getSecondCoordinate() + "]");
            this.directory.getDirectoryService().updateUserLocation(district.getDistrictName(), oldCoordinates.getFirstCoordinate(), oldCoordinates.getSecondCoordinate(),
                    newCoordinates.getFirstCoordinate(), newCoordinates.getSecondCoordinate());
        }
    }

    /**
     * Handle a error during receiving a message from the frontend and send the error
     * to frontend.
     */
    private void handleError(){
        this.pushSocket.send(
                Protocol.OperationReply.newBuilder()
                        .setErrors(
                                Protocol.OperationErrors.ERROR_DECODING
                        )
                        .build()
                        .toByteArray());
    }
}
