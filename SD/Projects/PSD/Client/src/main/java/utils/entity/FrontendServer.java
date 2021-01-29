package utils.entity;

public class FrontendServer {
    public final String frontend_address;
    public final Integer frontend_port;
    public final Integer publisher_broker_port;
    public final Integer private_notifications_publisher;

    public FrontendServer(String frontend_address, Integer frontend_port, Integer publisher_broker_port, Integer frontend_publisher) {
        this.frontend_address = frontend_address;
        this.frontend_port = frontend_port;
        this.publisher_broker_port = publisher_broker_port;
        this.private_notifications_publisher = frontend_publisher;
    }
}
