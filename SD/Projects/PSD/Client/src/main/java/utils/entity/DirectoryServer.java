package utils.entity;

public class DirectoryServer {
    public final String NAME;
    public final String URL;
    public final String HEALTH_URL;

    public DirectoryServer(String name, String url, String health_url) {
        this.NAME = name;
        this.URL = url;
        this.HEALTH_URL = health_url;
    }
}
