package utils;

import utils.entity.DirectoryServer;
import utils.entity.FrontendServer;

public class ClientConfig {

    public final FrontendServer frontendServer;
    public final  DirectoryServer directoryServer;

    public ClientConfig(FrontendServer frontendServer, DirectoryServer directoryServer) {
        this.frontendServer = frontendServer;
        this.directoryServer = directoryServer;
    }
}
