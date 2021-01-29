package service;

public class Directory {
    private final String directoryUrl;
    private final int directoryPort;
    private final DirectoryService directoryService;

    public Directory(String directoryUrl, int directoryPort) {
        this.directoryUrl = directoryUrl;
        this.directoryPort = directoryPort;
        this.directoryService = new DirectoryService(directoryUrl, directoryPort);
    }

    public String getDirectoryUrl() {
        return directoryUrl;
    }

    public int getDirectoryPort() {
        return directoryPort;
    }

    public DirectoryService getDirectoryService() {
        return directoryService;
    }
}