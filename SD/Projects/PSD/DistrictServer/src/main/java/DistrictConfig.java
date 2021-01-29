import service.Directory;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.util.*;

public class DistrictConfig {

    private final District district;
    private final Directory directory;
    private final int pushPort;
    private final int pullPort;
    private final int brokerPort;

    public DistrictConfig(District district, Directory directory, int pushPort, int pullPort, int brokerPort) {
        this.district = Objects.requireNonNull(district);
        this.directory = Objects.requireNonNull(directory);
        this.pushPort = pushPort;
        this.pullPort = pullPort;
        this.brokerPort = brokerPort;
    }

    public static DistrictConfig parseDistrictServerConfigFileYaml(Path configFilePath) throws FileNotFoundException {

        // Parse the YAML file
        final Yaml parser = new Yaml(new Constructor(YamlRootFile.class));
        final YamlRootFile rootFile = parser.load(new FileInputStream(new File(configFilePath.toUri())));

        // Create a new district config
        return new DistrictConfig(
                new District(rootFile.districtServer.id, rootFile.districtServer.name, rootFile.districtServer.size),
                new Directory(rootFile.districtServer.directoryUrl, rootFile.districtServer.directoryPort),
                rootFile.districtServer.pushPort,
                rootFile.districtServer.pullPort,
                rootFile.districtServer.brokerPort
        );
    }

    public District getDistrict() {
        return district;
    }

    public Directory getDirectory() {
        return directory;
    }

    public int getPushPort() {
        return pushPort;
    }

    public int getPullPort() {
        return pullPort;
    }

    public int getBrokerPort() {
        return brokerPort;
    }

    private static class YamlRootFile{
        public YamlDistrictServerFile districtServer;
    }

    private static class YamlDistrictServerFile {
        public int id;
        public String name;
        public int size;
        public int pushPort;
        public int pullPort;
        public int brokerPort;
        public String directoryUrl;
        public int directoryPort;
    }
}
