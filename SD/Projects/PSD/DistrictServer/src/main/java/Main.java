import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Paths;

public class Main {
    public static void main(String [] args) throws FileNotFoundException {

        // If the user didn't provided the required file path, or provided more than one.
        if (args.length != 1){
            System.err.println("> Required to provide config file path. Default is: configs/district-server-n.yaml");
            System.exit(2);
        }

        // Parse the configuration file (default path: configs/district-server-n.yaml)
        DistrictConfig config = DistrictConfig.parseDistrictServerConfigFileYaml(Paths.get(args[0]));

        System.out.println("------------ District Server Info ------------------");
        System.out.println("\t-> District Id: " + config.getDistrict().getDistrictId());
        System.out.println("\t-> District Name: " + config.getDistrict().getDistrictName());
        System.out.println("\t-> District Size: " + config.getDistrict().getDistrictSize());
        System.out.println("\t-> Push Port: " + config.getPushPort());
        System.out.println("\t-> Pull Port: " + config.getPullPort());
        System.out.println("\t-> Broker Port: " + config.getBrokerPort());
        System.out.println("\t-> Directory URL: " + config.getDirectory().getDirectoryUrl());
        System.out.println("\t-> Directory Port: " + config.getDirectory().getDirectoryPort());
        System.out.println("----------------------------------------------------");

        // POST this district to the Directory
        try {
            config.getDirectory().getDirectoryService().addDistrict(config.getDistrict().getDistrictName(), config.getDistrict().getDistrictSize());
        } catch (IOException e) {
            System.err.println("> Error during add new district, maybe Directory Server not Running!");
        }

        // Create a ZMQ District Server
        ZMQDistrictServer zmqDistrictServer = new ZMQDistrictServer(config);

        // Run ZMQ Server
        zmqDistrictServer.run();
    }
}
