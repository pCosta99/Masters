package utils;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import utils.entity.DirectoryServer;
import utils.entity.FrontendServer;

import java.io.FileReader;

/**
 * Util class to parse the JSON containing all Client configuration.
 */
public class JsonParser {


    public static ClientConfig parseConfig(String path) {
        JSONParser parser = new JSONParser();

        try {
            Object obj = parser.parse(new FileReader(path));

            // A JSON object. Key value pairs are unordered.
            JSONObject jsonObject = (JSONObject) obj;

            // Frontend Server
            JSONObject frontendServerJSON = (JSONObject) jsonObject.get("frontend");

            String frontend_address = (String) frontendServerJSON.get("frontend_address");
            int frontend_port = Integer.parseInt((String) frontendServerJSON.get("frontend_port"));
            int broker_port = Integer.parseInt((String) frontendServerJSON.get("publisher_broker_port"));
            int publisher_port = Integer.parseInt((String) frontendServerJSON.get("frontend_publisher"));

            FrontendServer frontendServer = new FrontendServer(frontend_address, frontend_port, broker_port, publisher_port);

            // Directory Server
            JSONObject directoryServerJSON = (JSONObject) jsonObject.get("directory");

            String directoryName = (String) directoryServerJSON.get("name");
            String directoryUrl = (String) directoryServerJSON.get("url");
            String directoryHealthUrl = (String) directoryServerJSON.get("health_url");

            DirectoryServer directoryServer = new DirectoryServer(directoryName, directoryUrl, directoryHealthUrl);

            System.out.println("------------ Frontend Server ------------");
            System.out.println("\nFrontend Server Address: " + frontend_address +
                    "\nFrontend Server Port: " + frontend_port + "\nBroker Port: " + broker_port + "\n"
                    + "Frontend Publisher Port: " + publisher_port + "\n");

            System.out.println("------------ Directory Server ------------");
            System.out.println("\nDirectory Name: " + directoryName +
                    "\nDirectory URL: " + directoryUrl + "\nDirectory Health URL: " + directoryHealthUrl + "\n");

            return new ClientConfig(frontendServer, directoryServer);

        } catch (Exception e) {
            System.err.println("Error parsing JSON File.");
            e.printStackTrace();
        }

        return null;
    }
}