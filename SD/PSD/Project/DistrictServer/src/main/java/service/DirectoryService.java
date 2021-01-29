package service;

import com.google.gson.Gson;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;

import java.io.IOException;

public class DirectoryService {

    // Google Json Helper
    private static final Gson gson = new Gson();

    // Directory
    private final String BASE_URL;

    public DirectoryService(String url, int port){
        this.BASE_URL = url + ":" + port;
    }

    /**
     * Add a new district to the directory.
     * @param district The district name.
     * @param size The district size.
     * @throws IOException
     */
    public void addDistrict(String district, Integer size) throws IOException {
        District districtObj = new District(district, size);
        String json = gson.toJson(districtObj);
        sendPostRequest(BASE_URL + "/District", json, "districts");
    }

    /**
     * Add a new user to the directory.
     * @param district The user district name.
     * @param latitude The current latitude of the user.
     * @param longitude The current longitude of the user.
     * @throws IOException
     */
    public void newUser(String district, Integer latitude, Integer longitude) throws IOException {
        User userObj = new User(district, latitude, longitude);
        String json = gson.toJson(userObj);
        sendPostRequest(BASE_URL + "/User", json, "new user");
    }

    /**
     * Add a new infected to the directory.
     * @param district The district name.
     * @param contacts The number of contacts before getting infected.
     * @throws IOException
     */
    public void newInfected(String district, Integer contacts) throws IOException {
        Infected infectedObj = new Infected(district, contacts);
        String json = gson.toJson(infectedObj);
        sendPostRequest(BASE_URL + "/Infected", json, "new infected");
    }

    /**
     * Update user location to a new one.
     * @param district The district name.
     * @param initialLatitude The user initials latitude.
     * @param initialLongitude The user initials longitude.
     * @param finalLatitude The user final latitude.
     * @param finalLongitude The user final longitude.
     * @throws IOException
     */
    public void updateUserLocation(String district, Integer initialLatitude, Integer initialLongitude, Integer finalLatitude, Integer finalLongitude) throws IOException {
        LocationUpdate locationUpdateObj = new LocationUpdate(district, initialLatitude, initialLongitude, finalLatitude, finalLongitude);
        String json = gson.toJson(locationUpdateObj);
        sendPutRequest(BASE_URL + "/MoveUser", json, "location update");
    }

    /**
     * Method that sends a POST Request to a given URL.
     * @param urlString URL of the API Resource.
     * @param json JSON Object already converted to String.
     * @throws IOException
     */
    private void sendPostRequest(String urlString, String json, String resource) {
        HttpClient httpClient = HttpClientBuilder.create().build();
        HttpPost post = new HttpPost(urlString);

        try {
            HttpResponse response = null;
            StringEntity postingString = new StringEntity(json);
            post.setEntity(postingString);
            post.setHeader("content-type", "application/json");
            response = httpClient.execute(post);

            if (response.getStatusLine().getStatusCode() == 200){
                System.out.println("> POST Successful on " + resource + "!");
            } else {
                System.err.println("> Error occurred during POST on " + resource + "!");
            }
        } catch (IOException e) {
            System.err.println("> API Server Maybe Not Running!");
        }
    }

    /**
     * Method that sends a PUT Request to a given URL.
     * @param urlString URL of the API Resource.
     * @param json JSON Object already converted to String.
     * @throws IOException
     */
    private void sendPutRequest(String urlString, String json, String resource) {
        HttpClient httpClient = HttpClientBuilder.create().build();
        HttpPut put = new HttpPut(urlString);
        try {
            StringEntity puttingString = new StringEntity(json);
            put.setEntity(puttingString);
            put.setHeader("Content-type", "application/json");
            HttpResponse response = httpClient.execute(put);

            if (response.getStatusLine().getStatusCode() == 200) {
                System.out.println("> PUT Successful on " + resource + "!");
            } else {
                System.err.println("> Error occurred during PUT on " + resource + "!");
            }
        }catch (IOException e){
            System.err.println("> API Server Maybe Not Running!");
        }
    }


    /**********************************************************************************
     *                                     CLASSES                                    *
     **********************************************************************************/

    private static class District{
         String district;
         int size;

        public District(String district, Integer size) {
            this.district = district;
            this.size = size;
        }
    }

    private static class User{
        String district;
        Integer latitude;
        Integer longitude;

        public User(String district, Integer latitude, Integer longitude) {
            this.district = district;
            this.latitude = latitude;
            this.longitude = longitude;
        }
    }

    private static class Infected{
        String district;
        Integer contacts;

        public Infected(String district, Integer contacts) {
            this.district = district;
            this.contacts = contacts;
        }
    }

    private static class LocationUpdate{
        String district;
        Integer initialLatitude;
        Integer initialLongitude;
        Integer finalLatitude;
        Integer finalLongitude;

        public LocationUpdate(String district, Integer initialLatitude, Integer initialLongitude, Integer finalLatitude, Integer finalLongitude) {
            this.district = district;
            this.initialLatitude = initialLatitude;
            this.initialLongitude = initialLongitude;
            this.finalLatitude = finalLatitude;
            this.finalLongitude = finalLongitude;
        }
    }
}