package services;

import utils.ClientConfig;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

public class HttpService {

    private final String URL;
    private final String HEALTH_URL;

    public HttpService(ClientConfig config) {
        this.URL = config.directoryServer.URL;
        this.HEALTH_URL = config.directoryServer.HEALTH_URL;
        this.activeVerification();
    }

    // query the API and check it's health
    public void activeVerification() {
        try {
            URL url = new URL(HEALTH_URL + "healthcheck");
            HttpURLConnection con = (HttpURLConnection) url.openConnection();

        } catch (IOException e){
            System.err.println("> Directory Server is not Running!");
        }
    }

    public String getRequest(String request){
        HttpURLConnection c = null;
        try {
            URL u = new URL(URL + request);
            c = (HttpURLConnection) u.openConnection();
            c.setRequestMethod("GET");
            c.setRequestProperty("Content-length", "0");
            c.setUseCaches(false);
            c.setAllowUserInteraction(false);
            c.connect();
            int status = c.getResponseCode();

            switch (status) {
                case 200:
                case 201:
                    BufferedReader br = new BufferedReader(new InputStreamReader(c.getInputStream()));
                    StringBuilder sb = new StringBuilder();
                    String line;
                    while ((line = br.readLine()) != null) {
                        sb.append(line).append("\n");
                    }
                    br.close();
                    return sb.toString();
            }

        } catch (IOException ex) {
            Logger.getLogger(getClass().getName()).log(Level.SEVERE, null, ex);
        } finally {
            if (c != null) {
                try {
                    c.disconnect();
                } catch (Exception ex) {
                    Logger.getLogger(getClass().getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
        return null;
    }
}