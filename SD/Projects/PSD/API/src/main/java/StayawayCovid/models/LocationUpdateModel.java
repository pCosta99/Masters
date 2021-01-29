package StayawayCovid.models;

import com.fasterxml.jackson.annotation.JsonProperty;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

public class LocationUpdateModel {

    @NotEmpty
    private String district;
    @NotNull
    private int initialLatitude;
    @NotNull
    private int initialLongitude;
    @NotNull
    private int finalLatitude;
    @NotNull
    private int finalLongitude;

    public LocationUpdateModel(@JsonProperty("district") String district, @JsonProperty("initialLatitude") int initialLatitude,
                               @JsonProperty("initialLongitude") int initialLongitude, @JsonProperty("finalLatitude") int finalLatitude,
                               @JsonProperty("finalLongitude") int finalLongitude) {
        this.district = district;
        this.initialLatitude = initialLatitude;
        this.initialLongitude = initialLongitude;
        this.finalLatitude = finalLatitude;
        this.finalLongitude = finalLongitude;
    }

    public String getDistrict() {
        return district;
    }

    public int getInitialLatitude() {
        return initialLatitude;
    }

    public int getInitialLongitude() {
        return initialLongitude;
    }

    public int getFinalLatitude() {
        return finalLatitude;
    }

    public int getFinalLongitude() {
        return finalLongitude;
    }
}
