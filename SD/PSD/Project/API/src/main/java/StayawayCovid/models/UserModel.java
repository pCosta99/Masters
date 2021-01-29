package StayawayCovid.models;

import com.fasterxml.jackson.annotation.JsonProperty;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

public class UserModel{
    @NotEmpty
    private String district;
    @NotNull
    private int latitude;
    @NotNull
    private int longitude;

    public UserModel(@JsonProperty("district") String district, @JsonProperty("latitude") int latitude, @JsonProperty("longitude") int longitude) {
        this.district = district;
        this.latitude = latitude;
        this.longitude = longitude;
    }

    @JsonProperty
    public String getDistrictName(){
        return this.district;
    }

    @JsonProperty
    public Integer getLatitude(){
        return this.latitude;
    }

    @JsonProperty
    public Integer getLongitude(){
        return this.longitude;
    }
}