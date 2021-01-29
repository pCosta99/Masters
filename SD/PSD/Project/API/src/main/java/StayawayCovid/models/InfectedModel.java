package StayawayCovid.models;

import com.fasterxml.jackson.annotation.JsonProperty;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

public class InfectedModel{
    @NotEmpty
    private String district;
    @NotNull
    private int contacts;

    public InfectedModel(@JsonProperty("district") String district, @JsonProperty("contacts") int contacts) {
        this.district = district;
        this.contacts = contacts;
    }

    @JsonProperty
    public String getDistrict() {
        return district;
    }

    @JsonProperty
    public int getContacts() {
        return contacts;
    }
}