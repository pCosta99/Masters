package StayawayCovid.models;

import com.fasterxml.jackson.annotation.JsonProperty;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

public class DistrictModel{

    @NotEmpty
    private String district;
    @NotNull
    private int size;

    public DistrictModel(@JsonProperty("district") String district, @JsonProperty("size") int size) {
        this.district = district;
        this.size = size;
    }

   @JsonProperty
   public String getDistrict(){
        return this.district;
    }

   @JsonProperty
   public int getSize(){
       return this.size;
    }
}