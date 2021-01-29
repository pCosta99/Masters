package entity;

public class District {

    private final Integer districtId; // Id of the district
    private final String name;        // Name of the district
    private final Integer n;          // Used to know each possible locations

    public District(Integer id, String name, Integer n) {
        this.districtId = id;
        this.name = name;
        this.n = n;
    }

    public Integer getDistrictId() {
        return districtId;
    }

    public String getName() {
        return name;
    }

    public Integer getN() {
        return n;
    }
}
