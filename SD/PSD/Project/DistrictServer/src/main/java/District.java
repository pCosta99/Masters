public class District {

    private final int districtId;
    private final String districtName;
    private final int size;

    private final DistrictStore districtStore;

    public District(int districtId, String districtName, int size) {
        this.districtId = districtId;
        this.districtName = districtName;
        this.size = size;
        this.districtStore = new DistrictStore(size);
    }

    public int getDistrictId() {
        return districtId;
    }

    public String getDistrictName() {
        return districtName;
    }

    public int getDistrictSize() {
        return size;
    }

    public DistrictStore getDistrictStore() {
        return districtStore;
    }
}
