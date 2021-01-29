package utils;

public class TopLocation {

    private String district;
    private String users;
    private String maxUsers;
    private Coordinate coordinate;

    public TopLocation(String district, String users, String maxUsers, String latitude, String longitude){
        this.district = district;
        this.users = users;
        this.maxUsers = maxUsers;
        this.coordinate = new Coordinate(latitude,longitude);
    }

    public String getDistrict(){
        return this.district;
    }

    public String getUsers(){
        return this.users;
    }

    public String getMaxUsers(){
        return this.maxUsers;
    }

    public String getLatitude(){
        return this.coordinate.getLatitude();
    }

    public String getLongitude(){
        return this.coordinate.getLongitude();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("   "+coordinate.toString() +"  ");
        sb.append("\t");
        sb.append(" "+this.district+" ");
        sb.append("\t");
        sb.append("  "+this.users+" ");
        sb.append("\t");
        sb.append("    "+this.maxUsers);
        return sb.toString();
    }
}
