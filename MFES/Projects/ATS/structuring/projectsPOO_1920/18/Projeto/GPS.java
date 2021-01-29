import java.lang.Math;

public class GPS {
    private float latitude;
    private float longitude;
    
    public GPS(){
        this.latitude=0;
        this.longitude=0;
    }

    public GPS(float la, float lo){
        this.latitude=la;
        this.longitude=lo;
    }
    
    public GPS(GPS gps){
        this.latitude=gps.getLatitude();
        this.longitude=gps.getLongitude();
    }
    
    public float getLatitude (){
        return this.latitude;
    }
    
    public void setLatitude (float la){
        this.latitude = la;
    }
    
    public float getLongitude (){
        return this.longitude;
    }
    
    public void setLongitude (float lo){
        this.longitude = lo;
    }
    
    public boolean equals (Object obj){
        if (obj==this) return true;
        if (obj==null || obj.getClass()!=this.getClass()) return false;
        GPS g=(GPS) obj;
        return this.latitude==g.getLatitude()&& this.longitude==g.getLongitude();
    }
    
    public GPS clone(){
        return new GPS(this);
    }
    
    public String toString (){
        StringBuilder sb=new StringBuilder();
        sb.append(this.latitude).append(",").append(this.longitude);
        return sb.toString();
    }
    
    public double distancia(GPS g){
        double lat1=Math.toRadians(this.latitude);
        double lat2=Math.toRadians(g.getLatitude());
        double lon1=Math.toRadians(this.longitude);
        double lon2=Math.toRadians(g.getLongitude());
        double raioTerra=6371.01;
        return raioTerra * Math.acos(Math.sin(lat1)*Math.sin(lat2) + Math.cos(lat1)*Math.cos(lat2)*Math.cos(lon1 - lon2));
    }
    
}
