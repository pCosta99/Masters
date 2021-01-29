import java.io.Serializable;
public class GPS implements Serializable
{
    private double longitude;
    private double latitude;
    
    public GPS(){ 
     this.longitude = 0;
     this.latitude = 0;
    }
    public GPS (double longitude,double latitude){ 
     this.longitude = longitude;
     this.latitude = latitude;
    }
    public GPS(GPS g){ 
     this.longitude = g.getLongitude();
     this.latitude = g.getLatitude();
    }
    public double getLongitude(){ 
     return this.longitude;
    }
    public double getLatitude(){ 
     return this.latitude;
    }
    public String toString(){
       return "(" + latitude + ", " + longitude + ")";
    }
    
    public GPS clone(){ 
     return new GPS (this);
    }
} 