import java.io.Serializable;
import java.util.Map;

public class Transporter extends Volunteer implements Serializable {
    private double price; //<--- price per kg plus distance.
    private int NIF;
    private int deliveryCapacity; //<--- How many orders can do at same time.

    public Transporter(){
        super();
        price=0; deliveryCapacity=1; NIF=0;
    }
    public Transporter(String code, String name, Coordinates coordinates,
                   double geographicRadius, Boolean available, int deliveryType,
                   double speed, double classification, boolean medicalDelivery,
                   Map<String,Order> deliveryRegist, double price, int deliveryCapacity,int Nif){
        super(code,name,coordinates,geographicRadius,available,deliveryType,speed,classification,medicalDelivery,deliveryRegist);
        this.price=price;
        this.deliveryCapacity=deliveryCapacity;
        this.NIF=Nif;
    }
    public Transporter(Transporter trans){
        super(trans.getCode(),trans.getName(),trans.getCoordinates(),trans.getGeographicRadius(),
              trans.getAvailable(),trans.getDeliveryType(),trans.getSpeed(),trans.getClassification(),
                trans.isMedicalDelivery(),trans.getDeliveryRegist());
        price=trans.getPrice();
        deliveryCapacity=trans.getDeliveryCapacity();
        NIF=trans.getNIF();
    }

    public double getPrice() {
        return price;
    }
    public void setPrice(double price) {
        this.price = price;
    }
    public int getDeliveryCapacity() {
        return deliveryCapacity;
    }
    public void setDeliveryCapacity(int deliveryCapacity) {
        this.deliveryCapacity = deliveryCapacity;
    }

    public int getNIF() {
        return NIF;
    }

    public void setNIF(int NIF) {
        this.NIF = NIF;
    }

    public double getKmPercorridos(){
        double km = 0;
        Map<String,Order> deliveryRegist = super.getDeliveryRegist();
        if(deliveryRegist!=null){
            km += deliveryRegist.values().stream().mapToDouble(Order::getDeliveryPrice).count();
            km/= this.getPrice();
        }
        return km;
    }

    public String toString() {
        return "Transporter{" +
                "code=" + this.getCode() +
                ", name='" + this.getName() +
                ", coordinates=" + this.getCoordinates() +
                ", geographicRadius=" + this.getGeographicRadius() +
                ", available=" + this.getAvailable() +
                ", deliveryType=" + this.getDeliveryType() +
                ", speed=" + this.getSpeed() +
                ", classification=" + this.getClassification()+
                ", medicalDelivery=" + this.isMedicalDelivery() +
                ", deliveryRegist=" + this.getDeliveryCapacity() +
                "price=" + price +
                "NIF" + NIF +
                ", deliveryCapacity=" + deliveryCapacity +
                '}';
    }
    protected Transporter clone(){return new Transporter(this);}

    public int compareTo(Transporter t1) {
        return this.getNIF()-t1.getNIF();
    }
}
