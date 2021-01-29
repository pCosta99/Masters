import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

public class Volunteer implements Comparable, Serializable {
    private String code;               //<--- Volunteer identification code.
    private String name;            //<--- Volunteer name.
    private Coordinates coordinates;//<--- Volunteer current location in coordinates.
    private double geographicRadius;   //<--- Volunteer geographicRadius for limitation of delivery distance.
    private Boolean available;      //<--- Volunteer Availability. True if available, False if not.
    private int deliveryType;       //<--- Volunteer deliveryType. 1-> Food; 2->Electronics; 3->Others
    private double speed;           //<--- Volunteer current speed. in kmh
    private double classification;     //<--- Volunteer avg classification.
    private boolean medicalDelivery;//<--- Volunteer certificate to medical Delivery. True if is certificated, false if not.
    private Map<String,Order> deliveryRegist; // <---Volunteer past orders.

    //Object Volunteer Constructors.
    public Volunteer(){
        code           ="";
        name           =new String();
        coordinates    =new Coordinates();
        available      =false;
        deliveryType   =1;
        speed          =0;
        classification =0;
        medicalDelivery=false;
        deliveryRegist =new HashMap<>();
    }
    public Volunteer(String code, String name, Coordinates coordinates,
                     double geographicRadius, Boolean available, int deliveryType,
                     double speed, double classification, boolean medicalDelivery,Map<String,Order>deliveryRegist) {
        this.code              = code;
        this.name              = name;
        setCoordinates(coordinates); //It's necessary to make set in order to have a clone of Coordinates. Guaranteed  encapsulation.
        this.geographicRadius  = geographicRadius;
        this.available         = available;
        this.deliveryType      = deliveryType;
        this.speed             = speed;
        setDeliveryRegist(deliveryRegist); //It's necessary in order to have a clone of Map<code,Order>. Guaranteed  encapsulation.
        this.classification    = classification;
        this.medicalDelivery   = medicalDelivery;
    }
    public Volunteer(Volunteer volunteer){
        code                   = volunteer.getCode();
        name                   = volunteer.getName();
        setCoordinates(volunteer.getCoordinates());
        setDeliveryRegist(volunteer.getDeliveryRegist());  //It's necessary in order to Guarantee encapsulation.
        geographicRadius       = volunteer.getGeographicRadius();
        this.available         = volunteer.getAvailable();
        this.deliveryType      = volunteer.getDeliveryType();
        this.speed             = volunteer.getSpeed();
        this.classification    = volunteer.getClassification();
        this.medicalDelivery   = volunteer.isMedicalDelivery();
    }


    //Getters and Setters.
    public String getCode() {
        return code;
    }
    public void setCode(String code) {
        this.code = code;
    }
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public Coordinates getCoordinates() {
        return coordinates.clone();
    }
    public void setCoordinates(Coordinates coordinates) {
        this.coordinates = coordinates.clone();
    }
    public double getGeographicRadius() {
        return geographicRadius;
    }
    public void setGeographicRadius(double geographicRadius) {
        this.geographicRadius = geographicRadius;
    }
    public Boolean getAvailable() {
        return available;
    }
    public void setAvailable(Boolean available) {
        this.available = available;
    }
    public int getDeliveryType() {
        return deliveryType;
    }
    public void setDeliveryType(int deliveryType) {
        this.deliveryType = deliveryType;
    }
    public double getSpeed() {
        return speed;
    }
    public void setSpeed(double speed) {
        this.speed = speed;
    }
    public double getClassification() {
        return classification;
    }
    public void setClassification(double classification) {
        this.classification = classification;
    }
    public boolean isMedicalDelivery() {
        return medicalDelivery;
    }
    public void setMedicalDelivery(boolean medicalDelivery) {
        this.medicalDelivery = medicalDelivery;
    }
    public void setDeliveryRegist(Map<String,Order> newDeliveryRegist){
        this.deliveryRegist=new HashMap<>();
        newDeliveryRegist.forEach((key, value) -> this.deliveryRegist.put(key, value));
    }
    public Map<String,Order> getDeliveryRegist(){
        Map<String,Order> newDeliveryRegist=new HashMap<>();
        this.deliveryRegist.forEach(newDeliveryRegist::put);
        return newDeliveryRegist;
    }

    public void addOrder(Order order){
        deliveryRegist.put(order.getCode(),order.clone());
    }


    public void replaceOrder(Order order){
        deliveryRegist.remove(order.getCode());
        deliveryRegist.put(order.getCode(),order.clone());
    }

    public void atualizarClass(){
        double soma = deliveryRegist.values().stream().filter(Order::isClassificated).mapToDouble(Order::getDeliveryClassification).sum();
        double size = deliveryRegist.values().stream().filter(Order::isClassificated).count();
        setClassification(soma/size);
    }


    @Override
    public String toString() {
        return "Volunteer{" +
                "code=" + code +
                ", name='" + name + '\'' +
                ", coordinates=" + coordinates +
                ", geographicRadius=" + geographicRadius +
                ", available=" + available +
                ", deliveryType=" + deliveryType +
                ", speed=" + speed +
                ", classification=" + classification +
                ", medicalDelivery=" + medicalDelivery +
                ", deliveryRegist=" + deliveryRegist +
                '}';
    }

    protected Volunteer clone(){
        return new Volunteer(this);
    }

    public String validateLogin(String nome, String code) throws ContaNãoExistente {
        if (nome.equals(getName()) && code.equals(getCode())) return code;
        else throw new ContaNãoExistente("Credenciais Erradas");
    }

    public int compareTo(Object o) {
        Volunteer Object = (Volunteer) o;
        return getCode().compareTo(Object.getCode());
    }
}
