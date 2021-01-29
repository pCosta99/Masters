import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

public class Order implements Comparable,Serializable {
    private String code; //<--- Order identification code.
    LocalDate date; //<--- Order date request.
    private String sellerCode; //<--- Seller identification code.
    private String buyerCode; //<--- Buyer  identification code.
    private String deliveryCode; //<--- Delivey Company  identification code.
    private double weight; //<--- Weight of order.
    private int orderType;       //<--- Order type. 1-> Food; 2->Electronics; 3->Others
    private double deliveryPrice; // <--- Delivery price assigned by transporter. 0 if it's volunteer.
    private double orderPrice; // <--- Order price.
    private double orderDeliveryTime; // <-- Order Delivery Time
    private double deliveryClassification; // <-- Classification of delivery company/volunteer
    private boolean classificated;;
    private Set<Product> products; //<--- products ordered.

    //Object Constructor.
    public Order(){
        code           ="";
        sellerCode     ="";
        buyerCode      ="";
        deliveryCode      ="";
        deliveryPrice  =0;
        weight         =0;
        orderPrice     =0;
        orderType      =1;
        orderDeliveryTime      =0;
        deliveryClassification      =0;
        date= LocalDate.now();
        classificated=false;
        this.products  =new HashSet<>();
    }
    public Order(String codigo, String codVendedor, String codComprador,String deliveryCode,LocalDate date, double peso,int orderType, double precoDeTransporte, double precoEnc,double orderDeliveryTime,double deliveryClassification,boolean classificated,Set<Product> products) {
        this.code          = codigo;
        this.sellerCode    = codVendedor;
        this.deliveryCode  = deliveryCode;
        this.buyerCode     = codComprador;
        this.weight        = peso;
        this.date          = date;
        this.orderType     = orderType;
        this.deliveryPrice = precoDeTransporte;
        this.orderPrice    = precoEnc;
        this.orderDeliveryTime  =orderDeliveryTime;
        this.deliveryClassification  =deliveryClassification;
        this.classificated=classificated;
        setProducts(products);
    }
    public Order(Order enc){
        this.code          = enc.getCode();
        this.sellerCode    = enc.getSellerCode();
        this.buyerCode     = enc.getBuyerCode();
        this.weight        = enc.getWeight();
        this.deliveryPrice = enc.getDeliveryPrice();
        this.orderType     = enc.getOrderType();
        this.orderPrice    = enc.getOrderPrice();
        this.date          = enc.getDate();
        this.orderDeliveryTime  =enc.getOrderDeliveryTime();
        this.deliveryClassification  =enc.getDeliveryClassification();
        this.deliveryCode = enc.getDeliveryCode();
        this.classificated=enc.isClassificated();
        setProducts(enc.getProducts());
    }

    //Getters and Setters.
    public String getCode() {
        return code;
    }
    public void setCode(String code) {
        this.code = code;
    }
    public String getSellerCode() {
        return sellerCode;
    }
    public void setSellerCode(String sellerCode) {
        this.sellerCode = sellerCode;
    }
    public String getBuyerCode() {
        return buyerCode;
    }
    public void setBuyerCode(String buyerCode) {
        this.buyerCode = buyerCode;
    }
    public double getWeight() {
        return weight;
    }
    public void setWeight(double weight) {
        this.weight = weight;
    }
    public double getDeliveryPrice() {
        return deliveryPrice;
    }
    public void setDeliveryPrice(double deliveryPrice) {
        this.deliveryPrice = deliveryPrice;
    }
    public double getOrderPrice() {
        return orderPrice;
    }
    public void setOrderPrice(double productPrice) {
        this.orderPrice = productPrice;
    }
    public int getOrderType() {
        return orderType;
    }
    public void setOrderType(int orderType) {
        this.orderType = orderType;
    }
    public void setProducts(Set<Product> oldProducts){
        products=new HashSet<>();
        oldProducts.forEach(e->products.add(e.clone()));
    }
    public Set<Product> getProducts(){
        Set<Product> newProducts = new HashSet<>();
        products.forEach(e->newProducts.add(e.clone()));
        return newProducts;
    }
    public LocalDate getDate(){return this.date;}

    public void setDate(LocalDate date) {
        this.date = date;
    }
    public String getDeliveryCode() {
        return deliveryCode;
    }
    public void setDeliveryCode(String deliveryCode) {
        this.deliveryCode = deliveryCode;
    }
    public boolean isClassificated() {
        return classificated;
    }
    public void setClassificated(boolean classificated) {
        this.classificated = classificated;
    }
    public void setOrderDeliveryTime(double orderDeliveryTime) {
        this.orderDeliveryTime = orderDeliveryTime;
    }
    public void setDeliveryClassification(double deliveryClassification) {
        this.deliveryClassification = deliveryClassification;
    }

    public double getOrderDeliveryTime() {
        return orderDeliveryTime;
    }
    public double getDeliveryClassification() {
        return deliveryClassification;
    }

    @Override
    public String toString() {
        return "Order{" +
                "code='" + code + '\'' +
                ", date=" + date +
                ", sellerCode='" + sellerCode + '\'' +
                ", buyerCode='" + buyerCode + '\'' +
                ", deliveryCode='" + deliveryCode + '\'' +
                ", weight=" + weight +
                ", orderType=" + orderType +
                ", deliveryPrice=" + deliveryPrice +
                ", orderPrice=" + orderPrice +
                ", orderDeliveryTime=" + orderDeliveryTime +
                ", deliveryClassification=" + deliveryClassification +
                ", classificated=" + classificated +
                ", products=" + products +
                '}';
    }

    protected Order clone(){return new Order(this);}

    @Override
    public int compareTo(Object o) {
        Order obj = (Order) o;
        if(obj.getOrderPrice()==getOrderPrice()){
            return obj.getCode().compareTo(getCode());
        }
        return Double.compare(getOrderPrice(),obj.getOrderPrice());
    }
}
