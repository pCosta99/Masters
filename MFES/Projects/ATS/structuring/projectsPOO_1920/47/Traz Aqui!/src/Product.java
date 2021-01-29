import java.io.Serializable;

public class Product implements Serializable {
    private String code;
    private String name;
    private double weight;
    private double price;


    public Product(){
        code  ="";
        weight=0;
        price =0;
        name  =new String();
    }
    public Product(String code,String name,double weight,double price){
        this.code  =code;
        this.weight=weight;
        this.price =price;
        this.name=name;
    }
    public Product(Product pro){
        code  =pro.getCode();
        name  =pro.getName();
        weight=pro.getWeight();
        price =pro.getPrice();
    }

    public String getCode() {
        return code;
    }
    public double getWeight() {
        return weight;
    }
    public void setWeight(double weight) {
        this.weight = weight;
    }
    public double getPrice() {
        return price;
    }
    public void setPrice(double price) {
        this.price = price;
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
    public String toStringMenu() {
        return code + "-" + name + "-" + weight +"-" + price;
    }

    @Override
    public String toString() {
        return "Product{" +
                "code='" + code + '\'' +
                ", name='" + name + '\'' +
                ", weight=" + weight +
                ", price=" + price +
                '}';
    }

    protected Product clone(){return new Product(this);}
}
