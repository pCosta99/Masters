import java.io.Serializable;
import java.util.*;

public class Store implements Serializable {
    private String cod;
    private String name;
    private int queue; //<--- queue time; 0->NULL; 1->litle; 2-> +-; 3->big
    private int avgServiceTime;
    private Coordinates coordinates;
    private Set<Product> availableProducts; //<--- produtos vendidos pela loja;

    public Store(){
        cod = "";
        name     = new String();
        availableProducts = new HashSet<>();
        queue    = 0;
        avgServiceTime = 0;
        coordinates = new Coordinates();
    }
    public Store(String cod,String name, int queue, int avgServiceTime, Coordinates coordinates,Set<Product> availableProducts) {
        this.cod=cod;
        this.name = name;
        this.queue = queue;
        this.avgServiceTime = avgServiceTime;
        setCoordinates(coordinates);
        setAvailableProducts(availableProducts);
    }
    public Store(Store ste){
        this.cod=ste.getCod();
        name = ste.getName();
        queue= ste.getQueue();
        avgServiceTime = ste.getAvgServiceTime();
        setCoordinates(ste.getCoordinates());
        setAvailableProducts(ste.getAvailableProducts());
    }


    public void setAvailableProducts(Set<Product> oldAvailableProducts) {
        availableProducts=new HashSet<>();
        oldAvailableProducts.forEach(e->this.availableProducts.add(e.clone()));
    }
    public Set<Product> getAvailableProducts(){
        Set<Product> newAvailableProducts = new HashSet<>();
        this.availableProducts.forEach(e->newAvailableProducts.add(e.clone()));
        return newAvailableProducts;
    }
    public Coordinates getCoordinates() {
        return coordinates.clone();
    }
    public void setCoordinates(Coordinates coordinates) {
        this.coordinates = coordinates.clone();
    }
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public int getQueue() {
        return queue;
    }
    public void setQueue(int newqueue) {
        this.queue = newqueue;
    }
    public int getAvgServiceTime() {
        return avgServiceTime;
    }
    public void setAvgServiceTime(int avgServiceTime) {
        this.avgServiceTime = avgServiceTime;
    }

    public String getCod() {
        return cod;
    }

    public String validateLogin(String nome, String code) throws ContaNãoExistente {
         if (nome.equals(getName()) && code.equals(getCod())) return getCod();
         else throw new ContaNãoExistente("Credenciais Erradas");
    }

    public void addProducts(Set<Product> products){
        products.forEach(e->availableProducts.add(e.clone()));
    }

    public void removeProducts(List<String> codigosProdutos){
        for(String code :codigosProdutos){
            availableProducts.removeIf(e->e.getCode().equals(code));
        }
    }

    @Override
    public String toString() {
        return "Store{" +
                "cod='" + cod + '\'' +
                ", name='" + name + '\'' +
                ", queue=" + queue +
                ", avgServiceTime=" + avgServiceTime +
                ", coordinates=" + coordinates +
                ", availableProducts=" + availableProducts +
                '}';
    }

    protected Store clone(){return new Store(this);}
}
