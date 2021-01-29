package Model;

import java.io.Serializable;
import java.util.*;

/** Declaration of Class Loja which contains the required information on each store.
 *
 *  String storeCode - The store's system code.
 *  String name - The store's name.
 *  int queue - The amount of packages waiting to be picked up by either a Volunteer or a transporting company.
 *  Location localizacao - The location from where we assume the Volunteer will start every service.
 *  List of Strings encomendas - List with the codes of previously sold packages.
 *  List of Products productCatalog - List of Products a store has available to be purchased.
 */
public class Loja implements Serializable {
    private String storeCode;
    private String name;
    private int queue;
    private Location location;
    private List<String> encomendas;
    private List<Produto> productCatalog;

    /* Class constructors*/
    public Loja(){
        this.storeCode = "";
        this.name = "";
        this.queue = 0;
        this.encomendas = new ArrayList<>();
        this.productCatalog = new LinkedList<>();
    }
    public Loja(String c, String n, int q, Location location, List<String> encomendas, List<Produto> produtos){
        this.storeCode = c;
        this.name = n;
        this.queue = q;
        setLocation(location);
        setEncomendas(encomendas);
        setProductCatalog(produtos);
    }
    public Loja(Loja l){
        setStoreCode(l.getStoreCode());
        setName(l.getName());
        setQueue(l.getQueue());
        setLocation(l.getLocation());
        setEncomendas(l.getEncomendas());
        setProductCatalog(l.getProductCatalog());
    }

    /* Getters and Setters*/
    public String getStoreCode() {
        return storeCode;
    }
    public void setStoreCode(String storeCode) {
        this.storeCode = storeCode;
    }

    public String getName(){
        return this.name;
    }
    public void setName(String name){
        this.name = name;
    }

    public int getQueue(){
        return this.queue;
    }
    public void setQueue(int q){
        this.queue = q;
    }

    public Location getLocation() {
        return this.location;
    }
    public void setLocation(Location location) {
        this.location = location;
    }

    public List<String> getEncomendas(){
        List<String> aux = new ArrayList<>(this.encomendas.size());
        aux.addAll(encomendas);
        return aux;
    }
    public void setEncomendas(List<String> encomendas){
        this.encomendas = new ArrayList<>(encomendas.size());
        this.encomendas.addAll(encomendas);
    }

    public LinkedList<Produto> getProductCatalog() {
        LinkedList<Produto> s = new LinkedList<>();
        for(Produto p : this.productCatalog){
            s.add(p.clone());//eu ao mecher vou ter de dar set no fim se tiver clone
        }
        return s;
    }
    public void setProductCatalog(List<Produto> productCatalog) {
        this.productCatalog = new LinkedList<>();
        for(Produto p : productCatalog){
            this.productCatalog.add(p.clone());
        }
    }

    @Override
    public Loja clone(){
        return new Loja(this);
    }
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Loja loja = (Loja) o;
        return this.queue == loja.queue &&
                Objects.equals(this.storeCode, loja.storeCode) &&
                Objects.equals(this.name, loja.name) &&
                Objects.equals(this.location, loja.location) &&
                Objects.equals(this.encomendas, loja.encomendas) &&
                Objects.equals(this.productCatalog, loja.productCatalog);
    }
    @Override
    public String toString() {
        return  "Código da Loja = " + storeCode + " | " +
                "Nome = " + name + " | " +
                "Queue = " + queue + " | " +
                "Location = " + location.toString() + " | " +
                "Encomendas = " + encomendas.toString() + " | " +
                "Catálogo de Produtos = " + productCatalog.toString();
    }

    /** Adds a Product to the Product Catalog of a store
     *
     * @param p - Product that will be added
     */
    public void addProduct(Produto p){
        productCatalog.add(p);
    }

    /** Removes the product with the given String as a code.
     *
     * @param referencia - code of the Product we wish to remove.
     */
    public void removeProduct(String referencia){
        productCatalog.removeIf(p -> p.getReferencia().equals(referencia));
    }

    /** Checks if a Store's Code is valid to be accepted on the system by checking if the first letter of the code is a lower case "s" and all other digits are integer numbers.
     *
     * @param l - Store whose code will be analyzed.
     * @return - Validation of the Store's code.
     */
    public static boolean validate(Loja l){
        String code = (l.getStoreCode());
        boolean valid = true;
        for(int i = 1; i < code.length() && valid; i++){
            if(!(Character.isDigit(code.charAt(i)))){
                valid = false;
            }
        }
        return (code.charAt(0) == 'l' && valid);
    }

    /** Given the code of a package that has been set as delivered by the user, adds the code to the User's history.
     *
     * @param s - code of the received package that will be added to the user's history.
     */
    public void addEncomendasHist(String s){
        int size = this.encomendas.size();
        List<String> r = new ArrayList<>(size+1);
        int i;
        for(i = 0; i < size; i++){
            r.add(i,this.encomendas.get(i));
        }
        r.add(i,s);
        setEncomendas(r);
    }
}
