package Model;

import java.io.Serializable;
import java.util.*;

/** Declaration of Class User which contains the required information on each user/client.
 *
 *  String codUser - The user's system code.
 *  String name - The user's name.
 *  Location localizacao - The location to where we assume the user will want his goods delivered at.
 *  List of Strings encomendas - List with the codes of previously requested packages.
 *  String password - The user's password to log-in to the app.
 */

public class User implements Serializable {
    private String CodUser; //Código do utilizador
    private String name; //Nome do utilizador
    private Location location; //GPS do utilizador
    private List<String> encomendas; //Lista com os codigos de encomenda feitas pelo utilizador

    /* Class constructors*/
    public User (){
        this.CodUser = "";
        this.name = "";
        this.encomendas = new ArrayList<>();
        this.location = new Location();
    }
    public User(String CodUser, String name, List<String> encomendas, Location l){
        setCodUser(CodUser);
        setName(name);
        setEncomendas(encomendas);
        setLocation(l);
    }
    public User(User u){
        this.CodUser = u.getCodUser();
        this.name = u.getName();
        this.location = u.getLocation();
        setEncomendas(u.getEncomendas());
    }

    /* Getters and Setters*/
    public String getName(){
        return this.name;
    }
    public void setName(String name){
        this.name = name;
    }

    public String getCodUser() {
        return CodUser;
    }
    public void setCodUser(String codUser) {
        CodUser = codUser;
    }

    public Location getLocation(){
        return this.location.clone();
    }
    public void setLocation(Location l){
        this.location = new Location(l);
    }

    public List<String> getEncomendas() {
        return encomendas;
    }
    public void setEncomendas(List<String> encomendas) {
        this.encomendas = encomendas;
    }

    @Override
    public User clone(){
        return new User(this);
    }
    @Override
    public String toString() {
        return  "User Code = " + CodUser + " | " +
                "Name ='" + name + " | " +
                "Location = " + location.toString() + " | " +
                "Encomendas = " + encomendas.toString();
    }
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return Objects.equals(this.CodUser, user.CodUser) &&
                Objects.equals(this.name, user.name) &&
                Objects.equals(this.location, user.location) &&
                Objects.equals(this.encomendas, user.encomendas);
    }

    /** Checks if a User Code is valid to be accepted on the system by checking if the first letter of the code is a lower case "u" and all other digits are integer numbers.
     *
     * @param u - user whose code will be analyzed.
     * @return - Validation of the User's code.
     */
    public static boolean validate(User u){
        String code = u.getCodUser();
        boolean valid = true;
        for(int i = 1; i < code.length() && valid; i++){
            if(!(Character.isDigit(code.charAt(i)))){
                valid = false;
            }
        }
        return (code.charAt(0) == 'u' && valid);
    }

    /** Adds a package code to the history of packages delivered to this user.
     *
     * @param codeEnc - code of the recently received package.
     * @return List with all previous packages received by the user plus the new one.
     */
    public List<String> addEnc(String codeEnc){
        int size = this.getEncomendas().size();
        List<String> a = new ArrayList<>(size+1);
        int i;
        for(i = 0; i < size+1; i++){
            a.add(i,this.getEncomendas().get(i));
        }
        a.add(i,codeEnc);
        return a;
    }
}
