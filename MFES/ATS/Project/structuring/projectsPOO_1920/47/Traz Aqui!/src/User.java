import java.io.Serializable;
public class User implements Comparable, Serializable {
    private String cod;
    private String name;
    private Coordinates gps;

    public User(String cod, String name, Coordinates gps) {
        this.cod = cod;
        this.name = name;
        this.gps = gps.clone();
    }

    public User(User u){
        this.cod = u.getCod();
        this.name = u.getName();
        this.gps = u.getGps().clone();
    }

    public String getCod() {
        return cod;
    }

    public void setCod(String cod) {
        this.cod = cod;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Coordinates getGps() {
        return gps.clone();
    }

    public void setGps(Coordinates gps) {
        this.gps = gps.clone();
    }

    public boolean equalsRegisto(User obj) {
        boolean result = false;
        if(obj.getName().equals(getName()) && obj.getGps().equals(getGps())) result = true;
        return result;
    }

    public String validateLogin(String nome, String code) throws ContaNãoExistente {
        if (nome.equals(getName()) && code.equals(getCod())) return code;
        else throw new ContaNãoExistente("Credenciais Erradas");
    }

    public User clone(){return new User(this);}

    public int compareTo(Object object) {
        User o = (User) object;
        return getName().compareTo(o.getName());
    }

    @Override
    public String toString() {
        return "User{" +
                "cod='" + cod + '\'' +
                ", name='" + name + '\'' +
                ", gps=" + gps +
                '}';
    }
}
