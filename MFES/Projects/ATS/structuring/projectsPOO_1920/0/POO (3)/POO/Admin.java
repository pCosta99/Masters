import java.io.Serializable;

public class Admin extends Utilizador implements Serializable {

    /*
     * Construtores da classe Administrador
     */
    public Admin() {

        super("admin", "admin", "admin", 0, 0);
    }

    public Admin(Admin u) {

        super(u);
    }

    public Admin(String nome, String email, String password, double posX, double posY) {

        super(nome, email, password, posX, posY);
    }

    public String toString() {
        return super.toString();
    }

    public Admin clone() {
        return new Admin(this);
    }
}
