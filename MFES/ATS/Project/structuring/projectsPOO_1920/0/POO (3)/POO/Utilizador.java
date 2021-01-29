import java.io.Serializable;

public abstract class Utilizador implements Serializable {

    // variaveis de instancia
    private String       nome;
    private String       email;
    private String       password;
    private double       posX;
    private double       posY;
    
    // Construtores
    public Utilizador() {

        this.nome = new String();
        this.email = new String();
        this.password = new String();
        this.posX = 0;
        this.posY = 0;

    }

    public Utilizador(String nome, String email, String password, double posX, double posY) {

        this.nome = nome;
        this.email = email;
        this.password = password;
        this.posX = posX;
        this.posY = posY;

    }

    public Utilizador(Utilizador u) {

        this.nome = u.getNome();
        this.email = u.getEmail();
        this.password = u.getPassword();
        this.posX = u.getPosX();
        this.posY = u.getPosY();
    }

    /* Gets e Sets */
    public String getNome() {
        return this.nome;
    }

    public String getEmail() {
        return this.email;
    }

    public String getPassword() {
        return this.password;
    }

    public double getPosX() {
        return this.posX;
    }

    public double getPosY() {
        return this.posY;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public void setPosX(double posX) {
        this.posX = posX;
    }

    public void setPosY(double posY) {
        this.posY = posY;
    }
    
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("\tNome: " + this.nome + "\n");
        s.append("\tEmail: " + this.email + "\n");
        s.append("\tPass : " + this.password + "\n");
        s.append("\tPos_x : " + this.posX + "\n");
        s.append("\tPos_y : " + this.posY + "\n");

        return s.toString();
    }

    public boolean equals(Object obj) {

        if (this == obj)
            return true;
        if ((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        Utilizador u = (Utilizador) obj;
        return this.nome.equals(u.getNome()) &&
                this.email.equals(u.getEmail()) &&
                this.password.equals(u.getPassword()) && 
                this.posX == u.getPosX() &&
                this.posY ==u.getPosY();
    }

    public abstract Utilizador clone();
}

