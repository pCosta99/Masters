import java.io.Serializable;

/**
 * Classe mais geral dos utilizadores da app TrazAqui.
 * Cada AppUser é definido pelo seu username e password.
 */
public abstract class AppUser implements Serializable {
    private String username;
    private String password;
    private static Sistema sistema;

    public static void setSistema(Sistema sistema) {
        AppUser.sistema = sistema;
    }

    public static Sistema getSistema() {
        return AppUser.sistema;
    }

    // Constructors

    public AppUser() {
        this.username = this.password = "";
    }

    public AppUser(String username, String password) {
        this.username = username;
        this.password = password;
    }


    public AppUser(Sistema sistema, String username, String password) {
        if (AppUser.sistema == null) setSistema(sistema);
        this.username = username;
        this.password = password;
    }

    public AppUser(AppUser o) {
        this.username = o.getUsername();
        this.password = o.getPassword();
    }

    // Getters e setters

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if ( o==null || !o.getClass().equals(this.getClass())) return false;

        AppUser u = (AppUser) o;
        return this.password.equals(u.getPassword()) &&
               this.username.equals(u.getUsername());
    }

    
    
}