package Projeto;

public class NovoLogin {
    private String user;
    private String password;

    public NovoLogin(String user, String pass) {
        this.user = user;
        this.password = pass;
    }

    public String getUser() {
        return this.user;
    }

    public String getPassword() {
        return this.password;
    }

    public void definePassword(){
        if (password == null) this.password = "admin";
    }

}
