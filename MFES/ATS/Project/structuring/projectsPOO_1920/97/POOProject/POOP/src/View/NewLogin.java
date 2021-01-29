package View;

public class NewLogin {
    private String User;
    private String Password;

    public NewLogin(String user, String password) {
        User = user;
        Password = password;
    }

    public String getUser() {
        return this.User;
    }

    public String getPassword() {
        return this.Password;
    }
}