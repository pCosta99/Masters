package Model.Logins;

public interface ILogin {
    void setLogin(String id);
    void setEmail(String email);
    void setPassword(String password);

    String getEmail();
    String getPassword();

    String toString();
}
