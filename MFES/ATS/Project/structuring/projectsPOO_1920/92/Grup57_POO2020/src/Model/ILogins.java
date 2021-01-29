package Model;

public interface ILogins {

    void put(String username, String pw);

    boolean checkUserName(String username);

    boolean checkCredentials(String username, String pw);

    String extractPassWord(String userName);

    void changePassWord(String userName, String pw);
}
