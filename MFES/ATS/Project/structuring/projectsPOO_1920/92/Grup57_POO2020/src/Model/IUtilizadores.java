package Model;

import java.util.List;

public interface IUtilizadores {

    void put(Utilizador u);

    void put(String cod, Utilizador u);

    Double extractXByUserName(String userName);

    Double extractYByUserName(String userName);

    String extractNameByUserName(String userName);

    String extractEmailByUserName(String userName);

    void changeName(String userName, String nome);

    void changeGPS(String userName, Double x, Double y);

    void changeEmail(String userName, String email);

    List<String> exportUsers();
}
