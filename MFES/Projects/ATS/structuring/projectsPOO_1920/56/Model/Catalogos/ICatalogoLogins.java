package Model.Catalogos;

import Model.Logins.ILogin;

import java.util.HashMap;

public interface ICatalogoLogins {

    HashMap<String, ILogin> getLogins();
    void setLogins(HashMap<String, ILogin> logins);
    void addLogin(ILogin log, String userID);
    boolean existsLogin(ILogin log);
    String getCodigoID(String email);
    ILogin getLog (String id);
}
