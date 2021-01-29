package Model.Catalogos;

import Model.Logins.ILogin;

import java.io.Serializable;
import java.util.HashMap;

/**
 * classe do catálogo de logins
 */
public class CatalogoLogins implements ICatalogoLogins, Serializable {
    private HashMap<String, ILogin> logins; // UserID + (Email + Password)

    public CatalogoLogins(){
        this.logins = new HashMap<>();
    }

    public HashMap<String, ILogin> getLogins() {
        return this.logins;
    }
    public void setLogins(HashMap<String, ILogin> logins) {
        this.logins = logins;
    }

    public void addLogin(ILogin log, String userID) {
        this.logins.put(userID,log);
    }

    public boolean existsLogin(ILogin log) {
        return this.logins.containsValue(log);
    }

    public String getCodigoID(String email){
        for( String codigo : this.logins.keySet()){
            if(this.logins.get(codigo).getEmail().equals(email)){
                return codigo;
            }
        }
        return null;
    }

    public ILogin getLog (String id){
        return this.logins.get(id);
    }
}
