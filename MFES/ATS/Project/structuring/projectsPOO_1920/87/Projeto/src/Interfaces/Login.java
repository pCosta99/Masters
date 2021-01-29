package Interfaces;

import java.io.Serializable;

public interface Login extends Serializable {

    boolean checkLogin(String code, String pass);
}
