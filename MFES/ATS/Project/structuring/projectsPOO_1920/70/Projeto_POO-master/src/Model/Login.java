package Model;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

public class Login implements Serializable{

    private Map<String,String> registers;

    /*  Model.Login Constructors
    * */
    public Login() {
        this.registers = new HashMap<>();
    }
    public Login(Login a) {
        if(a!=null) {
            this.registers = a.getRegisters();
            }
        }

    /*  Getters e Setters */
    public void setRegisters(Map<String, String> registers) {
        this.registers = registers;
    }
    public Map<String, String> getRegisters() {
        return registers;
    }

    /*  Ads a new register to the Map
    *   Returns true if the register is successfully added */
    public boolean addRegister(String email, String password){
        boolean return_value;
        if(this.registers.containsKey(email)){
            return_value = false;
        }else {
            this.registers.put(email,password);
            return_value = true;
        }
        return return_value;
    }

    /*  Checks if a password of a given email is correct
     *  by comparing the object of a given key */
    public boolean validatePassword(String email, String password){
        boolean result = false;
        if(this.registers.containsKey(email)){
            result = this.registers.get(email).equals(password);
        }
        return result;
    }

    /*  Changes the password of any existing email, given the old password
     *  by replacing an object of a given key
     * */
    public boolean changePassword(String email, String oldPassword, String newPassword){
        boolean return_value;
        if(this.registers.containsKey(email)){
            if(this.registers.get(email).equals(oldPassword)) {
                this.registers.replace(email, newPassword);
                return_value = true;
            }else {
                return_value = false;
            }
        }else {
            return_value = false;
        }
        return return_value;
    }

    /*  Deletes the register of any existing email, given the correct password
    *   by removing the element of the map equal to the given key and after comparing the object with the given one */
    public boolean deleteRegister(String email){
        boolean return_value;
        if(this.registers.containsKey(email)) {
            this.registers.remove(email);
            return_value = true;
        }
        else {
            return_value = false;
        }
        return return_value;
    }

    /* Checks the registers for the existance of a given key*/
    public boolean containsRegister(String email){
        return this.registers.containsKey(email);
    }
}

