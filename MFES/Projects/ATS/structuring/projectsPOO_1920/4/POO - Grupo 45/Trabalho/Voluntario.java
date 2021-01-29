
import java.util.*;
import java.io.*;

public class Voluntario extends Transporte {

    public Voluntario(){
        super();
    }

    public Voluntario(String email, String nome, String password, float pos_x, float pos_y, double raio, boolean disp){
        super(email,nome,password,pos_x,pos_y,raio, disp);
    }

    public Voluntario(Voluntario v){
        super(v);
    }

    public boolean equals(Object obj){
        if(this == obj) return true;
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        return super.equals(obj);
    }

    public Voluntario clone(){ return new Voluntario(this); }

    public String toString() { return super.toString(); }
}