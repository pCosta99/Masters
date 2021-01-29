import java.util.*;
import java.io.*;

public class Empresa extends Transporte{

    private float custo;

    public Empresa(){
        super();
        this.custo = 0;
    }

    public Empresa(String email, String nome, String password, float pos_x, float pos_y, double raio, boolean disp, float custo){
        super(email,nome,password,pos_x,pos_y,raio,disp);
        this.custo = custo;
    }

    public Empresa(Empresa e){
        super(e);
        this.custo = e.getCusto();
    }

    public float getCusto(){ return this.custo; }

    public void setCusto(float custo){ this.custo = custo; }

    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        if(super.equals(obj)==false) return false;
        Empresa e = (Empresa) obj;
        return (this.custo == e.getCusto());
    }

    public Empresa clone(){ return new Empresa(this); }

    public String toString() {
        return super.toString() +
                "custo=" + custo +
                '}';
    }
}
