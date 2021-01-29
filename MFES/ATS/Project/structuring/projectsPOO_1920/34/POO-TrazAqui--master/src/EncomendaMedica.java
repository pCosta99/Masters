import java.util.ArrayList;
import java.io.Serializable;

public class EncomendaMedica extends Encomenda implements Serializable{




    public EncomendaMedica() {
        super();
    }

    public EncomendaMedica(String codigoEncomenda, String codigoUtilizador, String codigoLoja, double peso, ArrayList<LinhaEncomenda> linhas) {
        super(codigoEncomenda, codigoUtilizador, codigoLoja, peso, linhas);
    }

    public EncomendaMedica(Encomenda e) {
        super(e);
    }

    public boolean equals(Object o){
        if(o==this) return true;
        if (o==null||o.getClass()!=this.getClass())return false;
        EncomendaMedica v= (EncomendaMedica) o;
        return super.equals(v);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        return sb.toString();
    }

    public EncomendaMedica clone(){
        return new EncomendaMedica(this);
    }
}
