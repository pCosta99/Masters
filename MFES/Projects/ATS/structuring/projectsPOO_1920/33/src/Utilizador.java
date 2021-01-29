import java.io.Serializable;
import java.nio.MappedByteBuffer;
import java.util.HashMap;
import java.util.Map;

public class Utilizador extends Pessoa implements Serializable {

    private int nrEncomendas;

    public Utilizador() {
        super();
        this.nrEncomendas = 0;
    }

    public Utilizador(String codigo, String nome, double gpsx, double gpsy, String email, String password, int nrEncomendas) {
        super(codigo,nome,gpsx,gpsy,email,password);
        this.nrEncomendas = nrEncomendas;
    }

    public Utilizador(Utilizador user){
        super(user);
        this.nrEncomendas = user.getNrEncomendas();
    }


    public void addEncomenda(){
        this.nrEncomendas++;
    }

    public int getNrEncomendas() {
        return this.nrEncomendas;
    }

    public void setNrEncomendas(int nrEncomendas) {
        this.nrEncomendas = nrEncomendas;
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }


    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador{\n")
             .append(super.toString())
             .append(", Número de encomendas: ").append(this.nrEncomendas)
             .append("\n}\n");
        return sb.toString();
    }

    public boolean equals(Object obj) {
        return super.equals(obj);
    }
}
