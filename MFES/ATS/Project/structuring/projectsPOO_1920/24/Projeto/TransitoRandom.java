import java.io.Serializable;
import java.util.Random;

public class TransitoRandom implements Serializable {

    private String[] tipostransito = {"limpo","moderado", "congestionado"};
    private String transito;

    public TransitoRandom(){
        Random generator = new Random();
        this.transito = this.tipostransito[generator.nextInt(3)];
    }

    public String getTransito(){
        return this.transito;
    }

    public void setTransito(String tr){
        this.transito=tr;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TransitoRandom traffic = (TransitoRandom) o;
        return this.transito.equals(traffic.getTransito());
    }

    public TransitoRandom clone(){
        TransitoRandom traffic = new TransitoRandom();
        traffic.setTransito(this.getTransito());
        return traffic;
    }

    public String toString(){
        return "O Transito encontra-se "+this.getTransito();
    }
}