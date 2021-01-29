import java.io.Serializable;

/** Classe auxiliar que agrupa um determinado código a uma distância*/
public class StringDistAux implements Serializable {
    public double dist;
    public String cod;

    public double getDist() {
        return dist;
    }

    public void setDist(double dist) {
        this.dist = dist;
    }

    public String getCod() {
        return cod;
    }

    public void setCod(String cod) {
        this.cod = cod;
    }

    public StringDistAux(double dist, String cod) {
        this.dist = dist;
        this.cod = cod;
    }

    public StringDistAux( StringDistAux a) {
        this.dist = a.getDist();
        this.cod = a.getCod();
    }

    public StringDistAux clone(){
        return new StringDistAux(this);
    }

}
