import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Loja extends Utilizador implements Serializable {
    private List<Encomenda> hist_Loja;

    public Loja(String codUtilizador, String nome, double gpsx, double gpsy) {
        super(codUtilizador, nome, gpsx, gpsy);
        this.hist_Loja = new ArrayList<>();
    }

    public Loja(String codUtilizador, String nome, double gpsx, double gpsy, String password, List<Encomenda> hist_Loja) {
        super(codUtilizador, nome, gpsx, gpsy, password);
        this.hist_Loja = hist_Loja.stream().map(Encomenda::clone).collect(Collectors.toList());
    }


    private Loja(){
        super();
    }

    public Loja(Loja l) {
        super(l);
        this.hist_Loja = l.getHist_Loja();
    }

    public List<Encomenda> getHist_Loja() {
        List<Encomenda> hist = new ArrayList<>();
        if (this.hist_Loja != null){
            for(Encomenda e : this.hist_Loja){
                hist.add(e.clone());
            }
            return hist;
        }
        return hist;
    }

    public void setHist_Loja(List<Encomenda> hist_Loja) {
        this.hist_Loja = hist_Loja.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Loja ");
        sb.append(super.toString());
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) return true;
        if((o == null) || o.getClass() != this.getClass()) return false;
        Loja p = (Loja) o;
        return super.equals(o);
    }

    public Loja clone(){
        return new Loja(this);
    }


}
