import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Loja extends UtilizadorSistema implements Serializable {
    private final double tempoEspera;
    private int nrPessoasEmFila;
    private List<Encomenda> encomendasRecebidas;

    public Loja (String email, String password, String codigo, String nome, double tempoEspera, double latitude, double longitude, ArrayList<Encomenda> encomendasRecebidas, int nrPessoasEmFila){
        super(email, password, "Loja", codigo, nome, latitude, longitude);
        this.tempoEspera = tempoEspera;
        this.nrPessoasEmFila = nrPessoasEmFila;
        setEncomendasRecebidas(encomendasRecebidas);
    }

    public Loja (Loja loja){
        super(loja);
        this.tempoEspera = loja.getTempoEspera();
        this.nrPessoasEmFila = loja.getNrPessoasEmFila();
        setEncomendasRecebidas(loja.getEncomendasRecebidas());
    }

    public double getTempoEspera() {
        return tempoEspera;
    }

    public int getNrPessoasEmFila() {
        return nrPessoasEmFila;
    }

    public List<Encomenda> getEncomendasRecebidas(){
        ArrayList<Encomenda> aux = new ArrayList<>();
        for(Encomenda s: this.encomendasRecebidas){
            aux.add(s.clone());
        }
        return aux;
    }

    public void setNrPessoasEmFila(int nrPessoasEmFila) {
        this.nrPessoasEmFila = nrPessoasEmFila;
    }

    public void setEncomendasRecebidas(List<Encomenda> encomendasRecebidas) {
        this.encomendasRecebidas = new ArrayList<>();
        for(Encomenda s: encomendasRecebidas){
            this.encomendasRecebidas.add(s.clone());
        }
    }

    public Loja clone(){
        return new Loja(this);
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Loja loja = (Loja) o;
        return  super.equals(o) &&
                this.tempoEspera == loja.getTempoEspera() &&
                this.encomendasRecebidas.equals(loja.encomendasRecebidas);
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("Loja: ").append("\n");
        sb.append("Código da loja: ").append(getCodigo()).append('\n');
        sb.append("Nome da loja: ").append(getNome()).append('\n');
        sb.append("Tempo de espera: ").append(this.tempoEspera).append('\n');
        sb.append("Latitude: ").append(getLatitude()).append('\n');
        sb.append("Longitude: ").append(getLongitude()).append('\n');
        sb.append("Lista de encomendas recebidas: ");
        this.encomendasRecebidas.forEach(e -> sb.append(e.toString()).append("\n"));
        sb.append(super.toString());
        return sb.toString();
    }

    /**
     * Método que adiciona uma nova encomenda a uma loja
     */
    public void addEncomenda(Encomenda e){
        this.encomendasRecebidas.add(e.clone());
    }

    /**
     * Método que remove uma encomenda do stock
     */
    public void removeEncomenda(Encomenda e){
        this.encomendasRecebidas.remove(e);
    }

    /**
     * Método que devolve uma encomenda com o código cod
     */
    public Encomenda getEnc(String cod) throws EncomendaNotFoundException{
        for(Encomenda e: this.encomendasRecebidas){
            if(cod.equals(e.getCodigo())) return e.clone();
        }
        throw new EncomendaNotFoundException();
    }

    /**
     * Método que devolve todas as encomendas que estão por preparar
     */
    public String getEncNotReady(){
        StringBuilder sb = new StringBuilder();
        int count = 0;
        for(Encomenda e: this.encomendasRecebidas){
            if(!e.isPreparada()){
                sb.append(e);
                count++;
            }
        }
        if(count == 0) sb.append("0");
        return sb.toString();
    }


    public void updateEncomenda(Encomenda enc){
        ArrayList<Encomenda> aux = new ArrayList<>();
        enc.setPreparada(true);
        aux.add(enc);
        for(Encomenda e: this.encomendasRecebidas){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setEncomendasRecebidas(aux);
    }

}
