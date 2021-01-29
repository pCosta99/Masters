import java.io.Serializable;
import java.util.ArrayList;

public class Loja extends UtilizadorSistema implements Serializable {
    private final double tempo_espera;
    private int nrPessoasEmFila;
    private ArrayList<Encomenda> encomendas_recebidas;

    public Loja (String email, String password, String codigo, String nome, double tempo_espera, double latitude, double longitude, ArrayList<Encomenda> encomendas_recebidas, int nrPessoasEmFila){
        super(email, password, "Loja", codigo, nome, latitude, longitude);
        this.tempo_espera = tempo_espera;
        this.nrPessoasEmFila = nrPessoasEmFila;
        setEncomendas_recebidas(encomendas_recebidas);
    }

    public Loja (Loja loja){
        super(loja);
        this.tempo_espera = loja.getTempo_espera();
        this.nrPessoasEmFila = loja.getNrPessoasEmFila();
        setEncomendas_recebidas(loja.getEncomendas_recebidas());
    }

    public String getCodigo(){
        return super.getCodigo();
    }

    public String getNome() {
        return super.getNome();
    }

    public double getTempo_espera() {
        return tempo_espera;
    }

    public int getNrPessoasEmFila() {
        return nrPessoasEmFila;
    }

    public double getLatitude() {
        return super.getLatitude();
    }

    public double getLongitude() {
        return super.getLatitude();
    }

    public ArrayList<Encomenda> getEncomendas_recebidas(){
        ArrayList<Encomenda> aux = new ArrayList<>();
        for(Encomenda s: this.encomendas_recebidas){
            aux.add(s.clone());
        }
        return aux;
    }

    public void setNrPessoasEmFila(int nrPessoasEmFila) {
        this.nrPessoasEmFila = nrPessoasEmFila;
    }

    public void setCodigo(String codigo){
      super.setCodigo(codigo);
    }

    public void setNome(String nome) {
        super.setNome(nome);
    }

    public void setLatitude(double latitude) {
        super.setLatitude(latitude);
    }

    public void setLongitude(double longitude) {
        super.setLongitude(longitude);
    }

    public void setEncomendas_recebidas(ArrayList<Encomenda> encomendas_recebidas) {
        this.encomendas_recebidas = new ArrayList<>();
        for(Encomenda s: encomendas_recebidas){
            this.encomendas_recebidas.add(s.clone());
        }
    }

    public Loja clone(){
        return new Loja(this);
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Loja Loja = (Loja) o;
        return  super.equals(o) &&
                this.tempo_espera == Loja.getTempo_espera() &&
                this.encomendas_recebidas.equals(Loja.encomendas_recebidas);
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer("Loja: ").append("\n");
        sb.append("Código da loja: ").append(getCodigo()).append('\n');
        sb.append("Nome da loja: ").append(getNome()).append('\n');
        sb.append("Tempo de espera: ").append(this.tempo_espera).append('\n');
        sb.append("Latitude: ").append(getLatitude()).append('\n');
        sb.append("Longitude: ").append(getLongitude()).append('\n');
        sb.append("Lista de encomendas recebidas: ");
        this.encomendas_recebidas.forEach(e -> sb.append(e.toString()).append("\n"));
        sb.append(super.toString());
        return sb.toString();
    }

    /**
     * Método que adiciona uma nova encomenda a uma loja
     */
    public void addEncomenda(Encomenda e){
        this.encomendas_recebidas.add(e.clone());
    }

    /**
     * Método que remove uma encomenda do stock
     */
    public void removeEncomenda(Encomenda e){
        this.encomendas_recebidas.remove(e);
    }

    /**
     * Método que devolve uma encomenda com o código cod
     */
    public Encomenda getEnc(String cod) throws EncomendaNotFoundException{
        for(Encomenda e: this.encomendas_recebidas){
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
        for(Encomenda e: this.encomendas_recebidas){
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
        for(Encomenda e: this.encomendas_recebidas){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setEncomendas_recebidas(aux);
    }

}
