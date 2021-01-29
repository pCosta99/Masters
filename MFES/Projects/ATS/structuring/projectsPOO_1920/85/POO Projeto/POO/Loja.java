import java.util.ArrayList;
import java.util.List;

public class Loja extends Traz_Aqui_Comum implements LojaI{
    // variáveis de instância
    private List<String> fe; // fila de espera.

    /**
     * Construtores para objetos da classe Loja
     */
    public Loja() {
        super();
        this.fe = new ArrayList<>();
    }

    public Loja(String codigo, String nome, double gpsX, double gpsY, List<String> fe) {
        super(codigo, nome, gpsX, gpsY);
        this.fe = fe;
    }

    public Loja(String cod, String nome, double x, double y) {
        super(cod, nome, x, y);
    }

    public Loja(Loja l) {
        super(l);
        this.fe = l.getFe();
    }

    /**
     * Metodos gets e sets,
     * clone, equals e toString
     */
    public List<String> getFe(){
        return new ArrayList<>(this.fe);
    }

    public void setFe(List<String> les){
        this.fe = new ArrayList<>(les);
    }

    public Loja clone(){
        return new Loja(this);
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Loja loja = (Loja) o;
        return super.equals(loja) &&
                loja.getFe().equals(this.fe);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Loja\n")
                .append(super.toString());
        if(this.fe.size()>0)
            sb.append("Fila de espera\n");

        for(String e: this.fe)
            sb.append(e).append("\n");

        return sb.toString();
    }

    /**
     * Metodo que adiciona o peso à encomenda
     */
    public EncomendaI addPeso(EncomendaI e, double peso){
        e.setPeso(peso);
        return e;
    }

    /**
     * Metodo que retorna o numero de pessoas em lista de espera.
     */
    public int filaEspera(){
        return this.fe.size();
    }

    /**
     * Metodo que adiciona uma encomenda
     */
    public void addEncomenda(String e){
        this.fe.add(e);
    }

    /**
     * Metodo que remove uma encomenda da fila de espera da loja
     */
    public void encomendaLevantada(String e){
        this.fe.remove(e);
    }

    /**
     * Metodo que le de uma String os dados de uma loja
     * e atualiza o conteudo da loja
     */
    public void leTA(String cod, String[] p) {
        this.setCodigo(cod);
        this.setNome(p[1]);
        this.setGpsX(Double.parseDouble(p[2]));
        this.setGpsY(Double.parseDouble(p[3]));

        if(p.length > 4){
            String e;
            ArrayList<String> le = new ArrayList<>();
            for(int i = 4; i < p.length; i++){
                e = p[i];
                le.add(e);
            }
            this.setFe(le);
            le.clear();
        }
        else
            this.setFe(new ArrayList<>());
    }

    /**
     * Metodo que no final de ler tudo atualiza os dados
     * de cada encomenda da loja
     */
    /*public void atualizaEnc(Map<String, EncomendaI> le){
        EncomendaI e;

        for(EncomendaI te: this.fe){
            e = le.get(te.getCodEncomenda()).clone();
            this.fe.remove(te);
            this.fe.add(e);
        }
    }*/
}
