import java.io.Serializable;
import java.time.LocalDate;

public class Viagem implements Serializable {
    private String       codTransportadora;
    private String       nomeTransportadora;
    private double       posInicialX, posInicialY;
    private double       posFinalX, posFinalY;
    private double       distancia;
    private double       custo;
    private LocalDate    data;
    private int          classificacao;

    public Viagem() {
        this.codTransportadora = "";
        this.nomeTransportadora = "";
        this.posInicialX = 0;
        this.posInicialY = 0;
        this.posFinalX = 0;
        this.posFinalY = 0;
        this.distancia = 0;
        this.custo = 0;
        this.data = LocalDate.MIN;
        this.classificacao = 0;
    }

    public Viagem(String codTransportadora, String nomeTransportadora, double posInicialX, double posInicialY, double posFinalX, double posFinalY, LocalDate data,
                  double distancia, double custo, int classificacao) {
        this.codTransportadora = codTransportadora;
        this.nomeTransportadora = nomeTransportadora;
        this.posInicialX = posInicialX;
        this.posInicialY = posInicialY;
        this.posFinalX = posFinalX;
        this.posFinalY = posFinalY;
        this.setDistancia(distancia);
        this.setCusto(custo);
        this.data = data;
        this.classificacao = classificacao;
    }

    public Viagem(Viagem v) {
        this.codTransportadora = v.getCodTransportadora();
        this.nomeTransportadora = v.getNomeTransportadora();
        this.posInicialX = v.getPosInicialX();
        this.posInicialY = v.getPosInicialY();
        this.posFinalX = v.getPosFinalX();
        this.posFinalY = v.getPosFinalY();
        this.distancia = v.getDistancia();
        this.custo = v.getCusto();
        this.data = v.getData();
        this.classificacao = v.getClassificacao();
    }
    
    public String getCodTransportadora(){
        return this.codTransportadora;
    }
    
    public String getNomeTransportadora(){
        return this.nomeTransportadora;
    }
    
    public double getPosInicialX() {
        return this.posInicialX;
    }

    public double getPosInicialY() {
        return this.posInicialY;
    }

    public double getPosFinalX() {
        return this.posFinalX;
    }

    public double getPosFinalY() {
        return this.posFinalY;
    }

    public double getDistancia() {
        return this.distancia;
    }

    public double getCusto() {
        return this.custo;
    }

    public LocalDate getData() {
        return this.data;
    }
    
    public void setCodTransportadora(String codTransportadora){
        this.codTransportadora = codTransportadora;
    }
    
    public void setNomeTransportadora(String nomeTransportadora){
        this.nomeTransportadora = nomeTransportadora;
    }
    
    public void setDistancia(double distancia) {
        this.distancia = distancia;
    }

    public void setCusto(double custo) {
        this.custo = custo;
    }

    public void setData(LocalDate data) {
        this.data = data;
    }

    public void setPosInicialX(int xi) {
        this.posInicialX = xi;
    }

    public void setPosInicialY(int yi) {
        this.posInicialY = yi;
    }

    public void setPosFinalX(int xf) {
        this.posFinalX = xf;
    }

    public void setPosFinalY(int yf) {
        this.posFinalY = yf;
    }

    public int getClassificacao() {
        return classificacao;
    }

    public void setClassificacao(int classificacao) {

        this.classificacao = classificacao;
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("Nome transportadora: " + this.nomeTransportadora);
        s.append("\nPosicao inicial: (" + this.posInicialX + "," + this.posInicialY + ")");
        s.append("\nPosicao final: (" + this.posFinalX + "," + this.posFinalY + ")");
        s.append("\nDistancia: " + this.distancia);
        s.append("\nCusto: " + this.custo);
        s.append("\nData: " + this.data);
        s.append("\nClassificacao: " + this.classificacao);
        return s.toString();
    }

    public boolean equals(Object obj) {

        if (this == obj)
            return true;
        if ((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        Viagem v = (Viagem) obj;
        return this.nomeTransportadora.equals(v.getNomeTransportadora()) &&
                this.posInicialX == v.getPosInicialX() &&
                this.posInicialY == v.getPosInicialY() &&
                this.posFinalX == v.getPosFinalX() &&
                this.posFinalY == v.getPosFinalY() &&
                this.distancia == v.getDistancia() &&
                this.custo == v.getCusto() &&
                this.data.equals(v.getData()) &&
                this.classificacao == v.getClassificacao();
    }

    public Viagem clone() {
        return new Viagem(this);
    }
}
