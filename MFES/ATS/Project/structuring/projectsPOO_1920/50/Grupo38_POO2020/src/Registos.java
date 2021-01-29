import java.io.Serializable;
import java.time.LocalDateTime;

public class Registos implements Serializable {
    private Encomenda enc;
    private LocalDateTime data;
    private String user;
    private String driver;
    private Double tmp;
    private Double custo;
    private Double custoT;

    public Encomenda getEnc() {
        return enc;
    }

    public void setEnc(Encomenda enc) {
        this.enc = enc;
    }

    public LocalDateTime getData() {
        return this.data;
    }

    public void setData(LocalDateTime data) {
        this.data = data;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getDriver() {
        return driver;
    }

    public void setDriver(String driver) {
        this.driver = driver;
    }

    public Double getTmp() {
        return tmp;
    }

    public Double getCusto() {
        return custo;
    }

    public void setCusto(Double custo) {
        this.custo = custo;
    }

    public void setTmp(Double tmp) {
        this.tmp = tmp;
    }

    public Double getCustoT() {
        return this.custoT;
    }

    public void setCustoT(Double custoT) {
        this.custoT = custoT;
    }

    public Registos(Encomenda enc, LocalDateTime data, String user, String driver, Double tmp, Double custo, Double custoT) {
        this.enc = enc;
        this.data = data;
        this.user = user;
        this.driver = driver;
        this.tmp = tmp;
        this.custo = custo;
        this.custoT = custoT;

    }

    public Registos(Registos b) {
        this.enc = b.getEnc();
        this.data = b.getData();
        this.user = b.getUser();
        this.driver = b.getDriver();
        this.tmp = b.getTmp();
        this.custo = b.getCusto();
        this.custoT = b.getCustoT();
    }

    public Registos clone(){
        return new Registos(this);
    }

    /** Método que retorna o código da loja a partir da encomenda do registo*/
    public String getLoja (){
        return this.enc.getLoja();
    }

    /** Método que retorna o peso da encomenda do registo*/
    public double getPeso (){
        return this.enc.getPeso();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Registo: ").append(this.enc).append(", ")
                .append(this.data).append(", ")
                .append(this.user).append(", ")
                .append(this.driver).append(", ")
                .append(this.tmp).append(", ")
                .append(this.custo).append(", ")
                .append(this.custoT);
        return sb.toString();
    }

}
