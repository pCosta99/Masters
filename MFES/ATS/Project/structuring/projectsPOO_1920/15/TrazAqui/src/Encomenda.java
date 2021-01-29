import java.io.Serializable;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;


public class Encomenda implements Serializable {
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private List<LinhaEncomenda> linhaEncomendas;
    private boolean userAceitaPreco;
    private String userEntrega;
    private boolean porClassificar;
    private LocalDate data;



    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso, List<LinhaEncomenda> linhaEncomendas) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.linhaEncomendas = linhaEncomendas;
        this.userAceitaPreco = false;
        this.userEntrega = "nao definido";
        this.porClassificar = true;
        this.data = LocalDate.now();
    }

    public Encomenda(Encomenda e) {
        this.codEncomenda = e.getCodEncomenda();
        this.codUtilizador = e.getCodUtilizador();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.linhaEncomendas = e.getLinhaEncomendas();
        this.userAceitaPreco = e.getUserAceitaPreco();
        this.userEntrega = e.getUserEntrega();
        this.porClassificar = e.getPorClassificar();
        this.data = e.getData();
    }

    private Encomenda(){
    }

    public LocalDate getData() {
        return data;
    }

    public void setData(LocalDate data) {
        this.data = data;
    }

    public boolean getPorClassificar() {
        return porClassificar;
    }

    public void setPorClassificar(boolean porClassificar) {
        this.porClassificar = porClassificar;
    }

    public String getCodEncomenda() {
        return codEncomenda;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public String getCodUtilizador() {
        return codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getCodLoja() {
        return codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public double getPeso() {
        return peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public List<LinhaEncomenda> getLinhaEncomendas() {
        return this.linhaEncomendas.stream().map(LinhaEncomenda:: clone).collect(Collectors.toList());
    }

    public void setLinhaEncomendas(List<LinhaEncomenda> linhaEncomendas) {
        this.linhaEncomendas = linhaEncomendas.stream().map(LinhaEncomenda:: clone).collect(Collectors.toList());
    }


    public String getUserEntrega(){
        return userEntrega;
    }

    public void setUserEntrega(String userEntrega){
        this.userEntrega = userEntrega;
    }

    public boolean getUserAceitaPreco(){
        return userAceitaPreco;
    }

    public void setUserAceitaPreco(boolean userAceitaPreco){
        this.userAceitaPreco = userAceitaPreco;
    }


    @Override
    public boolean equals(Object o) {
        if(this == o) return true;
        if((o == null) || o.getClass() != this.getClass()) return false;
        Encomenda p = (Encomenda) o;
        return(p.getCodEncomenda().equals(this.codEncomenda) && p.getCodUtilizador().equals(this.codUtilizador) && p.getCodLoja().equals(this.codLoja)
        && p.getPeso() == this.peso && getUserEntrega().equals(this.userEntrega));
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Encomenda: ");
        sb.append("CodEncomenda: ").append(this.codEncomenda).append(", ");
        sb.append("UserID: ").append(this.codUtilizador).append(", ");
        sb.append("CodLoja: ").append(this.codLoja).append(", ");
        sb.append("Peso: ").append(this.peso).append(", ");
        sb.append("Produtos: ").append(this.linhaEncomendas.toString() + "\n");
        return sb.toString();
    }

    public Encomenda clone(){
        return new Encomenda(this);
    }


}
