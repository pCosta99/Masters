import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Encomenda implements Serializable {
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private LinhaEncomenda linha;
    private LocalDate data;
    private String codTransport;
    private int estado;


    public static final int ACEITETRANSPORTADORA = 5;
    public static final int REJEITADO = 0;
    public static final int PENDENTE = 1;
    public static final int ACEITE = 2;
    public static final int FINALIZADA = 3; // nao sei se necessario
    public static  final int PARAENTREGA = 4;

    public Encomenda(){
        this.codEncomenda = "";
        this.codUtilizador = "";
        this.codTransport = "";
        this.codLoja = "";
        this.peso = 0;
        this.linha = new LinhaEncomenda();
        this.data = LocalDate.now();
        this.estado = PENDENTE;
    }

    public Encomenda(String codEncomenda, String codUtilizador, String codLoja,
                     double peso, LinhaEncomenda linhasEnc, LocalDate data, int e,String cod){
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.linha =linhasEnc.clone();
        this.data = data;
        this.estado = e;
        this.codTransport = cod;
    }

    public Encomenda(Encomenda e){
        this.codEncomenda = e.getCodEncomenda();
        this.codUtilizador = e.getCodUtilizador();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.linha = e.getLinha().clone();
        this.data = e.getData();
        this.estado = e.getEstado();
        this.codTransport =e.getCodTransporte();
    }

    public String getCodTransporte(){
        return this.codTransport;
    }
    public void setCodTransport(String s){
        this.codTransport=s;
    }
    public LinhaEncomenda getLinha() {
        return this.linha.clone();
    }


    public List<Produto> getProdutos() {
        return this.linha.getProdutos().stream().map(Produto::clone).collect(Collectors.toList());
    }



    public void setEncomendas(LinhaEncomenda linhasEnc) {
        this.linha = linhasEnc.clone();
    }

    public String getCodEncomenda() {
        return this.codEncomenda;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getCodLoja() {
        return this.codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public double getPeso() {
        return this.peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    /* adiciona uma linha de encomenda
    public void adicionaLinha(LinhaEncomenda linha) {
        this.linhas.add(linha.clone());
    }*/

    public LocalDate getData() {
        return this.data;
    }

    public void setData(LocalDate data) {
        this.data = data;
    }

    public int getEstado(){
        return this.estado;
    }

    public void setEstado(int e){
        this.estado = e;
    }

    public Encomenda clone(){
        return new Encomenda(this);
    }
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda encomenda = (Encomenda) o;
        return Double.compare(encomenda.peso, peso) == 0 &&
                Objects.equals(codEncomenda, encomenda.codEncomenda) &&
                Objects.equals(codUtilizador, encomenda.codUtilizador) &&
                Objects.equals(codLoja, encomenda.codLoja) &&
                Objects.equals(linha, encomenda.linha) &&
                Objects.equals(data, encomenda.data)&&
                Objects.equals(estado,encomenda.estado)&&
                Objects.equals(codTransport,encomenda.codTransport);
    }

    @Override
    public int hashCode() {
        return Objects.hash(codEncomenda, codUtilizador, codLoja, peso, linha, data);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda{\nCódigo encomenda: ").append(this.codEncomenda)
          .append(", Data da encomenda: ").append(this.data)
          .append(", Código utilizador: ").append(this.codUtilizador)
          .append(", Código loja: ").append(this.codLoja)
          .append(", Peso: ").append(this.peso).append("\n")
          .append("Produtos encomendados: ").append(this.linha.toString()).append("\n")
          .append("Estado:" ).append(this.estado).append("\n")
          .append("Codigo Transporte: " ).append(this.codTransport);
        return sb.toString();
    }
}

