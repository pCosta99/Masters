import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Encomenda implements Serializable {
    private String codigo;
    private String utilizador;
    private String loja;
    private String transportador;
    private LocalDateTime horaI;
    private LocalDateTime horaF;
    private double peso;
    private List<LinhaEncomenda> produtos;
    private List<String> propostas;
    private boolean medica;
    private boolean entregue;

    public Encomenda(){
        this.codigo = "N/A";
        this.utilizador = "N/A";
        this.loja = "N/A";
        this.transportador = "";
        this.horaI = LocalDateTime.now();
        this.horaF = LocalDateTime.now();
        this.peso = 0.0;
        this.produtos = new ArrayList<>();
        this.propostas = new ArrayList<>();
        this.medica = false;
        this.entregue = false;
    }

    public Encomenda(String codigo,String utilizador, String loja,String transportador ,LocalDateTime horaI,
                     LocalDateTime horaF, double peso, List<LinhaEncomenda> produtos, List<String> propostas, boolean medica, boolean entregue){
        this.codigo = codigo;
        this.utilizador = utilizador;
        this.loja = loja;
        this.transportador = transportador;
        this.horaI = horaI;
        this.horaF = horaF;
        this.peso = peso;
        setProdutos(produtos);
        setPropostas(propostas);
        this.medica = medica;
        this.entregue = entregue;
    }

    public Encomenda(Encomenda e){
        this.codigo = e.getCodigo();
        this.utilizador = e.getUtilizador();
        this.loja = e.getLoja();
        this.transportador = e.getTransportador();
        this.horaI = e.getHoraI();
        this.horaF = e.getHoraF();
        this.peso = e.getPeso();
        setProdutos(e.getProdutos());
        setPropostas(e.getPropostas());
        this.medica = e.getMedica();
        this.entregue =e.getEntregue();
    }

    public String getCodigo(){
        return this.codigo;
    }

    public String getUtilizador() {
        return this.utilizador;
    }

    public String getLoja() {
        return this.loja;
    }

    public String getTransportador() {
        return this.transportador;
    }

    public LocalDateTime getHoraI() {
        return this.horaI;
    }

    public LocalDateTime getHoraF() {
        return this.horaF;
    }

    public double getPeso() {
        return this.peso;
    }

    public List<LinhaEncomenda> getProdutos() {
        List<LinhaEncomenda> l = new ArrayList<>();
        this.produtos.stream().forEach(e->l.add(e.clone()));
        return l;
    }

    public List<String> getPropostas(){
        List<String> le = new ArrayList<>();
        this.propostas.stream().forEach(e->le.add(e));
        return le;
    }

    public boolean getMedica(){
        return this.medica;
    }

    public boolean getEntregue(){
        return this.entregue;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public void setUtilizador(String utilizador) {
        this.utilizador = utilizador;
    }

    public void setLoja(String loja) {
        this.loja = loja;
    }

    public void setTransportador(String transportador) {
        this.transportador = transportador;
    }

    public double getPreco(){
        double total = 0;
        for(LinhaEncomenda le: this.produtos){
            total = total + le.getPreco();
        }
        return total;
    }

    public void setHoraI(LocalDateTime horaI) {
        this.horaI = horaI;
    }

    public void setHoraF(LocalDateTime horaF) {
        this.horaF = horaF;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public void setProdutos(List<LinhaEncomenda> produtos) {
        this.produtos = new ArrayList<>();
        produtos.stream().forEach(e->this.produtos.add(e.clone()));
    }

    public void setPropostas(List<String> propostas){
        this.propostas = new ArrayList<>();
        propostas.stream().forEach(e->this.propostas.add(e));
    }

    public void adicionaLinha(LinhaEncomenda linha){
        this.produtos.add(linha.clone());
    }

    public void removeProduto(String codProd){
        for(LinhaEncomenda l: this.produtos){
            if(l.getReferencia().equals(codProd)){
                this.produtos.remove(l);
            }
        }
        /**
         * this.encomenda.removeIf(l -> l.getReferencia().equals(codProd));
         */
    }

    public void adicionaProposta(String username){
        this.propostas.add(username);
    }

    public void setMedica(boolean medica) {
        this.medica = medica;
    }

    public void setEntregue(boolean entregue) {
        this.entregue = entregue;
    }

    public Encomenda clone(){
        return new Encomenda(this);
    }

    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\nCodigo: ").append(this.codigo + "\n")
                .append("\nUtilizador: ").append(this.utilizador + "\n")
                .append("Loja: ").append(this.loja + "\n")
                .append("Transportador: ").append(this.transportador + "\n")
                .append("Hora do pedido: ").append(this.horaI + "\n")
                .append("Hora da entrega: ").append(this.horaF + "\n")
                .append("Peso : ").append(this.peso + "\n")
                .append("Produtos: ").append(this.produtos + "\n")
                .append("Propostas: ").append(this.propostas + "\n")
                .append("Contém material médico?: ").append(this.medica + "\n")
                .append("Já foi entregue?: ").append(this.entregue).append("\n");
        return sb.toString();
    }

    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Encomenda e = (Encomenda) o;
        return this.getCodigo().equals(e.getCodigo()) &&
                this.getUtilizador().equals(e.getUtilizador()) &&
                this.getLoja().equals(e.getLoja()) &&
                this.getTransportador().equals(e.getTransportador()) &&
                this.getHoraI().equals(e.getHoraI()) &&
                this.getHoraF().equals(e.getHoraF()) &&
                this.getPeso() == e.getPeso() &&
                this.getProdutos().equals(e.getProdutos()) &&
                this.getPropostas().equals(e.getPropostas()) &&
                this.getMedica() == e.getMedica() &&
                this.getEntregue() == e.getEntregue();
    }
}
