package Model;

import java.time.LocalDateTime;

public class RegistoEncomenda {

    private String codEncomenda;
    private String quemTransportou;
    private String quemEncomendou;
    private LocalDateTime dataEncomenda;
    private double distanciaPercorrida;
    private double valorTotalEncomenda;
    private double custoTotalTransporte;
    private Encomenda encomenda;

    public RegistoEncomenda() {
        this.codEncomenda = new String();
        this.quemTransportou = new String();
        this.quemEncomendou = new String();
        this.dataEncomenda = LocalDateTime.now();
        this.distanciaPercorrida = 0;
        this.valorTotalEncomenda = 0;
        this.custoTotalTransporte = 0;
        this.encomenda = new Encomenda();
    }

    public RegistoEncomenda(String codEncomenda, String quemTransportou, String quemEncomendou,
                            LocalDateTime dataEncomenda, double distanciaPercorrida, double valorTotalEncomenda,
                            double custoTotalTransporte, Encomenda e) {
        setCodEncomenda(codEncomenda);
        setQuemTransportou(quemTransportou);
        setQuemEncomendou(quemEncomendou);
        setDataEncomenda(dataEncomenda);
        setDistanciaPercorrida(distanciaPercorrida);
        setValorTotalEncomenda(valorTotalEncomenda);
        setCustoTotalTransporte(custoTotalTransporte);
        setEncomenda(e);
    }

    public RegistoEncomenda(RegistoEncomenda r) {
        setCodEncomenda(r.getCodEncomenda());
        setQuemTransportou(r.getQuemTransportou());
        setQuemEncomendou(r.getQuemEncomendou());
        setDataEncomenda(r.getDataEncomenda());
        setDistanciaPercorrida(r.getDistanciaPercorrida());
        setValorTotalEncomenda(r.getValorTotalEncomenda());
        setCustoTotalTransporte(r.getCustoTotalTransporte());
        setEncomenda(r.getEncomenda());
    }

    public String getCodEncomenda() {
        return this.codEncomenda;
    }

    public String getQuemTransportou() {
        return this.quemTransportou;
    }

    public String getQuemEncomendou() {
        return this.quemEncomendou;
    }

    public LocalDateTime getDataEncomenda() {
        return this.dataEncomenda;
    }

    public double getDistanciaPercorrida() {
        return this.distanciaPercorrida;
    }

    public double getValorTotalEncomenda() {
        return this.valorTotalEncomenda;
    }

    public double getCustoTotalTransporte() {
        return this.custoTotalTransporte;
    }

    public Encomenda getEncomenda() {
        return this.encomenda;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public void setQuemTransportou(String quemTransportou) {
        this.quemTransportou = quemTransportou;
    }

    public void setQuemEncomendou(String quemEncomendou) {
        this.quemEncomendou = quemEncomendou;
    }

    public void setDataEncomenda(LocalDateTime dataEncomenda) {
        this.dataEncomenda = dataEncomenda;
    }

    public void setDistanciaPercorrida(double distanciaPercorrida) {
        this.distanciaPercorrida = distanciaPercorrida;
    }

    public void setValorTotalEncomenda(double valorTotalEncomenda) {
        this.valorTotalEncomenda = valorTotalEncomenda;
    }

    public void setCustoTotalTransporte(double custoTotalTransporte) {
        this.custoTotalTransporte = custoTotalTransporte;
    }

    public void setEncomenda (Encomenda e){
        this.encomenda = e.clone();
    }

    public RegistoEncomenda clone() {
        return new RegistoEncomenda(this);
    }

    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        RegistoEncomenda r = (RegistoEncomenda) o;
        return ((this.quemTransportou == r.getQuemTransportou())
                && (this.quemEncomendou == r.getQuemEncomendou())
                && (this.dataEncomenda.equals(r.getDataEncomenda()))
                && (this.distanciaPercorrida == r.getDistanciaPercorrida())
                && (this.valorTotalEncomenda == r.getValorTotalEncomenda())
                && (this.custoTotalTransporte == r.getCustoTotalTransporte())
                && (this.encomenda.equals(r.getEncomenda())));
    }

    public String toString() {
        String formattedString = new String();
        StringBuilder sb = new StringBuilder();
        sb.append("Registo da Encomenda: ").append(codEncomenda).append("\n")
                .append("Transportada Por: ").append(quemTransportou).append("\n")
                .append("Encomendada Por: ").append(quemEncomendou).append("\n")
                .append("Data da Encomenda: ").append(dataEncomenda).append("\n")
                .append("Distância Percorrida Pelo Transportador: ").append(distanciaPercorrida).append("\n")
                .append("Custo Total da Encomenda: ").append(valorTotalEncomenda).append("\n")
                .append("Custo Total do Transporte: ").append(custoTotalTransporte).append("\n")
                .append("\n")
                .append("Código de Encomenda: ").append(encomenda);
        formattedString = sb.toString().replace(",","").replace("[","").
                replace("]","");
        return formattedString;
    }
}
