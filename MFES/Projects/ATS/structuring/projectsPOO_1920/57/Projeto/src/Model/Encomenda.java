/**
 * classe que representa uma encomenda
 */
package Model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Encomenda implements Serializable {
    private String encCode;
    private String userCode;
    private String transpCode;
    private String storeCode;
    private double weight;
    private boolean isMedic;
    private LocalDateTime data;
    private boolean aceiteLoja;
    private boolean entregue;
    private double preco;
    private double tempoEntrega;
    private boolean standBy;
    private List<LinhaEncomenda> linha;

    //--------------------------------------------------------------Construtores--------------------------------------------------------------------------\\

    public Encomenda() {
        this.encCode = "";
        this.userCode = "";
        this.transpCode = "";
        this.weight = 0d;
        this.storeCode = "";
        this.isMedic = false;
        this.data = LocalDateTime.now();
        this.preco = 0;
        this.aceiteLoja = false;
        this.entregue = false;
        this.tempoEntrega = 0;
        this.standBy = false;
        this.linha = new ArrayList<>();
    }

    public Encomenda(String encCode, String userCode, String transpCode, String storeCode, double weight, boolean isMedic, LocalDateTime data, boolean aceiteLoja, List<LinhaEncomenda> linha, boolean entregue, double tempo, boolean standBy) {
        this.encCode = encCode;
        this.userCode = userCode;
        this.transpCode = transpCode;
        this.storeCode = storeCode;
        this.weight = weight;
        this.isMedic = isMedic;
        this.aceiteLoja = aceiteLoja;
        this.entregue = entregue;
        this.tempoEntrega = tempo;
        this.data = data;
        this.standBy = standBy;
        setLinha(linha);
        this.preco = calculaPrice();
    }

    public Encomenda(Encomenda enc) {
        this.encCode = enc.getEncCode();
        this.userCode = enc.userCode;
        this.transpCode = enc.transpCode;
        this.weight = enc.weight;
        this.storeCode = enc.storeCode;
        this.isMedic = enc.isMedic;
        this.data = enc.getData();
        this.aceiteLoja = isAceiteLoja();
        this.entregue = enc.isEntregue();
        this.standBy = enc.isStandBy();
        setLinha(enc.getLinha());
        this.preco = enc.getPreco();
        this.tempoEntrega = enc.getTempoEntrega();
    }

     //--------------------------------------------------------------Getters/Setters--------------------------------------------------------------------------\\

    /**
     * devolve codigo encomenda
     *
     * @return codigo encomenda
     */
    public String getEncCode() {
        return encCode;
    }

    /**
     * Altera preço de encomenda
     * @param preco preço
     */
    public void setPreco(double preco) {
        this.preco += preco;
    }

    /**
     * Devolve preço
     * @return preço
     */
    public double getPreco() {
        return this.preco;
    }

    /**
     * altera encCode
     *
     * @param encCode encCode
     */
    public void setEncCode(String encCode) {
        this.encCode = encCode;
    }

    /**
     * devolve user code
     *
     * @return user code
     */
    public String getUserCode() {
        return userCode;
    }

    /**
     * devolve transp code
     *
     * @return transp code
     */
    public String getTranspCode() {
        return transpCode;
    }

    /**
     * altera transp code
     *
     * @param transpCode Codigo de transporte
     */
    public void setTranspCode(String transpCode) {
        this.transpCode = transpCode;
    }

    /**
     * devolve store code
     *
     * @return store code
     */
    public String getStoreCode() {
        return storeCode;
    }

    /**
     * devolve peso
     *
     * @return peso
     */
    public double getWeight() {
        return weight;
    }

    /**
     *devolve isMedic
     *
     * @return isMedic
     */
    public boolean isMedic() {
        return isMedic;
    }

    /**
     * devolve data
     *
     * @return data
     */
    public LocalDateTime getData() {
        return data;
    }

    /**
     * devolve aceite
     *
     * @return aceite
     */
    public boolean isAceiteLoja() {
        return aceiteLoja;
    }

    /**
     * altera aceite
     *
     * @param aceiteLoja aceite
     */
    public void setAceiteLoja(boolean aceiteLoja) {
        this.aceiteLoja = aceiteLoja;
    }

    /**
     * devolve entregue
     *
     * @return entregue
     */
    public boolean isEntregue() {
        return entregue;
    }

    /**
     * altera entregue
     *
     * @param entregue entregue
     */
    public void setEntregue(boolean entregue) {
        this.entregue = entregue;
    }

    /**
     * altera tempo entrega
     *
     * @param tempoEntrega tempo de entrega
     */
    public void setTempoEntrega(double tempoEntrega) {
        this.tempoEntrega = tempoEntrega;
    }

    /**
     * devolve tempo entrega
     *
     * @return entrega
     */
    public double getTempoEntrega() {
        return tempoEntrega;
    }

    /**
     * verifica se é voluntario
     *
     * @return true se for voluntario
     */
    public boolean isVoluntario() {
        return transpCode.charAt(0) == 'v';
    }

    /**
     * verifica se é transportadora
     *
     * @return true se for transportadora
     */
    public boolean isTransportadora() {
        return transpCode.charAt(0) == 't';
    }

    /**
     * devolve linhas de encomenda
     *
     * @return list de LinhaEncomenda
     */
    public List<LinhaEncomenda> getLinha() {
        List<LinhaEncomenda> line = new ArrayList<>();

        for(LinhaEncomenda l: this.linha)
            line.add(l.clone());

        return line;
    }

    /**
     * altera lista de linhas de encomenda
     *
     * @param line lista de linha de encomenda
     */
    public void setLinha(List<LinhaEncomenda> line) {
        this.linha = new ArrayList<>();

        for (LinhaEncomenda l: line)
            this.linha.add(l.clone());

    }

    /**
     * Verifica se uma encomenda está ou não em StandBy
     *
     * @return Booleano com o informação sonre StandBy
     */
    public boolean isStandBy() {
        return standBy;
    }

    /**
     * Método que altera o booleano standBy
     *
     * @param standBy   novo valor do booleano standBy
     */
    public void setStandBy(boolean standBy) {
        this.standBy = standBy;
    }

    //--------------------------------------------------------------toString, equals e clone--------------------------------------------------------------------------\\


    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("(Encomenda ");
        sb.append(encCode);
        sb.append(")\nuserCode: ").append(userCode);
        sb.append(" | transpCode: ").append(transpCode);
        sb.append(" | storeCode: ").append(storeCode);
        sb.append("\nweight: ").append(weight);
        sb.append(" | isMedic: ").append(isMedic);
        sb.append("\nData: ").append(data.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")));
        sb.append(" | aceite: ").append(aceiteLoja);
        sb.append("\nPreço encomenda: ").append(String.format("%.2f", preco));
        sb.append("€\nTempo de entrega: ").append(String.format("%.2f", tempoEntrega)).append(" min\n");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Encomenda encomenda = (Encomenda) o;
        return Double.compare(encomenda.weight, weight) == 0 &&
                isMedic == encomenda.isMedic &&
                aceiteLoja == encomenda.aceiteLoja &&
                Objects.equals(encCode, encomenda.encCode) &&
                Objects.equals(userCode, encomenda.userCode) &&
                Objects.equals(transpCode, encomenda.transpCode) &&
                Objects.equals(storeCode, encomenda.storeCode) &&
                Objects.equals(data, encomenda.data) &&
                Objects.equals(linha, encomenda.linha);
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

    //--------------------------------------------------------------Outros métodos--------------------------------------------------------------------------\\

    public void addLinhaEncomenda(String productCode, String description, double quantity, double unitPrice) {
        this.linha.add(new LinhaEncomenda(productCode, description, quantity, unitPrice));
    }

    public void addLinhaEncomenda(LinhaEncomenda l) {
        this.linha.add(l.clone());
    }

    /**
     * verifica se a encomenda está entre as datas dadas
     *
     * @param min data min
     * @param max data max
     * @return    true se menor que max e maior que min
     */
    public boolean encData(LocalDateTime min, LocalDateTime max) {
        return data.isBefore(max) && data.isAfter(min);
    }

    /**
     * devolve o preço da encomenda
     *
     * @return preço
     */
    public double calculaPrice() {
        double price = 0d;

        for(LinhaEncomenda l: linha)
            price += l.getPrice();

        return price;
    }
}
