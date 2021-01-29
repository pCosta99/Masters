package app.models;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import app.interfaces.IEncomenda;

public class Encomenda implements IEncomenda, Serializable {

    /**
    *
    */
    private static final long serialVersionUID = 6498352019616112228L;
    // #region variables
    private String codEnc;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private LocalDateTime dataCriacao;
    private List<LinhaEncomenda> linhasEncomenda;
    // #endregion

    // #region Construtores

    public Encomenda() {
        this.codEnc = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.peso = 0;
        this.dataCriacao = LocalDateTime.now();
        this.linhasEncomenda = new ArrayList<>();
    }

    /**
     * @param codEnc
     * @param codUtilizador
     * @param codLoja
     * @param peso
     * @param dataCriacao
     * @param linhasEncomenda
     */
    public Encomenda(String codEnc, String codUtilizador, String codLoja, double peso,
            LocalDateTime dataCriacao, List<LinhaEncomenda> linhasEncomenda) {
        this.codEnc = codEnc;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.dataCriacao = dataCriacao;
        this.setLinhasEncomenda(linhasEncomenda);
    }

    /**
     * @param e
     */
    public Encomenda(Encomenda e) {
        this.codEnc = e.codEnc;
        this.codUtilizador = e.codUtilizador;
        this.codLoja = e.codLoja;
        this.peso = e.peso;
        this.dataCriacao = e.dataCriacao;
        this.setLinhasEncomenda(e.linhasEncomenda);
    }

    // #endregion

    // #region getters setter
    /**
     * @return the peso
     */
    public double getPeso() {
        return peso;
    }

    /**
     * @return the linhasEncomenda
     */
    public List<LinhaEncomenda> getLinhasEncomenda() {
        return this.linhasEncomenda.stream().map(LinhaEncomenda::clone)
                .collect(Collectors.toList());
    }

    /**
     * @return the codLoja
     */
    public String getCodLoja() {
        return codLoja;
    }

    /**
     * @param codLoja the codLoja to set
     */
    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    /**
     * @return the codUtilizador
     */
    public String getCodUtilizador() {
        return codUtilizador;
    }

    /**
     * @param codUtilizador the codUtilizador to set
     */
    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    /**
     * @return the codEnc
     */
    public String getCodEnc() {
        return codEnc;
    }

    /**
     * @param codEnc the codEnc to set
     */
    public void setCodEnc(String codEnc) {
        this.codEnc = codEnc;
    }

    /**
     * @param linhasEncomenda the linhasEncomenda to set
     */
    public void setLinhasEncomenda(List<LinhaEncomenda> linhasEncomenda) {
        this.linhasEncomenda =
                linhasEncomenda.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());
    }

    /**
     * @return the dataCriacao
     */
    public LocalDateTime getDataCriacao() {
        return dataCriacao;
    }

    /**
     * @param dataCriacao the dataCriacao to set
     */
    public void setDataCriacao(LocalDateTime dataCriacao) {
        this.dataCriacao = dataCriacao;
    }

    /**
     * @param peso the peso to set
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }
    // #endregion

    // #region Overrrides
    @Override
    public boolean equals(Object o) {
        // self check
        if (this == o) {
            return true;
        }
        // null check
        if (o == null) {
            return false;
        }
        // type check and cast
        if (getClass() != o.getClass()) {
            return false;
        }
        Encomenda encomenda = (Encomenda) o;
        // field comparison
        return Objects.equals(this.codEnc, encomenda.codEnc)
                && Objects.equals(this.codUtilizador, encomenda.codUtilizador)
                && Objects.equals(this.codLoja, encomenda.codLoja)
                && Objects.equals(this.peso, encomenda.peso)
                && Objects.equals(this.dataCriacao, encomenda.dataCriacao)
                && Objects.equals(this.linhasEncomenda, encomenda.linhasEncomenda);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código de Encomenda: ");
        sb.append(this.codEnc);
        sb.append('\n');
        sb.append("Código de Utilizador: ");
        sb.append(this.codUtilizador);
        sb.append('\n');
        sb.append("Código de Loja: ");
        sb.append(this.codLoja);
        sb.append('\n');
        sb.append("Peso: ");
        sb.append(this.peso);
        sb.append('\n');
        sb.append("Data: ");
        sb.append(this.dataCriacao);
        sb.append('\n');
        sb.append("LinhaEncomendas:");
        sb.append('\n');
        for (LinhaEncomenda linhaencomenda : this.linhasEncomenda) {
            sb.append("LinhaEncomenda: ");
            sb.append('\n');
            sb.append(linhaencomenda.toString());
        }
        return sb.toString();
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

    public int compareTo(Encomenda e) {
        return this.dataCriacao.compareTo(e.dataCriacao);
    }
    // #endregion

    // #region Methods
    // #endregion

}
