import java.io.Serializable;
import java.text.DateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe controller que faz a comunicação com o model e a view
 */
public class Encomenda implements Serializable {
    private String codEncomenda;
    private boolean encMedica;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private LocalDateTime dataA;
    private LocalDateTime dataE;
    private LocalDateTime dataP;
    private LocalDateTime dataL;
    private LocalDateTime dataV;
    private LocalDateTime dataR;
    private Map<String,LinhaEncomenda> linha;
    private String estado;

    /**
     * Contrutor vazio
     */
    public Encomenda() {
        this.codEncomenda = "";
        this.encMedica = false;
        this.codUtilizador = "";
        this.codLoja = "";
        this.peso = 0;
        this.dataA = LocalDateTime.now();
        this.dataE = LocalDateTime.now();
        this.dataP = LocalDateTime.now();
        this.dataL = LocalDateTime.now();
        this.dataV = LocalDateTime.now();
        this.dataR = LocalDateTime.now();
        this.linha = new TreeMap<>();
        this.estado = "Pendente";
    }

    /**
     * Construtor com argumentos
     * @param codEncomenda Código da Encomenda
     * @param ec Flag Encomenda médica
     * @param codUtilizador Código do Utilizador
     * @param codLoja Código da Loja
     * @param peso Peso da Encomenda
     * @param linha Map linhas de encomenda
     * @param a Data de requirimento
     * @param t Data de entrega
     * @param p Data de preparação
     * @param v Data em viagem
     * @param l Data de prontidão
     * @param r Data de recolhimento
     * @param est Estado da encomenda
     */
    public Encomenda(String codEncomenda, boolean ec, String codUtilizador, String codLoja, double peso, TreeMap<String,LinhaEncomenda> linha, LocalDateTime a, LocalDateTime t, LocalDateTime p, LocalDateTime v, LocalDateTime l, LocalDateTime r, String est) {
        this.codEncomenda = codEncomenda;
        this.encMedica = ec;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        TreeMap<String, LinhaEncomenda> copy = new TreeMap<>();
        for (Map.Entry<String, LinhaEncomenda> entry : linha.entrySet()) {
            copy.put(entry.getKey(), entry.getValue());
        }
        this.linha = copy;
        this.dataA = a;
        this.dataE = t;
        this.dataP = p;
        this.dataL = l;
        this.dataV = v;
        this.dataR = r;
        this.estado = est;
    }

    /**
     * Contrutor com uma Encomenda
     * @param e Encomenda
     */
    public Encomenda(Encomenda e) {
        this.codEncomenda = e.codEncomenda;
        this.encMedica = e.encMedica;
        this.codUtilizador = e.codUtilizador;
        this.codLoja = e.codLoja;
        this.peso = e.peso;
        TreeMap<String, LinhaEncomenda> copy = new TreeMap<>();
        for (Map.Entry<String, LinhaEncomenda> entry : e.linha.entrySet()) {
            copy.put(entry.getKey(), entry.getValue());
        }
        this.linha = copy;
        this.dataA = e.dataA;
        this.dataE = e.dataE;
        this.dataP = e.dataP;
        this.dataL = e.dataL;
        this.dataV = e.dataV;
        this.dataR = e.dataR;
        this.estado = e.estado;
    }

    /**
     * Devolve o Código da Encomenda
     * @return Código da Encomenda
     */
    public String getCodEncomenda () {return this.codEncomenda;}

    /**
     * Devolve se a encomenda é médica
     * @return true se a encomenda é médica, false caso contrário
     */
    public boolean getEncMedica () {return this.encMedica;}

    /**
     * Devolve o Código do Utilizador
     * @return Código do Utilizador
     */
    public String getCodUtilizador () {return this.codUtilizador;}

    /**
     * Devolve o Código da Loja
     * @return Código da Loja
     */
    public String getCodLoja () {return this.codLoja;}

    /**
     * Devolve o peso
     * @return Peso
     */
    public double getPeso () {return this.peso;}

    /**
     * Devolve a Linha de encomenda
     * @return Map de Linha de encomenda
     */
    public TreeMap<String, LinhaEncomenda> getLinha () {
        TreeMap<String, LinhaEncomenda> copy = new TreeMap<>();
        for (Map.Entry<String, LinhaEncomenda> entry : this.linha.entrySet()) copy.put(entry.getKey(), entry.getValue());
        return copy;
    }

    /**
     * Devolve a data de requirimento
     * @return LocalDateTime Data de requirimento
     */
    public LocalDateTime getDataA () {return this.dataA;}

    /**
     * Devolve a data de entrega
     * @return LocalDateTime Data de entrega
     */
    public LocalDateTime getDataE () {return this.dataE;}

    /**
     * Devolve a data de preparação
     * @return LocalDateTime Data de preparação
     */
    public LocalDateTime getDataP () {return this.dataP;}

    /**
     * Devolve a data de prontidão
     * @return LocalDateTime Data de prontidão
     */
    public LocalDateTime getDataL () {return this.dataL;}

    /**
     * Devolve a data de viagem
     * @return LocalDateTime Data de viagem
     */
    public LocalDateTime getDataV () {return this.dataV;}

    /**
     * Devolve a data de recolhimento
     * @return LocalDateTime Data de recolhimento
     */
    public LocalDateTime getDataR () {return this.dataR;}

    /**
     * Devolve o estado
     * @return Estado
     */
    public String getEstado () {return this.estado;}

    /**
     * Introduz o Código da Encomenda
     * @param c Código da Encomenda
     */
    public void setCodEncomenda (String c) {this.codEncomenda = c;}

    /**
     * Introduz se a encomenda é médica
     * @param c boolean
     */
    public void setEncMedica (boolean c) {this.encMedica = c;}

    /**
     * Introduz o Código do Utilizador
     * @param c Código da Utilizador
     */
    public void setCodUtilizador (String c) {this.codUtilizador = c;}

    /**
     * Introduz o Código da Loja
     * @param c Código da Loja
     */
    public void setCodLoja (String c) {this.codLoja = c;}

    /**
     * Introduz o Peso
     * @param c Peso
     */
    public void setPeso (double c) {this.peso = c;}

    /**
     * Introduz a Linha de encomenda
     * @param linha Map de Linha de encomenda
     */
    public void setLinha (Map<String, LinhaEncomenda> linha) {
        Map<String, LinhaEncomenda> copy = new TreeMap<>();
        for (Map.Entry<String, LinhaEncomenda> entry : linha.entrySet()) copy.put(entry.getKey(), entry.getValue());
        this.linha = copy;
    }

    /**
     * Introduz a data de requirimento
     * @param c Data de requirimento
     */
    public void setDataA (LocalDateTime c) {this.dataA = c;}

    /**
     * Introduz a data de entrega
     * @param c Data de entrega
     */
    public void setDataE (LocalDateTime c) {this.dataE = c;}

    /**
     * Introduz a data de preparação
     * @param c Data de preparação
     */
    public void setDataP (LocalDateTime c) {this.dataP = c;}

    /**
     * Introduz a data de prontidão
     * @param c Data de prontidão
     */
    public void setDataL (LocalDateTime c) {this.dataL = c;}

    /**
     * Introduz a data de viagem
     * @param c Data de viagem
     */
    public void setDataV (LocalDateTime c) {this.dataV = c;}

    /**
     * Introduz a data de recolhimento
     * @param c Data de recolhimento
     */
    public void setDataR (LocalDateTime c) {this.dataR = c;}

    /**
     * Introduz o estado
     * @param c Estado
     */
    public void setEstado (String c) {this.estado = c;}

    /**
     * Método clone
     * @return Clone de uma Encomenda
     */
    public Encomenda clone () {
        return new Encomenda(this);
    }

    /**
     * Método equals
     * @param o Object
     * @return true se as Encomendas forem iguais ou false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda encomenda = (Encomenda) o;
        return encMedica == encomenda.encMedica &&
                Double.compare(encomenda.peso, peso) == 0 &&
                Objects.equals(codEncomenda, encomenda.codEncomenda) &&
                Objects.equals(codUtilizador, encomenda.codUtilizador) &&
                Objects.equals(codLoja, encomenda.codLoja) &&
                Objects.equals(linha, encomenda.linha) &&
                Objects.equals(dataA, encomenda.dataA) &&
                Objects.equals(dataE, encomenda.dataE) &&
                Objects.equals(dataP, encomenda.dataP) &&
                Objects.equals(dataL, encomenda.dataL) &&
                Objects.equals(dataV, encomenda.dataV) &&
                Objects.equals(dataR, encomenda.dataR) &&
                Objects.equals(estado, encomenda.estado);
    }

    /**
     * Método toString
     * @return String com informação relativa a uma Encomenda
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Encomenda: ").append(this.codEncomenda).append("\n")
                .append("Encomenda Médica: ").append(this.encMedica).append("\n")
                .append("Codigo Utilizador: ").append(this.codUtilizador).append("\n")
                .append("Codigo Loja: ").append(this.codLoja).append("\n")
                .append("Peso: ").append(this.peso).append("\n")
                .append("Linhas: ").append(this.linha).append("\n")
                .append("Estado: ").append(this.estado).append("\n");
        return sb.toString();
    }

    /**
     * Método que calcula o preço total de uma encomenda
     * @return Preço da ecomenda
     */
    public double calculaPrecoTotal () {
        return this.linha.values().stream().map(LinhaEncomenda::getPrecoLinha).reduce(0.0, Double::sum);
    }

    /**
     * Método que adiciona uma linha de encomenda a uma encomenda
     * @param l Linha da ecomenda
     */
    public void adicionaLinha (LinhaEncomenda l) {
        this.linha.put(l.getCodProd(),l);
    }

    /**
     * Método que altera o estado da encomenda para "Entregue"
     */
    public void aceitaEncomenda () {this.estado = "Entregue";}

    /**
     * Método que altera o estado da encomenda para "Cancelada"
     */
    public void rejeitaEncomenda () {this.estado = "Cancelada";}
}
