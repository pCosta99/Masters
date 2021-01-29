import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Classe que corresponde a uma encomenda. Contem os metodos necessarios para fazer a gestao das encomendas.
 */
public class Encomenda implements Serializable {
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private List<LinhaEncomenda> encomendas;
    private String codTrans;
    private boolean entregue;
    private LocalDateTime levantamento;
    private LocalDateTime entrega;
    private boolean encomendaMedica;

    /**
     * Construtor não parametrizado da classe.
     */
    public Encomenda() {
        this.codEncomenda = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.peso = 0;
        this.encomendas = new ArrayList<>();
        this.codTrans = "";
        this.entregue = false;
        this.levantamento = LocalDateTime.of(0,1,1,0,0);
        this.entrega = LocalDateTime.of(0,1,1,0,0);
        this.encomendaMedica = false;
    }


    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso, List<LinhaEncomenda> encomendas,boolean encomendaMedica) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.encomendas = encomendas;
        this.codTrans = "";
        this.entregue = false;
        this.levantamento = LocalDateTime.of(0,1,1,0,0);
        this.entrega = LocalDateTime.of(0,1,1,0,0);
        this.encomendaMedica = encomendaMedica;
    }

    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso, List<LinhaEncomenda> encomendas, String codTrans, boolean entregue, LocalDateTime levantamento, LocalDateTime entrega, boolean encomendaMedica) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.encomendas = encomendas;
        this.codTrans = codTrans;
        this.entregue = entregue;
        this.levantamento = levantamento;
        this.entrega = entrega;
        this.encomendaMedica = encomendaMedica;
    }

    public Encomenda(Encomenda o){
        this.codEncomenda = o.getCodEncomenda();
        this.codUtilizador = o.getCodUtilizador();
        this.codLoja = o.getCodLoja();
        this.peso = o.getPeso();
        this.setEncomendas(o.getEncomendas());
        this.codTrans = o.getCodTrans();
        this.entregue = o.isEntregue();
        this.entrega = o.getEntrega();
        this.levantamento = o.getLevantamento();
        this.encomendaMedica = o.isEncomendaMedica();
    }

    /**
     * @return Retorna o codigo de encomenda.
     */
    public String getCodEncomenda() {
        return codEncomenda;
    }

    /**
     * @return Retorna o codigo do utilizador que pediu a encomenda.
     */
    public String getCodUtilizador() {
        return codUtilizador;
    }
    /**
     * @return Retorna o codigo da loja que preparou a encomenda.
     */
    public String getCodLoja() {
        return codLoja;
    }

    /**
     * @return Retorna o peso de encomenda.
     */
    public double getPeso() {
        return peso;
    }

    /**
     * @return Retorna uma cópia das linhas de encomenda.
     */
    public List<LinhaEncomenda> getEncomendas() {
        List<LinhaEncomenda> aux = new ArrayList<>();
        for(LinhaEncomenda l:this.encomendas)
            aux.add(l.clone());
        return aux;
    }

    /**
     * @return Retorna o codigo do transporte que entregou a encomenda.
     */
    public String getCodTrans() {
        return codTrans;
    }

    /**
     * @return Retorna um booleano que indica que a encomenda foi ou nao entregue.
     */
    public boolean isEntregue() {
        return entregue;
    }

    /**
     * @return Retorna a data de entrega.
     */
    public LocalDateTime getEntrega() {
        return entrega;
    }

    /**
     * @return Retorna a data em que foi requisitada a um transporte.
     */
    public LocalDateTime getLevantamento() {
        return levantamento;
    }

    /**
     * @return Retorna um booleano que indica se a encomenda é médica.
     */
    public boolean isEncomendaMedica() {
        return encomendaMedica;
    }

    /**
     * Setter das linhas de encomenda.
     */
    public void setEncomendas(List<LinhaEncomenda> encomendas) {
        this.encomendas = new ArrayList<>();
        for(LinhaEncomenda l:encomendas)
            this.encomendas.add(l.clone());
    }

    /**
     * Setter do codigo de transporte.
     */
    public void setCodTrans(String codTrans) {
        this.codTrans = codTrans;
    }

    /**
     * Setter do status de entrega.
     */
    public void setEntregue(boolean entregue) {
        this.entregue = entregue;
    }

    /**
     * Método que compara um encomenda com outro objeto.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda encomenda = (Encomenda) o;
        return Double.compare(encomenda.peso, peso) == 0 &&
                entregue == encomenda.entregue &&
                encomendaMedica == encomenda.encomendaMedica &&
                Objects.equals(codEncomenda, encomenda.codEncomenda) &&
                Objects.equals(codUtilizador, encomenda.codUtilizador) &&
                Objects.equals(codLoja, encomenda.codLoja) &&
                Objects.equals(encomendas, encomenda.encomendas) &&
                Objects.equals(codTrans, encomenda.codTrans) &&
                Objects.equals(levantamento, encomenda.levantamento) &&
                Objects.equals(entrega, encomenda.entrega);
    }

    /**
     * To String da encomenda.
     */
    public String toString() {
        final StringBuilder sb = new StringBuilder("Encomenda:");
        sb.append(this.codEncomenda).append(",");
        sb.append(this.codUtilizador).append(",");
        sb.append(this.codLoja).append(",");
        sb.append(this.peso).append(",");
        sb.append(encomendas).append(",");
        sb.append(this.entregue);
        return sb.toString();
    }

    /**
     * Método que retorna um clone da encomenda.
     */
    public Encomenda clone(){
        return new Encomenda(this);
    }

    //----------------------------------------------------------------------

    /**
     * Método que altera a encomenda com a data em que foi requisitada e a data de entrega.
     * @param transportador GPS do Transporte que fez a encomenda.
     * @param loja Loja que preparou a encomenda.
     * @param user GPS do utilizador que pediu a encomenda.
     */
    public void requesitaEncomenda(GPS transportador,Loja loja,GPS user){
        this.levantamento = LocalDateTime.now();
        double transito = Math.random()*30;
        double t1 = (transportador.distancia(loja.getCoordenadas())/Transportes.velocidadeMedia)*60;
        double t2 = (loja.getCoordenadas().distancia(user)/Transportes.velocidadeMedia)*60;
        double queue = loja.getFila().size()*loja.getMediaEspera();
        this.entrega = this.levantamento.plusMinutes((long) (transito+t1+t2+queue));
        this.entregue = true;
    }

    /**
     * Método que calcula o tempo esperado de entrega da encomenda.
     * @param transportador GPS do Transporte que fez a encomenda.
     * @param loja Loja que preparou a encomenda.
     * @param user GPS do utilizador que pediu a encomenda.
     * @return Retorna o tempo esperado de entrega.
     */
    public static double tempoPrevisto(GPS transportador,Loja loja,GPS user){
        //double transito = Math.random()*11;
        double t1 = (transportador.distancia(loja.getCoordenadas())/Transportes.velocidadeMedia)*60;
        double t2 = (loja.getCoordenadas().distancia(user)/Transportes.velocidadeMedia)*60;
        double queue = loja.getFila().size()*loja.getMediaEspera();
        return t1+t2+queue;
    }

    /**
     * Método que calcula o preco da encomenda não incluindo transporte.
     */
    public double precoEncomenda(){
        return this.encomendas.stream()
                              .map(a->a.getPrecoU()*a.getQuantidade())
                              .mapToDouble(Double::doubleValue)
                              .sum();
    }
}
