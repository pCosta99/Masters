package Model;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import Exceptions.NoEntityException;
import View.PrintFormat;

/** Classe que representa uma encomenda. */
public class Encomenda implements Serializable, IEncomenda {
    private static final long serialVersionUID = 126L;

    /** Código da encomenda. */
    private String codigo;
    /** Código do utilizador que encomendou. */
    private String codUtil;
    /** Código da loja onde foi feita a encomenda. */
    private String codLoja;
    
    /** Indica se contém produtos médicos. */
    private boolean medica;
    /** Custo dos portes de envio. */
    private BigDecimal portes;

    /** Peso da encomenda. */
    private double peso;
    /** Data do pedido de encomenda. */
    private LocalDateTime data;
    /** Data em que a encomenda foi atribuida a um distribuidor. */
    private LocalDateTime dataAceitacao;

    /** Conjunto de linhas de encomenda. */
    private Set<LinhaEncomenda> linhas;


    /** Construtor por omissão. */
    public Encomenda() {
        this.codigo  = "n/a";
        this.codUtil = "n/a";
        this.codLoja = "n/a";
        this.peso    = 0;
        this.data    = LocalDateTime.of(1970, 1, 1, 0, 0, 0);
        this.data    = null;
        this.linhas  = new HashSet<>();
        this.portes = BigDecimal.ZERO;
        medica = false;
    }

    /**
     * Construtor parametrizado.
     * @param cod           Código da encomenda.
     * @param codUtil       Código do utilizador que encomendou.
     * @param codLoja       Código da loja onde foi feita a encomenda.
     * @param peso          Peso da encomenda.
     * @param linhas        Conjunto de linhas de encomenda.
     * @param data          Data do pedido de encomenda.
     * @param dataAceitacao Data em que a encomenda foi atribuida a um distribuidor.
     * @param medica        Indica se a encomenda contém produtos médicos.
     */
    public Encomenda(String cod, String codUtil, String codLoja, double peso, Set<LinhaEncomenda> linhas, LocalDateTime data, LocalDateTime dataAceitacao, boolean medica) {
        this.codigo  = cod;
        this.codUtil = codUtil;
        this.codLoja = codLoja;
        this.peso    = peso;
        this.data    = data;
        this.dataAceitacao    = dataAceitacao;
        setLinhas(linhas);
        this.portes = BigDecimal.ZERO;
        this.medica = medica;
    }

    /**
     * Construtor de cópia.
     * @param enc Encomenda.
     */
    public Encomenda(Encomenda enc) {
        this.codigo  = enc.getCodigo();
        this.codUtil = enc.getCodUtil();
        this.codLoja = enc.getCodLoja();
        this.peso    = enc.getPeso();
        this.data    = enc.getData();
        setLinhas(enc.getLinhas());
        this.portes = enc.getPortes();
        this.medica = enc.getMedica();
    }


    public String getCodigo() {
        return this.codigo;
    }

    /** 
     * Define o código da encomenda.
     * @param codigo Código da encomenda.
     */    
    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public String getCodUtil() {
        return this.codUtil;
    }

    /** 
     * Define o código do utilizador que encomendou.
     * @param codUtil Código do utilizador que encomendou.
     */
    public void setCodUtil(String codUtil) {
        this.codUtil = codUtil;
    }

    public String getCodLoja() {
        return this.codLoja;
    }
    
    /** 
     * Define o código da loja onde foi feita a encomenda. 
     * @param codLoja Código da loja onde foi feita a encomenda. 
     */
    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public double getPeso() {
        return this.peso;
    }

    /**
     * Define o peso da encomenda.
     * @param peso Peso da encomenda.
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }

    public BigDecimal getPortes() {
        return this.portes;
    }

    public void setPortes(BigDecimal portes) {
        this.portes = portes;
    }

    /** 
     * Retorna o conjunto de linhas de encomenda.
     * @return Conjunto de linhas de encomenda.
     */
    public Set<LinhaEncomenda> getLinhas() {
        return this.linhas.stream()
            .map(LinhaEncomenda :: clone)
            .collect(Collectors.toSet());
    }

    /** 
     * Define o conjunto de linhas de encomenda.
     * @param linhas Conjunto de linhas de encomenda. 
     */
    public void setLinhas(Set<LinhaEncomenda> linhas) {
        this.linhas = linhas.stream()
            .map(LinhaEncomenda :: clone)
            .collect(Collectors.toSet());
    }

    public LocalDateTime getData() {
        return this.data;
    }

    public void setData(LocalDateTime data) {
        this.data = data;
    }

    public LocalDateTime getDataAceitacao() {
        return this.dataAceitacao;
    }

    public void setDataAceitacao(LocalDateTime data) {
        this.dataAceitacao = data;
    }

    public boolean getMedica() {
        return this.medica;
    }

    public void setMedica(boolean medica) {
        this.medica = medica;
    }


    /** 
     * Implementação do método toString().
     * @return Representação em String de uma instância da classe Encomenda.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n┏━━━━━┫ Encomenda [");
        sb.append(codigo);
        sb.append("] ┣━━━━━┓\n");
        sb.append("\n Código do utilizador: ");
        sb.append(codUtil);
        sb.append("\n Código da loja: ");
        sb.append(codLoja);
        sb.append("\n Peso: ");
        sb.append(peso < 0 ? "A definir" : (peso + " Kg"));
        sb.append("\n Data: ");
        sb.append(PrintFormat.dateFormat(data));
        sb.append("\n Data de Atribuição a um distribuidor: ");
        sb.append(PrintFormat.dateFormat(dataAceitacao));
        if(this.medica)
            sb.append("\n É encomenda médica");
        sb.append("\n Linhas de encomenda: ");
        for(LinhaEncomenda l : this.linhas)
            sb.append("\n").append(l);
        sb.append("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        return sb.toString();
    }

    /**
     * Implementação do método equals().
     * 
     * @param o Objeto ao qual será comparado.
     * @return Valor booleano a indicar a igualdade ao dado objeto.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if ((o == null) || (o.getClass() != this.getClass()))
            return false;

        Encomenda enc = (Encomenda) o;
        return codigo.equals(enc.getCodigo()) && codUtil.equals(enc.getCodUtil())
            && codLoja.equals(enc.getCodLoja()) && peso == enc.getPeso()
            && linhas.equals(enc.getLinhas()) && data.equals(enc.getData()) 
            && dataAceitacao.equals(enc.getDataAceitacao());
    }

    /**
     * Implementação do método clone().
     * @return Cópia de uma dada instância de Encomenda.
     */
    public Encomenda clone() {
        return new Encomenda(this);
    }


    /**
     * Adiciona uma linha de encomenda à encomenda. 
     * @param cod       Código do produto.
     * @param desc      Descrição do produto.
     * @param quant     Quantidade do produto.
     * @param valorUnit Valor unitário do produto.
    */
    public void addLinhaEncomenda(String cod, String desc, int quant, BigDecimal valorUnit) {
        this.linhas.add(new LinhaEncomenda(cod, desc, quant, valorUnit));
    }

    /** @return Quantidade total de todos os produtos na encomenda. */
    public int quantidadeDeProdutos() {
        return linhas.stream().mapToInt(le -> le.getQuantidade()).sum();
    }

    /** @return Valor total de todos os produtos na encomenda. */
    public BigDecimal valorTotal() {
        return linhas.stream()
            .map(le -> le.getValorUnitario()
                .multiply(BigDecimal.valueOf(le.getQuantidade())))
            .reduce(BigDecimal.ZERO,(a,l) -> a.add(l));
    }

    /**
     * Implementação do método hashCode().
     * @return Código de hash com base no código da encomenda.
     */
    @Override
    public int hashCode() {
        return codigo.hashCode();
    }

    public int nLinhasEnc() {
        return this.linhas.size();
    }

    public Set<String> getProdutosDasLinhas() {
        return this.linhas.stream()
            .map(l -> l.getCodigo())
            .collect(Collectors.toSet());
    }

    public void setValorUnitn(String codigo, BigDecimal preco) throws NoEntityException {
        boolean existe = false;
        for (LinhaEncomenda l : this.linhas)
            if (l.getCodigo().equals(codigo)) {
                l.setValorUnitario(preco);
                existe = true;
            }
        if (!existe) 
            throw new NoEntityException("não existe produto com código \'" + codigo +  "\'");
    }
}