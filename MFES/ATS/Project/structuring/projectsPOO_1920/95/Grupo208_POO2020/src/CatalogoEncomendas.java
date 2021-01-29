import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import Exception.EncomendaNotFoundException;
import Exception.EncomendaInvalidaException;

/**
 * Classe que lida com informação de várias Encomendas.
 * 
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class CatalogoEncomendas implements Serializable {
    // Instance Variables
    private Map<String, Encomenda> encomendas;

    // Constructors

    /**
     * Construtor de um CatalogoEncomendas.
     */
    public CatalogoEncomendas() {
        this.encomendas = new HashMap<>();
    }

    /**
     * Construtor de um CatalogoEncomendas.
     * @param encomendas Encomendas do CatalogoEncomendas a construir.
     */
    public CatalogoEncomendas(Map<String, Encomenda> encomendas) {
        this.setEncomendas(encomendas);
    }

    /**
     * Construtor de um CatalogoEncomendas por cópia.
     * @param c CatálogoEncomendas a copiar.
     */
    public CatalogoEncomendas(CatalogoEncomendas c) {
        this.encomendas = c.getEncomendas();
    }

    // Get

    /**
     * Função que devolve as Encomendas do CatalogoEncomendas.
     * @return Map com as Encomendas do CatalogEncomendas.
     */
    private Map<String, Encomenda> getEncomendas() {
        return this.encomendas.entrySet()
                              .stream()
                              .collect(Collectors.toMap(Map.Entry::getKey,
                                                        v -> v.getValue().clone()));
    }

    // Set

    /**
     * Função que altera as Encomendas do CatalogoEncomendas.
     * @param e Map com as novas Encomendas do CatalogoEncomendas.
     */
    private void setEncomendas(Map<String, Encomenda> e) {
        this.encomendas = e.entrySet()
                           .stream()
                           .collect(Collectors.toMap(Map.Entry::getKey,
                                                     v -> v.getValue().clone()));
    }

    //


    /**
     * Função que cria um clone de um CatalogoEncomendas.
     * @return CatalogoEncomendas clonado.
     */
    public CatalogoEncomendas clone() {
        return new CatalogoEncomendas(this);
    }

    /**
     * Função que compara CatalogoEncomendas.
     * @param o Objeto a comparar.
     * @return true se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        CatalogoEncomendas ce = (CatalogoEncomendas) o;
        return this.encomendas.equals(ce.getEncomendas());
    }

    /**
     * Função que converte os parâmetros de um CatalogoEncomendas em String.
     * @return String com os parâmetros de um CatalogoEncomendas.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        this.encomendas.values().forEach(e -> sb.append(e).append("\n"));
        return sb.toString();
    }

    //

    /**
     * Função que adiciona uma Encomenda ao CatalogoEncomendas
     * @param e Encomenda a adicionar.
     */
    public void add(Encomenda e) {
        this.encomendas.put(e.getCodEncomenda(), e.clone());
    }

    /**
     * Função que retorna uma Encomenda a partir de um código.
     * @param codigo Código da Encomenda a pesquisar.
     * @return Encomenda correspondente ao código.
     * @throws EncomendaNotFoundException Quando a Encomenda não for encontrada.
     */
    public Encomenda getEncomenda(String codigo) throws EncomendaNotFoundException {
        Encomenda e = this.encomendas.get(codigo);
        if(e == null) throw new EncomendaNotFoundException();
        return e.clone();
    }

    /**
     * Função que gera um código.
     * @return String com o código gerado.
     */
    public String codeGenerator(){
        String s = null;
        while(s == null || this.encomendas.containsKey(s)){
            StringBuilder sb = new StringBuilder();
            s = sb.append('e')
                    .append(ThreadLocalRandom.current()
                                             .nextInt(1,10000))
                    .toString();
        }
        return s;
    }

    /**
     * Função que adiciona uma Encomenda ao Catálogo.
     * @param codUtilizador Código do Utilizador que fez a Encomenda.
     * @param codLoja Código da Loja da Encomenda.
     * @param peso Peso da Encomenda.
     * @param les LinhasEncomenda.
     * @param certificado Diz se a Encomenda é médica ou não.
     * @return String da Encomenda adicionada.
     * @throws EncomendaInvalidaException Quando a Encomenda for inválida.
     */
    public String add(String codUtilizador, String codLoja, double peso, List<LinhaEncomenda> les, boolean certificado) throws EncomendaInvalidaException {
        String codigoEnc = this.codeGenerator();
        this.encomendas.put(codigoEnc,new Encomenda(codigoEnc,codUtilizador,codLoja,peso,les,certificado));
        return codigoEnc;
    }
}
