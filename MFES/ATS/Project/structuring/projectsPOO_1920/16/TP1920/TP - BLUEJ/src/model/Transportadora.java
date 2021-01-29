package src.model;

import javax.swing.tree.TreeNode;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que cria Transportadoras
 */

public class Transportadora extends Distribuidor {
    //Variáveis de Classe
    private static String codTransGlobal = "t0";
    
    //Variáveis de Instância
    private double kmsPercorridos;
    private double precoPorKm;
    private double precoPorMin;
    private Map<String,Entrega> entregasAtivas;


    /**
     * Devolve o username de model.Transportadora gerado para a próxima model.Transportadora a ser criada
     * @return String com o novo username de model.Transportadora
     */
    private static String getCodTransGlobal(){
        String cod = Transportadora.codTransGlobal;
        String inc = cod.substring(1);
        int i = Integer.parseInt(inc);
        i = i+1;
        Transportadora.setCodTransGlobal("t"+i);
        return cod;
    }


    /**
     * Atualiza o username de model.Transportadora gerado para a próxima model.Transportadora a ser criada
     * @param cod String com o novo username de model.Transportadora que substituirá o atual
     */
    private static void setCodTransGlobal(String cod){
        Transportadora.codTransGlobal = cod;
    }



    /**
     * Atualiza o username de transportadora a ser atribuido para a transportadora seguinte, baseado no ultimo codigo atribuído através de logs;
     * @param cod String com o ultimo username de transportadora usado nos logs
     */
    public static void updateCodGlobal(String cod){
        String atual = Transportadora.getCodTransGlobal();
        int numberAtual = Integer.parseInt(atual.substring(1));
        int numberCod = Integer.parseInt(cod.substring(1));
        if (numberCod > numberAtual) {
            Transportadora.setCodTransGlobal("t"+(numberCod+1));
        }
    }

    /**
     * Construtores da classe model.Transportadora.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.Transportadora.
     */

    public Transportadora(){
        super();
        this.setUsername(Transportadora.getCodTransGlobal());
        this.precoPorKm = 0;
        this.precoPorMin = 0;
        this.kmsPercorridos = 0;
        this.entregasAtivas = new HashMap<>();
    }

    /**
     * Construtor parametrizado de model.Transportadora.
     * Aceita como parâmetros os valores para cada variável de instância e chama o construtor de super.
     */

    public Transportadora( String password, String nome, String email, boolean recolhe, double precoPorKm, double precoPorMin, List<Entrega> historicoEntregas, Classificacao classificacao, double raio, Ponto localizacao){
        super(Transportadora.getCodTransGlobal(),password, nome,email,recolhe,historicoEntregas,classificacao,raio,localizacao);
        this.precoPorKm = precoPorKm;
        this.precoPorMin = precoPorMin;
        this.entregasAtivas = new HashMap<>();
        this.kmsPercorridos = 0;
    }

    /**
     * Construtor de cópia de model.Transportadora.
     * Aceita como parâmetro outra model.Transportadora e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public Transportadora(Transportadora t){
        super(t.getUsername(),t.getPassword(),t.getNome(),t.getEmail(),t.estaARecolher(),t.getHistoricoEntregas(),t.getClassificacao(),t.getRaio(),t.getLocalizacao());
        this.precoPorKm = t.getPrecoPorKm();
        this.precoPorMin = t.getPrecoPorMin();
        this.kmsPercorridos = t.getKmsPercorridos();
        this.entregasAtivas = t.getEntregasAtivasc();
    }

    /**
     * Construtor parametrizado de model.Transportadora para criação de instância a partir de logs.
     * Aceita como parâmetros os valores para cada variável de instância.
     */
    public Transportadora(String cod, String password, String nome, String email, boolean recolhe, double precoPorKm, double precoPorMin, List<Entrega> historicoEntregas, Classificacao classificacao, double raio, Ponto localizacao){
        super(cod,password, nome,email,recolhe,historicoEntregas,classificacao,raio,localizacao);
        this.precoPorKm = precoPorKm;
        this.precoPorMin = precoPorMin;
        this.kmsPercorridos = 0;
        Transportadora.updateCodGlobal(cod);
        this.entregasAtivas = new HashMap<>();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o preço por km da recolha de uma encomenda
     * @return double preco por km da recolha de uma encomenda
     */

    public double getPrecoPorKm() {
        return this.precoPorKm;
    }

    /**
     *  Devolve o preço por minuto de espera da recolha de uma encomenda
     *  @return double preco por minuto da recolha de uma encomenda
     */

    public double getPrecoPorMin(){
        return this.precoPorMin;
    }

    /**
     * Devolve a quantidade de Kms percorridos por essa transportadora
     * @return double com os kms percorridos por essa transportadora
     */
    public double getKmsPercorridos() {
        return this.kmsPercorridos;
    }

    /**
     * Devolve o mapa com as entregas ativas da transportadora
     * @return mapa que associa o codigo de encomenda a sua devida entrega
     */
    public Map<String,Entrega> getEntregasAtivasc(){
        Map<String,Entrega> r = new HashMap<>();

        for(Map.Entry<String,Entrega> e : this.entregasAtivas.entrySet()){
            r.put(e.getKey(),e.getValue().clone());
        }

        return r;
    }


    /**
     * Atualiza o preço por km da recolha de uma encomenda
     * @param preco novo preço por km da recolha de uma encomenda
     */

    public void setPrecoPorKm(double preco) {
        this.precoPorKm = preco;
    }

    /**
     * Atualiza o preço por minuto de espera da recolha de uma encomenda
     * @param preco novo preço por minuto de espera da recolha de uma encomenda
     */

    public void setPrecoPorMin(double precoPorMin){
        this.precoPorMin = precoPorMin;
    }


    /**
     * Atualiza a quantidade de Kms percorridos por uma transportadora
     * @param kmsPercorridos novo valor de Kms percorridos pela transportadora
     */
    public void setKmsPercorridos(double kmsPercorridos){
        this.kmsPercorridos = kmsPercorridos;
    }

    /**
     * Retorna o mapa com as entregas ativas da transportadora
     * @param entregasAtivas mapa que associa o codigo de encomenda a sua devida entrega
     */
    public void setEntregasAtivas(Map<String,Entrega> entregasAtivas) {
        Map<String,Entrega> res = new HashMap<>();
        for(Map.Entry<String,Entrega> e : entregasAtivas.entrySet()){
            res.put(e.getKey(),e.getValue().clone());
        }
        this.entregasAtivas = res;
    }

    /**
     * Devolve o mapa com as entregas ativas da transportadora
     * @return set de entregas ativas da transportadora
     */
    public Set<Entrega> getEntregasAtivas(){
        return this.entregasAtivas.values().stream().map(Entrega::clone).collect(Collectors.toCollection(TreeSet::new));
    }

    /**
     * Adiciona uma quantidade de Kms percorridos por uma transportadora
     * @param kmsPercorridos Valor de Kms percorridos a ser adicionado na transportadora
     */
    public void addKmsPercorridos(double kmsPercorridos){
        this.kmsPercorridos += kmsPercorridos;
    }



    /**
     * Calcula custo de entrega para se realizar a entrega de uma encomenda vinda de uma loja para um dado destino
     * @param l loja de onde vem a entrega
     * @param destino Ponto para onde a entrega se destina
     * @return double com o custo para se realizar a entrega
     */
    public double calcCustoEntrega(Loja l, Ponto destino) {
        double dist = this.getLocalizacao().distancia(l.getLocalizacao()) + this.getLocalizacao().distancia(destino);
        return this.precoPorKm * dist + this.precoPorMin * l.getTempoEspera();
    }


    /** 
     * Adiciona entrega ativa no mapa de entregas ativas de uma transportadora
     * @param e Entrega a ser colocada como ativa na transportadora
     */
    public void addEntregaAtiva(Entrega e){
        this.entregasAtivas.put(e.getEncomenda().getCodEnc(),e.clone());
    }


    /** 
     * remove entrega ativa no mapa de entregas ativas de uma transportadora
     * @param codEnc codigo de encomenda da Entrega a ser removida das ativas na transportadora
     */
    public Entrega removeEntregaAtiva(String codEnc){
        return this.entregasAtivas.remove(codEnc);
    }

    /**
     * Devolve o valor faturado por uma Empresa em um dado período de tempo
     * @param aPartirDe valor da data da qual começa-se a buscar o faturamento
     * @param ate valor da data até quando se irá buscar o valor faturado
     */
    public double getValorFaturado(LocalDateTime aPartirDe, LocalDateTime ate){
        return this.getHistoricoEntregas().stream()
                .filter( e -> e.getDataLevantamento().isAfter(aPartirDe))
                .filter( e -> e.getDataLevantamento().isBefore(ate))
                .map(Entrega :: getCusto)
                .reduce((double) 0, Double::sum);
    }



    /**
     * Método que transforma uma model.Transportadora numa String
     * @return String com toda a informação presente na model.Transportadora
     */

    public String toString() {
        String m = super.toString();
        StringBuilder sb = new StringBuilder("model.Transportadora{");
        sb.append("precoPorKm=").append(this.precoPorKm);
        sb.append("precoPorMin=").append(this.precoPorMin);
        sb.append("entregasAtivas=").append(this.entregasAtivas.toString());
        sb.append("kmsPercorridos=").append(this.kmsPercorridos);
        sb.append('}');
        return m + sb.toString();
    }

    /**
     * Método que determina se uma model.Transportadora e um Objeto são iguais
     * @param o Objeto qualquer
     * @return true caso a model.Transportadora e o Objeto sejam iguais, e vice versa
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Transportadora t = (Transportadora) o;
        return this.precoPorKm == t.getPrecoPorKm() &&
                this.precoPorMin == t.getPrecoPorMin() &&
                this.entregasAtivas.equals(t.getEntregasAtivasc()) &&
                this.kmsPercorridos == t.getKmsPercorridos();
    }

    /**
     * Método que clona uma model.Transportadora, para tal é usado o construtor de cópia
     * @return Objeto model.Transportadora que é uma cópia da model.Transportadora
     */

    public Transportadora clone(){
        return new Transportadora(this);
    }



}
