import Exception.*;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que lida com a informação de um Utilizador.
 * 
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class Utilizador extends Entidade implements Serializable {
    private final List<PossibilidadeEntrega> pendentes;
    private final Historico  entregues;

    // Constructors

    /**
     * Construtor de um Utilizador.
     */
    public Utilizador() {
        super();
        this.pendentes = new ArrayList<>();
        this.entregues = new Historico();
    }

    /**
     * Construtr de um Utilizador.
     * @param codigo Codigo do Utilizador.
     * @param nome Nome do Utilizador.
     * @param localizacao Localização do Utilizador.
     */
    public Utilizador(String codigo, String nome, Localizacao localizacao) {
        super(codigo, nome, localizacao);
        this.pendentes = new ArrayList<>();
        this.entregues = new Historico();
    }

    /**
     * Construtor de um Utilizador.
     * @param codigo Código do Utilizador.
     * @param nome Nome do Utilizador.
     * @param mail Mail do Utilizador.
     * @param password Password do Utilizador.
     * @param localizacao Localização do Utilizador.
     */
    public Utilizador(String codigo, String nome, String mail, String password, Localizacao localizacao){
        super(codigo,nome,mail,password,localizacao);
        this.pendentes = new ArrayList<>();
        this.entregues = new Historico();
    }

    /**
     * Construtor de um Utilizador por cópia.
     * @param user Utilizador a copiar.
     */
    public Utilizador(Utilizador user) {
        super(user);
        this.pendentes = user.getPendentes();
        this.entregues = user.getEntregues();
    }

    /**
     * Função que devolve a Lista de Encomendas pendentes.
     * @return List Encomendas pendentes.
     */
    private List<PossibilidadeEntrega> getPendentes(){
        return this.pendentes.stream()
                             .map(PossibilidadeEntrega::clone)
                             .collect(Collectors.toList());
    }

    /**
     * Função que devolve o Histórico de todas as Encomendas feitas pelo Utilizador e que foram entregues.
     * @return Informação sobre Encomendas entregues.
     */
    private Historico getEntregues(){
        return this.entregues.clone();
    }

    //

    /**
     * Função que cria um clone de um Utilizador.
     * @return Utilizador clonado.
     */
    public Utilizador clone() {
        return new Utilizador(this);
    }

    /**
     * Função que converte os parâmetros de um Utilizador em String.
     * @return String com os parâmetros de um Utilizador.
     */
    public String toString() {
        return super.toString();
    }

    /**
     * Função que compara Utilizadores.
     * @param o Objeto a comparar.
     * @return true se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        return super.equals(o);
    }
    //

    /**
     * Cria um Utilizador a partir de uma linha.
     * @param linha Linha a partir de onde se irá criar um Utilizador.
     * @return Utilizador criado a partir de uma linha.
     * @throws NumberArgumentsLineException Quando a linha tiver argumentos insuficientes.
     * @throws TypeConvertionException Quando é impossível converter dados.
     */
    static public Utilizador parse(String linha) throws NumberArgumentsLineException, TypeConvertionException{
        String[] campos = linha.split(",");
        if (campos.length != 4)
            throw new NumberArgumentsLineException();
        double lat,lon;
        try{ lat = Double.parseDouble(campos[2]);
             lon = Double.parseDouble(campos[3]);}
        catch(NumberFormatException | NullPointerException e) {
            throw new TypeConvertionException("String","Double");
        }
        return new Utilizador(campos[0],campos[1],new Localizacao(lat,lon));
    }

    /**
     * Função que adiciona um pedido de encomenda à lista de pendentes.
     * @param pe PossibilidadeEntrega de uma Encomenda.
     */
    public void addPedidoEncomenda(PossibilidadeEntrega pe){
        this.pendentes.add(pe);
    }

    /**
     * Função que devolve as Encomendas pendentes feitas por um Utilizador.
     * @return Map com as Encomendas e seu custo.
     */
    public Map<String,Double> getListPendentes(){
        return this.pendentes.stream()
                             .collect(Collectors.toMap(PossibilidadeEntrega::getCodigoEncomenda,
                                                       PossibilidadeEntrega::getCusto));
    }

    /**
     * Função que retira uma 'Encomenda' da lista de pendentes.
     * @param codEncomenda Código da Encomenda a pesquisar.
     * @return PossibilidadeEntrega da Encomenda.
     * @throws EncomendaNotFoundException Quando a Encomenda não for encontrada.
     */
    public PossibilidadeEntrega responder(String codEncomenda) throws EncomendaNotFoundException{
        Iterator<PossibilidadeEntrega> it = this.pendentes.iterator();
        boolean found = false;
        PossibilidadeEntrega pe = null;
        while(!found && it.hasNext()){
            pe = it.next();
            found = codEncomenda.equals(pe.getCodigoEncomenda());
        }
        if(!found) throw new EncomendaNotFoundException(codEncomenda);
        this.pendentes.remove(pe);
        return pe.clone();
    }

    /**
     * Função que adiciona informação sobre uma Encomenda entregue à lista de Encomendas entregues.
     * @param infoHistorico Informação sobre a Encomenda.
     */
    public void entregue(InfoHistorico infoHistorico){
        this.entregues.add(infoHistorico.clone());
    }

    /**
     * Função que classifica a entrega de uma Encomenda.
     * @param encomenda Encomenda entregue.
     * @param classificacao Classificação a atribuir.
     * @return Código do Transportador que entregou a encomenda.
     * @throws ClassificacaoInvalidaException Quando a classificação não está entre 1 e 10 ,inclusive.
     * @throws EncomendaNotFoundException Quando a Encomenda não for encontrada.
     */
    public String classificar(String encomenda, int classificacao) throws ClassificacaoInvalidaException, EncomendaNotFoundException {
        return this.entregues.classificar(encomenda,classificacao);
    }

    /**
     * Função que retorna o número de Encomendas entregues, feitas pelo Utilzador.
     * @return Número de Encomendas entregues.
     */
    public int getSize(){
        return this.entregues.getSize();
    }

    /**
     * Função que retorna uma Lista da das Encomendas que foram entregues, mas não estão classificadas.
     * @return List com as Encomendas não classificadas.
     */
    public List<String> getEntreguesNaoClassificados(){
        return this.entregues.getListNaoClassificados();
    }

    /**
     * Função que devolve informação das Encomendas entregues num determinado intervalo de tempo.
     * @param inf Limite inferior.
     * @param sup Limite superior.
     * @param transportador Código do Transportador que efetuou a entrega.
     * @return Informação de todas as encomendas entregues num determindao intervalo de tempo.
     * @throws OrdemCronologicaErradaException Quando a ordem cronológica estiver errada.
     */
    public Collection<String> getInfoHistoricoIntervaloUti(LocalDateTime inf, LocalDateTime sup,String transportador) throws OrdemCronologicaErradaException {
        return this.entregues.getInfoHistoricoIntervalo(inf,sup,transportador);
    }
}
