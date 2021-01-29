import Enums.Estado;
import Exception.*;

import java.io.Serializable;
import java.util.List;
import java.util.OptionalDouble;

/**
 * Classe que lida com a informação de um Voluntário.
 * 
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class Voluntario extends Entidade implements Serializable, Transportador {
    // Instance Variables
    private double    raio;
    private boolean   available;
    private boolean   certificado; //Medico
    private Historico historico;
    private PossibilidadeEntrega pe;

    // Constructors

    /**
     * Construtor de um Voluntário.
     */
    public Voluntario() {
        super();
        this.raio        = 0.0;
        this.available   = false;
        this.certificado = false;
        this.historico   = new Historico();
        this.pe          = null;
    }

    /**
     * Construtor de um Voluntário.
     * @param codigo Código do Voluntário.
     * @param nome Nome do Voluntário.
     * @param localizacao Localização do Voluntário.
     * @param raio Raio de ação do Voluntário.
     */
    public Voluntario(String codigo, String nome, Localizacao localizacao, double raio) {
        super(codigo, nome, localizacao);
        this.raio        = raio;
        this.available   = false;
        this.certificado = false;
        this.historico   = new Historico();
        this.pe          = null;
    }

    /**
     * Construtor de um Voluntário.
     * @param codigo Código do Voluntário.
     * @param nome Nome do Voluntário.
     * @param mail Mail do Voluntário.
     * @param password Password do Voluntário.
     * @param localizacao Localização do Voluntário.
     */
    public Voluntario(String codigo, String nome, String mail, String password, Localizacao localizacao){
        super(codigo,nome,mail,password,localizacao);
        this.raio        = 0;
        this.available   = false;
        this.certificado = false;
        this.historico   = new Historico();
        this.pe          = null;
    }

    /**
     * Construtor de um Voluntário por cópia.
     * @param voluntario Voluntário a copiar.
     */
    public Voluntario(Voluntario voluntario) {
        super(voluntario);
        this.raio        = voluntario.getRaio();
        this.available   = voluntario.getAvailable();
        this.certificado = voluntario.aceitoTransporteMedicamentos();
        this.historico   = voluntario.getHistorico();
        this.pe          = voluntario.getPossibilidadeEntrega();
    }

    // Gets

    /**
     * Função que devolve o Raio de um Voluntário.
     * @return Raio do Voluntário.
     */
    public double getRaio() {
        return this.raio;
    }

    /**
     * Função que devlve a disponibilidade do Voluntário.
     * @return true se estiver disponível.
     */
    public boolean getAvailable() {
        return this.available;
    }

    /**
     * Função que devolve o Histórico de um Voluntário.
     * @return Histórico do Voluntário.
     */
    public Historico getHistorico(){
        return this.historico.clone();
    }

    /**
     * Função que devolve a PossibilidadeEntrega de um Voluntário.
     * @return PossibilidadeEntrega do Voluntário.
     */
    private PossibilidadeEntrega getPossibilidadeEntrega(){
        return this.pe != null ? this.pe.clone() : null;
    }

    // Sets

    /**
     * Função que modifica a disponibilidade de um Voluntário.
     * @param available Nova disponibilidade.
     */
    public void setAvailable(boolean available) {
        this.available = available;
    }

    /**
     * Função que modifica a PossibilidadeEntrega de um Voluntário.
     * @param pe Nova PossibilidadeEntrega.
     */
    private void setPossibilidadeEntrega(PossibilidadeEntrega pe){
        this.pe = pe.clone();
    }

    //

    /**
     * Função que cria um clone de um Voluntário.
     * @return Voluntário clonado.
     */
    public Voluntario clone() {
        return new Voluntario(this);
    }

    /**
     * Função que compara Voluntários.
     * @param o, Objeto a comparar.
     * @return 'true' se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Voluntario v = (Voluntario) o;
        return super.equals(v) &&
               v.getRaio()        == this.raio &&
               v.getAvailable()   == this.available &&
               v.aceitoTransporteMedicamentos() == this.certificado &&
               v.getHistorico().equals(this.historico);
    }

    /**
     * Função que converte os parametros de um Voluntário para String.
     * @return String com os parametros do Voluntário.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("Raio: ").append(this.raio).append("\n");
        sb.append("Voluntário ").append(this.certificado ? "certificado " : "não certificado").append(" para encomendas médicas!\n");
        return sb.toString();
    }

    //
    /**
     * Função que diz se aceita encomendas médicas.
     * @return true se aceitar.
     */
    public boolean aceitoTransporteMedicamentos(){
        return this.certificado;
    }

    /**
     * Função que altera o estado de aceitação de encomendas médicas.
     * @param state Novo estado de aceitação.
     */
    public void aceitaMedicamentos(boolean state){
        this.certificado = state;
    }

    /**
     * Função que cria uma Voluntário a partir de uma linha.
     * @param linha, String a partir da qual se irá retirar os campos que permitem a criação de um Voluntário.
     * @return Voluntário criado a partir da linha.
     * @throws NumberArgumentsLineException, Quando o número de argumentos for inválido.
     * @throws TypeConvertionException, Quando o tipo for inválido.
     */
    static public Voluntario parse(String linha) throws NumberArgumentsLineException, TypeConvertionException {
        String[] campos = linha.split(",");
        if (campos.length != 5)
            throw new NumberArgumentsLineException();
        double lat,lon,raio;
        try{ lat = Double.parseDouble(campos[2]);
             lon = Double.parseDouble(campos[3]);
             raio = Double.parseDouble(campos[4]);}
        catch(NumberFormatException | NullPointerException e) {
            throw new TypeConvertionException("String","Double");
        }
        return new Voluntario(campos[0],campos[1],new Localizacao(lat,lon),raio);
    }

    /**
     * Função que verifica se um Voluntário está numa Localização acessivel.
     * @param l Localização.
     * @return true se a localização do Voluntário for menor ou igual à Localização dada.
     */
    public boolean acessivel(Localizacao l){
        return super.getLocalizacao().closerOrEqual(l,this.raio);
    }

    /**
     * Função que atribui uma entrega a um Voluntário.
     * @param pe PossibilidadeEntrega do voluntário.
     */
    public void entregar(PossibilidadeEntrega pe){
        this.setAvailable(false);
        this.setPossibilidadeEntrega(pe);
        this.pe.start();
    }

    /**
     * Função que sucede uma entrega de um Voluntário.
     * @param tempo Tempo da entrega.
     * @return InfoHistorico com a informação da Encomenda entregue.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public InfoHistorico entregue(double tempo) throws EncomendaNotFoundException {
        try{
            PossibilidadeEntrega pe = this.pe;
            this.pe = null;
            Localizacao loja = pe.getGpsLoja();
            Localizacao user = pe.getGpsUtilizador();
            double distancia = loja.distance(user) + loja.distance(super.getLocalizacao());
            InfoHistorico ih = new InfoHistorico(pe.getCodigoEncomenda(),
                                                 pe.getCodigoUtilizador(),
                                                 pe.getCodigoLoja(),
                                                 super.getCodigo(),
                                                 distancia,
                                                 tempo,
                                           0);
            this.historico.add(ih);
            return ih.clone();
        } catch (NullPointerException e){
            throw new EncomendaNotFoundException("Sem entrega a fazer");
        }
    }

    /**
     * Função que devolve a distância total percorrida por um Voluntário.
     * @return Distância total de um Voluntário.
     */
    public double getDistanciaTotal() {
        return this.historico.getDistanciaTotal();
    }

    /**
     * Função que modifica o raio de ação de um Voluntário.
     * @param raio Novo raio.
     * @throws NotPositiveNumberException Quando o raio não for positivo.
     */
    public void setRaio(double raio) throws NotPositiveNumberException{
        if(raio < 0) throw new NotPositiveNumberException(raio);
        this.raio = raio;
    }

    /**
     * Função que faz um Voluntário aceitar uma entrega.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public void aceitar() throws EncomendaNotFoundException{
        if(this.pe == null) throw new EncomendaNotFoundException();
        this.pe.setAEntregar(super.getCodigo());
    }

    /**
     * Função que faz um Voluntário recusar uma entrega.
     * @return PossibilidadeEntrega.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public PossibilidadeEntrega recusar() throws EncomendaNotFoundException{
        if(this.pe == null) throw new EncomendaNotFoundException();
        PossibilidadeEntrega pe = this.pe.clone();
        this.pe = null;
        return pe;
    }

    /**
     * Fução que retorna o estado de entrega de um Voluntário.
     * @return Estado de entrega de um voluntário.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public Estado getEstadoEntrega() throws EncomendaNotFoundException {
        if(pe == null) throw new EncomendaNotFoundException();
        return this.pe.getAtribuida();
    }

    /**
     * Função que devolve o código da Encomenda da entrega de um Voluntário.
     * @return Código da Encomenda.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public String getCodigoEncomenda() throws EncomendaNotFoundException{
        if(pe == null) throw new EncomendaNotFoundException();
        return this.pe.getCodigoEncomenda();
    }

    /**
     * Função que devolve o custo de um Voluntário (0).
     * @param peso Peso da encomenda.
     * @param distancia Distância da entrega.
     * @param tempo Tempo da entrega.
     * @return Custo de um Voluntário.
     */
    public double getCusto(double peso, double distancia, double tempo){
        return 0.0;
    }

    /**
     * Função que classifica a entrega pela parte do Voluntário.
     * @param encomenda Código da encomenda entregue.
     * @param classificacao Classificação atribuida.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     * @throws ClassificacaoInvalidaException Quando a classificação não se encontra entre 1 e 10, inclusive.
     */
    public void classificar(String encomenda, int classificacao) throws EncomendaNotFoundException, ClassificacaoInvalidaException{
        this.historico.classificar(encomenda,classificacao);
    }

    /**
     * Função que devolve a classificção média de um Voluntário.
     * @return Classificação Média do Voluntário.
     */
    public double classificacaoMedia(){
        return this.historico.ratingMedioTransportador();
    }

    /**
     * Devolve informação relativa às encomendas tranportadas pelo transportador.
     * @return Estrutura com informação sobre as encomendas transportadas.
     */
    public List<String> encomendasTransportadas() {
        return this.historico.getListTransportadas();
    }
}
