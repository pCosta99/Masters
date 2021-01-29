import Enums.Estado;
import Exception.*;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Classe que lida com a informação de uma Empresa.
 * 
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class Empresa extends Entidade implements Serializable, Transportador {
    // Instance Variables
    private int     nif;
    private double  raio;
    private double  precoPorKm;
    private double  precoPorPeso;
    private double  precoPorHora;
    private boolean available;
    private boolean certificado; //Medico
    private final Historico historico;
    private PossibilidadeEntrega pe; //Encomenda atual a entregar

    // Constructors

    /**
     * Construtor de uma Empresa.
     */
    public Empresa() {
        super();
        this.nif          = 0;
        this.raio         = 0.0;
        this.precoPorKm   = 0.0;
        this.precoPorPeso = 0.0;
        this.precoPorHora = 0.0;
        this.available    = false;
        this.certificado  = false;
        this.historico    = new Historico();
        this.pe           = null;
    }

    /**
     * Construtor de uma Empresa.
     * @param codigo Código da Empresa.
     * @param nome Nome da Empresa.
     * @param localizacao Localização da Empresa.
     * @param nif Nif da Empresa.
     * @param raio Raio da empresa.
     * @param precoPorKm Preco por kilómetro da Empresa.
     */
    public Empresa(String codigo, String nome, Localizacao localizacao, int nif, double raio, double precoPorKm) {
        super(codigo, nome, localizacao);
        this.nif          = nif;
        this.raio         = raio;
        this.precoPorKm   = precoPorKm;
        this.precoPorPeso = 0.0;
        this.precoPorHora = 0.0;
        this.available    = false;
        this.certificado  = false;
        this.historico    = new Historico();
        this.pe           = null;
    }

    /**
     * Construtor de uma Empresa.
     * @param codigo Código da Empresa.
     * @param nome Nome da Empresa.
     * @param mail Mail da Empresa.
     * @param password Password da Empresa.
     * @param localizacao Localização da Empresa.
     */
    public Empresa(String codigo, String nome, String mail, String password, Localizacao localizacao){
        super(codigo,nome,mail,password,localizacao);
        this.nif          = 0;
        this.raio         = 0;
        this.precoPorKm   = 0.0;
        this.precoPorPeso = 0.0;
        this.precoPorHora = 0.0;
        this.available    = false;
        this.certificado  = false;
        this.historico    = new Historico();
        this.pe           = null;
    }

    /**
     * Construtor por cópia de uma Empresa.
     * @param empresa Empresa a copiar.
     */
    public Empresa(Empresa empresa) {
        super(empresa);
        this.nif          = empresa.getNif();
        this.raio         = empresa.getRaio();
        this.precoPorKm   = empresa.getPrecoPorKm();
        this.precoPorPeso = empresa.getPrecoPorPeso();
        this.precoPorHora = empresa.getPrecoPorHora();
        this.available    = empresa.getAvailable();
        this.certificado  = empresa.aceitoTransporteMedicamentos();
        this.historico    = empresa.getHistorico();
        this.pe           = empresa.getPossibilidadeEntrega();
    }

    // Get's

    /**
     * Função que devolve o Nif de uma empresa.
     * @return Nif da empresa.
     */
    public int getNif() {
        return this.nif;
    }

    /**
     * Função que devolve o Raio de uma empresa.
     * @return Raio da empresa.
     */
    public double getRaio() {
        return this.raio;
    }

    /**
     * Função que devolve o Preço por kilómetro de uma empresa.
     * @return Preço por Km da empresa.
     */
    public double getPrecoPorKm() {
        return this.precoPorKm;
    }

    /**
     * Função que devolve o preço por peso de uma empresa.
     * @return Preço por peso da empresa.
     */
    public double getPrecoPorPeso() { return this.precoPorPeso; }

    /**
     * Função que devolve o preço por hora de uma empresa.
     * @return Preço por Hora da empresa.
     */
    public double getPrecoPorHora(){
        return this.precoPorHora;
    }

    /**
     * Função que devolve a disponibilidade de uma empresa.
     * @return true se estiver disponível.
     */
    public boolean getAvailable() { return this.available; }

    /**
     * Função que devolve o Histórico de uma empresa.
     * @return Histórico da Empresa.
     */
    private Historico getHistorico(){
        return this.historico.clone();
    }

    /**
     * Função que devolve a PossibilidadeEntrega de uma empresa.
     * @return PossibilidadeEntrega da empresa.
     */
    private PossibilidadeEntrega getPossibilidadeEntrega(){
        return this.pe != null ? this.pe.clone() : null;
    }

    //Set's

    /**
     * Função que modifica a disponibilidade de uma empresa.
     * @param available Nova dispoibilidade.
     */
    public void setAvailable(boolean available) {
        this.available = available;
    }

    /**
     * Função que modifica a PossibilidadeEntrega de uma Empresa.
     * @param pe Nova PossibilidadeEntrega.
     */
    public void setPossibilidadeEntrega(PossibilidadeEntrega pe){
        this.pe = pe.clone();
    }

    //

    /**
     * Função que cria um clone de uma Empresa.
     * @return Empresa clonada.
     */
    public Empresa clone() {
        return new Empresa(this);
    }

    /**
     * Função que compara Empresas.
     * @param o, Objeto a comparar.
     * @return 'true' se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Empresa e = (Empresa) o;
        return super.equals(e) &&
               e.getNif()          == this.nif          &&
               e.getRaio()         == this.raio         &&
               e.getPrecoPorKm()   == this.precoPorKm   &&
               e.getPrecoPorPeso() == this.precoPorPeso &&
               e.getPrecoPorHora() == this.precoPorHora &&
               e.getAvailable()    == this.available    &&
               e.aceitoTransporteMedicamentos() == this.certificado &&
               e.getHistorico().equals(this.historico);
    }

    /**
     * Função que converte os parâmetros de uma Empresa para String.
     * @return String com os parâmetros da Empresa.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("NIF: ").append(this.nif).append('\n');
        sb.append("Raio: ").append(this.raio).append('\n');
        sb.append("Preço por km: ").append(this.precoPorKm).append('\n');
        sb.append("Preco por peso: ").append(this.precoPorPeso).append('\n');
        sb.append("Preco por hora: ").append(this.precoPorHora).append('\n');
        sb.append("Empresa ").append(this.certificado ? "certificada " : "não certificada").append(" para encomendas médicas!\n");
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
     * Função que cria uma Empresa a partir de uma linha.
     * @param linha, String a partir da qual se irá retirar os campos que permitem a criação de uma Empresa.
     * @return Empresa criado a partir da linha.
     * @throws NumberArgumentsLineException, Quando o número de argumentos for inválido.
     * @throws TypeConvertionException, Quando o tipo for inválido.
     */
    static public Empresa parse(String linha) throws NumberArgumentsLineException, TypeConvertionException {
        String[] campos = linha.split(",");
        if (campos.length != 7)
            throw new NumberArgumentsLineException();
        double lat,lon,raio, precoPorKm;
        int nif;
        try{ lat = Double.parseDouble(campos[2]);
            lon = Double.parseDouble(campos[3]);
            nif = Integer.parseInt(campos[4]);
            raio = Double.parseDouble(campos[5]);
            precoPorKm = Double.parseDouble(campos[6]);
        }
        catch(NumberFormatException | NullPointerException e) {
            throw new TypeConvertionException();
        }
        return new Empresa(campos[0],campos[1],new Localizacao(lat,lon),nif,raio,precoPorKm);
    }

    /**
     * Função que verifica se uma Empresa está numa Localização acessivel.
     * @param l Localização.
     * @return true se a localização da Empresa for menor ou igual à Localização dada.
     */
    public boolean acessivel(Localizacao l){
        return super.getLocalizacao().closerOrEqual(l,this.raio);
    }

    /**
     * Função que atribui uma entrega a uma Empresa.
     * @param pe PossibilidadeEntrega da Empresa.
     */
    public void entregar(PossibilidadeEntrega pe){
        this.setAvailable(false);
        this.setPossibilidadeEntrega(pe.clone());
        this.pe.start();
    }

    /**
     * Função que sucede uma entrega de uma Empresa.
     * @param tempo Tempo da entrega.
     * @return InfoHistorico com a informação da Encomenda entregue.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public InfoHistorico entregue(double tempo) throws EncomendaNotFoundException {
        try{
            if(this.pe.isEsperarResposta()) throw new EncomendaNotFoundException(Estado.ESPERARESPOSTA);
            PossibilidadeEntrega pe = this.pe.clone();
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
                                                 pe.getCusto());
            this.historico.add(ih);
            return ih.clone();
        } catch (NullPointerException e){
            throw new EncomendaNotFoundException("Sem entrega a fazer");
        }
    }

    /**
     * Função que devolve a distância total percorrida por uma Empresa.
     * @return Distância total de uma Empresa.
     */
    public double getDistanciaTotal(){
        return this.historico.getDistanciaTotal();
    }

    /**
     * Função que devolve o total faturado pelo empresa num intervalo de tempo.
     * @param inf Limite inferior.
     * @param sup Limite superior.
     * @return Total faturado pela empresa nesse intervalo.
     * @throws OrdemCronologicaErradaException Quando a ordem cronológica estiver incorreta.
     */
    public double getFaturadoIntervalo (LocalDateTime inf , LocalDateTime sup) throws OrdemCronologicaErradaException {
        return this.historico.getFaturadoIntervalo(inf,sup);
    }

    /**
     * Função que altera o Nif de uma Empresa.
     * @param nif Novo Nif.
     * @throws NotPositiveNumberException Quando o Nif for menor que 0.
     */
    public void setNif(int nif) throws NotPositiveNumberException{
        if(nif < 0) throw new NotPositiveNumberException("Nif inválido!");
        this.nif = nif;
    }

    /**
     * Função que altera o raio de uma Empresa.
     * @param raio Novo raio.
     * @throws NotPositiveNumberException Quando o raio for menor que 0.
     */
    public void setRaio(double raio) throws NotPositiveNumberException{
        if(raio < 0) throw new NotPositiveNumberException(raio);
        this.raio = raio;
    }

    /**
     * Função que faz uma Empresa aceitar uma entrega.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public void aceitar() throws EncomendaNotFoundException{
        if(this.pe == null) throw new EncomendaNotFoundException();
        this.pe.setAEntregar(super.getCodigo());
    }

    /**
     * Função que faz uma Empresa recusar uma entrega.
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
     * Fução que retorna o estado de entrega de uma Empresa.
     * @return Estado de entrega de uma Empresa.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public Estado getEstadoEntrega() throws EncomendaNotFoundException {
        if(this.pe == null) throw new EncomendaNotFoundException();
        return this.pe.getAtribuida();
    }

    /**
     * Função que devolve o código da Encomenda da entrega de uma Empresa.
     * @return Código da Encomenda.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public String getCodigoEncomenda() throws EncomendaNotFoundException{
        if(this.pe == null) throw new EncomendaNotFoundException();
        return this.pe.getCodigoEncomenda();
    }

    /**
     * Função que modifica o PreçoPorKm de uma Empresa.
     * @param precoPorKm Novo PreçoPorKm.
     * @throws NotPositiveNumberException Quando o preçoPorKm for menor que 0.
     */
    public void setPrecoPorKm(double precoPorKm) throws NotPositiveNumberException {
        if(precoPorKm < 0) throw new NotPositiveNumberException("Preço inválido!");
        this.precoPorKm= precoPorKm;
    }

    /**
     * Função que modifica o PreçoPorPeso de uma Empresa.
     * @param precoPorPeso Novo PreçoPorPeso.
     * @throws NotPositiveNumberException Quando o preço por peso for menor que 0.
     */
    public void setPrecoPorPeso(double precoPorPeso) throws NotPositiveNumberException{
        if(precoPorPeso < 0) throw new NotPositiveNumberException("Preço inválido!");
        this.precoPorPeso = precoPorPeso;
    }

    /**
     * Função que modifica o PreçoPorHora de uma Empresa.
     * @param precoPorHora Novo PreçoPorHora.
     * @throws NotPositiveNumberException Quando o preço por hora for menor que 0.
     */
    public void setPrecoPorHora(double precoPorHora) throws NotPositiveNumberException {
        if(precoPorHora < 0) throw new NotPositiveNumberException("Preço inválido!");
        this.precoPorHora = precoPorHora;
    }

    /**
     * Função que devolve o custo de uma Empresa.
     * @param peso Peso da encomenda.
     * @param distancia Distância da entrega.
     * @param tempo Tempo da entrega.
     * @return Custo da Empresa.
     */
    public double getCusto(double peso, double distancia, double tempo){
        return peso * this.precoPorPeso    +
               distancia * this.precoPorKm +
               tempo * this.precoPorHora;
    }

    /**
     * Função que classifica a entrega pela parte da Empresa.
     * @param encomenda Código da encomenda entregue.
     * @param classificacao Classificação atribuida.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     * @throws ClassificacaoInvalidaException Quando a classificação não se encontra entre 1 e 10, inclusive.
     */
    public void classificar(String encomenda, int classificacao) throws EncomendaNotFoundException, ClassificacaoInvalidaException{
        this.historico.classificar(encomenda,classificacao);
    }

    /**
     * Função que devolve a classificção média de uma Empresa.
     * @return Classificação Média da Empresa.
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
