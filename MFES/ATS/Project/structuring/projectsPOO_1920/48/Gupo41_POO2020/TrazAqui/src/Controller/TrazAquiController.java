package Controller;

import Model.*;
import Model.Exceptions.*;

import java.io.*;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

public class TrazAquiController implements ITrazAquiController{

    private ITrazAquiModel model;

    /**
     * Contrutor da classe TrazAquiController
     * @param model
     */
    public TrazAquiController(ITrazAquiModel model){
        this.model = model;
    }

    /**
     * Método que processa o registo de um utilizador
     * @param nome
     * @param email
     * @param password
     * @param nif
     * @param latitude
     * @param longitude
     * @throws EmailExistenteException
     */
    public void processaRegistaUtilizador(String nome, String email, String password, String nif, String latitude, String longitude) throws EmailExistenteException {
        model.registaUtilizador(nome,email,password,nif,Double.parseDouble(latitude),Double.parseDouble(longitude));
    }

    /**
     * Método que processa o registo de uma loja
     * @param nome
     * @param email
     * @param password
     * @param nif
     * @param latitude
     * @param longitude
     * @throws EmailExistenteException
     */
    public void processaRegistaLoja(String nome, String email, String password, String nif, String latitude, String longitude) throws EmailExistenteException {
        model.registaLoja(email,nif,nome,password,Double.parseDouble(latitude),Double.parseDouble(longitude));
    }

    /**
     * Método que processa o registo de um voluntário
     * @param nome
     * @param email
     * @param password
     * @param nif
     * @param latitude
     * @param longitude
     * @param raio
     * @throws EmailExistenteException
     */
    public void processaRegistaVoluntario(String nome, String email, String password,
                                          String nif, String latitude, String longitude, String raio, String velocidade) throws EmailExistenteException {
        model.registaVoluntario(email,nif,nome,password,Double.parseDouble(latitude),
                Double.parseDouble(longitude), Double.parseDouble(raio), Double.parseDouble(velocidade));
    }

    /**
     * Método que processa o registo de uma empresa
     * @param nome
     * @param email
     * @param password
     * @param nif
     * @param latitude
     * @param longitude
     * @param raio
     * @param precoKm
     * @throws EmailExistenteException
     */
    public void processaRegistaEmpresa(String nome, String email, String password, String nif, String latitude,
                                       String longitude, String raio, String precoKm, boolean medica,
                                       String velocidade, String precoPorKg) throws EmailExistenteException {
        model.registaEmpresaTransporte(email,nif,nome,password,Double.parseDouble(latitude),
                Double.parseDouble(longitude), Double.parseDouble(raio), Double.parseDouble(precoKm),
                medica, Double.parseDouble(velocidade), Double.parseDouble(precoPorKg));
    }

    /**
     * Método que processa o login de uma entidade
     * @param tipo
     * @param email
     * @param pass
     * @return
     * @throws EmailNaoValidoException
     * @throws PasswordNaoValidoException
     */
    public String processaLogin(String tipo, String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException {
        String res = "";
        switch (tipo){
            case "U":
                res = this.model.loginUtilizador(email, pass);
                break;
            case "L":
                res = this.model.loginLoja(email, pass);
                break;
            case "V":
                res = this.model.loginVoluntario(email, pass);
                break;
            case "E":
                res = this.model.loginEmpresa(email, pass);
                break;
            default:
                break;
        }
        return res;
    }

    /**
     * Método que processa um pedido de encomenda por parte de um utilizador
     * @param codUtil
     * @param codLoja
     * @param encomendas
     * @throws ProdutosNaoExisteException
     * @throws LojaInexistenteException
     */
    public void processaPedidoEncomenda(String codUtil, String codLoja, List<List<String>> encomendas, boolean medica) throws ProdutosNaoExisteException, LojaInexistenteException {
        this.model.pedidoEncomenda(codUtil, codLoja, encomendas, medica);
    }

    /**
     * Método que processa uma encomenda aceite por uma loja
     * @param codLoja
     * @param codEnc
     * @throws LojaInexistenteException
     * @throws EncomendaInexistenteException
     */
    public void processaEncomendaPronta(String codLoja, String codEnc) throws LojaInexistenteException, EncomendaInexistenteException {
        this.model.aceitaEncomendaLoja(codLoja, codEnc);
    }

    /**
    /**
     * Método que processa as encomendas em espera
     * para serem mostradas à empresa
     * @param codEmp
     * @return
     * @throws EmpresaInexistenteException
     */
    public String processaEncomendasParaEntrega(String codEmp) throws EmpresaInexistenteException {
        if(codEmp.charAt(0) == 't')
            return this.model.encomendasParaEntregaEmpresa(codEmp);
        else
            return this.model.encomendasParaEntregaVoluntario(codEmp);
    }

    /**
     * Método que processa as encomendas pedidas
     * a uma dada loja, para serem mostradas à mesma
     * @param codLoja
     * @return
     */
    public String processaEncomendasPedidas(String codLoja){
        return this.model.encomendasEmEspera(codLoja);
    }

    /**
     * Método que processa as encomendas aceites
     * por uma empresa de transporte, que aguardam
     * aprovação do utilizador
     * @param codUtil
     * @return
     */
    public String processaEncomendasAguardandoUtilizador(String codUtil){

        List<List<String>> encs = this.model.encomendasAguardandoUtilizador(codUtil);

        if (encs.isEmpty())
            return "";

        StringBuilder sb = new StringBuilder();

        for (List<String> aux : encs){
            sb.append("Encomenda: ");
            sb.append(aux.get(0)).append("\n");
            sb.append("Transportadora: ");
            sb.append(aux.get(1)).append("\n");
            sb.append("Nome: ");
            sb.append(aux.get(2)).append("\n");
            sb.append("Custo: ");
            sb.append(aux.get(3)).append("\n");
            sb.append("Tempo: ");
            sb.append(aux.get(4)).append("\n");
            sb.append("\n");
        }
        return sb.toString();
    }

    /**
     * Método responsável por aceitar o transporte
     * de uma encomenda por parte do utilizador,
     * através de uma empresa de transporte
     * @param codEnc
     */
    public void processaAceitaEncomendaUtilizador(String codEnc, String codTrans) throws EncomendaInexistenteException, EmpresaInexistenteException{

        this.model.aceitaEncomendaUtilizador(codEnc, codTrans);
    }

    /**
     * Método responsável por recusar o transporte
     * de uma encomenda por parte do utilizador,
     * através de uma empresa de transporte
     * @param codEnc
     */
    public void processaRecusaEncomendaUtilizador(String codEnc, String codTrans) throws EncomendaInexistenteException, EmpresaInexistenteException{

        this.model.recusaEncomendaUtilizador(codEnc, codTrans);
    }

    /**
     * Método responsável por processar uma consulta de faturação de uma empresa
     * @param cod
     * @param dataI
     * @param dataF
     * @return
     */
    public double processaFaturacaoEmp(String cod, LocalDate dataI, LocalDate dataF) throws EmpresaInexistenteException{
        return this.model.faturacaoEmpresa(cod, dataI, dataF);
    }

    /**
     * Método responsável por processar uma consulta de faturação de uma empresa
     * @param cod
     * @param dataI
     * @param dataF
     * @return
     */
    public double processaFaturacaoLoja(String cod, LocalDate dataI, LocalDate dataF) throws LojaInexistenteException{
        return this.model.faturacaoLoja(cod, dataI, dataF);
    }

    /**
     * Método que processa a classificação de uma encomenda
     * @param codEnc
     * @param classificacao
     */
    public void processaClassificarEncomenda(String codEnc, int classificacao) throws EncomendaInexistenteException{
        this.model.classificarEntrega(codEnc, classificacao);
    }

    /**
     * Método que processa o histórico de encomendas feitas
     * por um utilizador para serem apresentadas na view
     * @param codUt
     */
    public String processaHistoricoEntregasUt(String codUt){
        List<String> historico= this.model.historicoEncomendasUtilizador(codUt);

        StringBuilder sb = new StringBuilder();

        for (String s : historico){
            sb.append(s);
            sb.append("\n");
        }

        return sb.toString();
    }

    /**
     * Método que processa o histórico de encomendas feitas
     * por uma Loja para serem apresentadas na view
     * @param codLoja
     */
    public String processaHistoricoEntregasLoja(String codLoja){
        List<String> historico= this.model.historicoEncomendasLoja(codLoja);

        StringBuilder sb = new StringBuilder();

        for (String s : historico){
            sb.append(s);
            sb.append("\n");
        }

        return sb.toString();
    }

    /**
     * Método que processa o histórico de encomendas feitas
     * por uma Loja para serem apresentadas na view
     * @param codTransportadora
     */
    public String processaHistoricoEntregasTransportadora(String codTransportadora){
        List<String> historico= this.model.historicoEncomendasTransportadora(codTransportadora);

        StringBuilder sb = new StringBuilder();

        for (String s : historico){
            sb.append(s);
            sb.append("\n");
        }

        return sb.toString();
    }

    /**
     * Método que devolve os produtos disponiveis
     * para serem vistos pelo utilizador
     * @return
     */
    public String processaVerProdutos(){
        return this.model.verProdutos();
    }

    /**
     * Método que processa o registo de um produto
     * por parte de uma loja
     * @param codLoja
     * @param descricao
     * @param peso
     * @param preco
     */
    public void processaRegistaProduto(String codLoja, String descricao, String peso, String preco){
        this.model.adicionaProdutoLoja(codLoja, descricao, Double.parseDouble(peso), Double.parseDouble(preco));
    }

    /**
     * Método que processa o pedido de transporte de
     * uma encomenda por parte de uma empresa
     *
     * @param codEmp
     * @param codEnc
     */
    public void processaPedidoTransporteEncomenda(String codEmp, String codEnc) throws EncomendaInexistenteException{
        this.model.pedidoTransporteEncomenda(codEmp,codEnc);
    }

    /**
     * Método que devolve os códigos das encomendas aceites
     * pelos utilizadores e que aguardam o transporte por
     * parte da empresa transportadora
     * @param codTrans
     * @return
     */
    public String processaEncomendasAceitesUtilizador(String codTrans){
        return this.model.encomendasAceitesUtilizador(codTrans);
    }

    /**
     * Método que devolve uma string com os códigos da
     * encomendas que a empresa pode transportar
     *
     * @param codEmp
     */
    public String processaEncomendaParaTransporte(String codEmp){
        StringBuilder sb = new StringBuilder();

        List<String> res = this.model.encomendasEmTransporteEmpresa(codEmp);
        if(!res.isEmpty()) {
            for (String s : res) {
                sb.append(s).append("\n");
            }
        }

        return sb.toString();
    }

    /**
     * Método que processa as encomendas que o
     * voluntário pode transportar
     *
     * @param codVoluntario
     * @return
     */
    public String processaEncomendasParaEntregaVoluntario(String codVoluntario){
        return this.model.encomendasParaEntregaVoluntario(codVoluntario);
    }

    /**
     * Método que sinaliza uma encomenda como
     * transportada por uma empresa
     *
     * @param codTrans
     * @param codEnc
     */
    public void processaTransportarEncomendaEmpresa(String codTrans, String codEnc) throws EncomendaInexistenteException, TransportadoraNaoMedicaException, EncomendaNaoAceiteUtilizadorException {
        this.model.transportaEncomendaEmpresa(codTrans, codEnc);
    }

    /**
     * Método que sinaliza uma encomenda como
     * transportada por um voluntario
     *
     * @param codTrans
     * @param codEnc
     */
    public void processaTransportarEncomendaVoluntario(String codTrans, String codEnc) throws EncomendaInexistenteException, TransportadoraNaoMedicaException {
        this.model.transportaEncomendaVoluntario(codTrans, codEnc);
    }

    /**
     * Método que processa o resultado da consulta dos dez
     * utilizadores mais ativos do sistema
     */
    public String processaUtilizadoresMaisAtivos(){
        return this.model.utilizadoresMaisAtivos();
    }

    /**
     * Método que processa o resultado da consulta das dez
     * empresas mais ativas do sistema (mais Kms)
     * @return
     */
    public String processaEmpresaMaisAtivas(){
        return this.model.empresasMaisAtivas();
    }

    /**
     * Processa a gravação em ficheiro de objetos do estado atual da aplicação
     * @param nomeFicheiro
     * @return
     * @throws FileNotFoundException
     * @throws IOException
     */
    public boolean processaGuardaEstado(String nomeFicheiro) throws FileNotFoundException, IOException {
        return this.model.guardaEstado(nomeFicheiro);
    }

    /**
     * Processo o carregamento do estado da aplicação a partir de um ficheiro de objetos
     * @param nomeFicheiro
     * @return
     * @throws FileNotFoundException
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public boolean processaCarregarEstado(String nomeFicheiro) throws FileNotFoundException, IOException, ClassNotFoundException{
        FileInputStream fis = new FileInputStream(nomeFicheiro);
        ObjectInputStream ois = new ObjectInputStream(fis);

        this.model = (TrazAquiModel) ois.readObject();
        ois.close();
        return true;
    }

    /**
     * Método encarregue de atualizar o tempo médio de espera de cada encomenda numa determinada loja
     * @param tempoEspera
     */
    public void processaAtualizaTempoEspera (String codLoja, Duration tempoEspera){
        this.model.atualizaTempoEspera(codLoja, tempoEspera);
    }

    /**
     * Método encarregue de sinalizar que uma transportadora acabou de entregar uma encomenda com sucesso
     * @param codTransportadora
     * @param codEnc
     */
    public void processaEncomendaEntregue (String codTransportadora, String codEnc, Duration tempo) throws EncomendaNaoTransportadaException{
        this.model.encomendaEntregue(codTransportadora, codEnc, tempo);
    }

    /**
     * Permite à loja atualizar o tamanho da fila de espera para levantar encomendas
     * @param codLoja
     * @param tamanho
     */
    public void processaTamanhoFila(String codLoja, int tamanho) throws FilaDeEsperaException{
        this.model.atualizaFilaDeEspera(codLoja, tamanho);
    }

    /**
     * Permite atualizar a velocidade média de uma empresa
     * @param codEmpresa
     * @param vel
     */
    public void processaAtualizaVelocidadeEmp(String codEmpresa, double vel){
        this.model.atualizaVelMediaEmp(codEmpresa, vel);
    }

    /**
     * Permite atualizar a velocidade média de um voluntário
     * @param coVol
     * @param vel
     */
    public void processaAtualizaVelocidadeVol(String coVol, double vel){
        this.model.atualizaVelMediaVoluntario(coVol, vel);
    }

    /**
     * Devolve o histórico das encomendas processadas por uma determinada empresa ou voluntario
     * num determinado período de tempo
     * @param cod
     * @param di
     * @param df
     * @return
     */
    public String processaHistoricoEncomendasTransportadoraData(String cod, LocalDate di, LocalDate df)
            throws EmpresaInexistenteException, VoluntarioInexistenteException{
        List<String> historico =  this.model.historicoEncomendasTransportadoraData(cod, di, df);

        StringBuilder sb = new StringBuilder();

        for (String s : historico){
            sb.append(s);
            sb.append("\n");
        }

        return sb.toString();
    }

    /**
     * Permite a uma empresa transportadora atualizar o preço por Km a cobrar
     * @param codEmp
     * @param preco
     */
    public void processaAtualizaPrecoKm(String codEmp, double preco){
        this.model.atualizaPrecoKm(codEmp, preco);
    }

    /**
     * Permite a uma empresa transportadora atualizar o preço por Kg a cobrar
     * @param codEmp
     * @param preco
     */
    public void processaAtualizaPrecoKg(String codEmp, double preco){
        this.model.atualizaPrecoKg(codEmp, preco);
    }

    /**
     * Processa o pedido de transporte de várias encomendas para vários utilizadores
     * @param codTransp
     * @param encs
     * @throws EncomendaInexistenteException
     */
    public void processapedidoTransporteEncomendaGrupo(String codTransp, List<String> encs) throws EncomendaInexistenteException{
        this.model.pedidoTransporteEncomendaGrupo(codTransp, encs);
    }

    /**
     * Processa o inicio do processo de transporte de um conjunto de encomendas para vários utilizadores
     * @param codTransp
     * @param encs
     * @throws EncomendaInexistenteException
     * @throws TransportadoraNaoMedicaException
     * @throws EncomendaNaoAceiteUtilizadorException
     */
    public void processatransportaParaUtilizadoresGrupo(String codTransp, List<String> encs)
            throws EncomendaInexistenteException, TransportadoraNaoMedicaException, EncomendaNaoAceiteUtilizadorException{
        this.model.transportaParaUtilizadoresGrupo(codTransp, encs);
    }

    /**
     * Método que devolve a metereologia atual
     * @return
     */
    public int processaGetMetereologia(){
        return this.model.getMetereologia();
    }

    /**
     * Método que devolve o trânsito atual
     * @return
     */
    public int processaGetTransito(){
        return this.model.getTransito();
    }

    /**
     * Método responsável por devolver o nome de um utilizador
     * @param codUt
     * @return
     */
    public String processaGetNomeUt(String codUt){
        return this.model.getNomeUtilizador(codUt);
    }
}
