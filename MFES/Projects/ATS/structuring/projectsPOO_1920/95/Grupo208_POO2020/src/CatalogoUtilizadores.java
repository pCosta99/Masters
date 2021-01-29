import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.concurrent.ThreadLocalRandom;

import Exception.*;

/**
 * Classe que lida com informação de vários Utilizadores.
 * 
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class CatalogoUtilizadores implements Serializable {
    // Instance Variables
    private Map<String, Utilizador> utilizadores; //Codigo, Utilizador
    private Map<String, String> credenciais;      //Mail,   Codigo

    // Constructors

    /**
     * Construtor de um CatalogoUtilizadores.
     */
    public CatalogoUtilizadores() {
        this.utilizadores = new HashMap<>();
        this.credenciais = new HashMap<>();
    }

    /**
     * Construtor de um CatálogoUtilizadores
     * @param utilizadores Utilizadores do Catálogo a construir.
     * @param credenciais Credencias dos Utilizadores do Catálogo a construir.
     */
    public CatalogoUtilizadores(Map<String, Utilizador> utilizadores, Map<String,String> credenciais) {
        this.setUtilizadores(utilizadores);
        this.setCredenciais(credenciais);
    }

    /**
     * Construtor de um CatálogoUtilizadores por cópia.
     * @param c CatalogoUtilizadores a copiar.
     */
    public CatalogoUtilizadores(CatalogoUtilizadores c) {
        this.utilizadores = c.getUtilizadores();
        this.credenciais = c.getCredenciais();
    }

    // Get

    /**
     * Função que devolve os Utilizadores do CatálogoUtilizadores.
     * @return Map com os Utilizadores do Catálogo.
     */
    private Map<String, Utilizador> getUtilizadores() {
        return this.utilizadores.entrySet()
                                .stream()
                                .collect(Collectors.toMap(Map.Entry::getKey,
                                                          v -> v.getValue().clone()));
    }

    /**
     * Função que devolve as credenciais dos Utilizadres do Catálogo.
     * @return Map com as Credenciais dos Utilizadores.
     */
    private Map<String, String> getCredenciais() {
        return this.credenciais.entrySet()
                                .stream()
                                .collect(Collectors.toMap(Map.Entry::getKey,
                                                          Map.Entry::getValue));
    }

    // Set

    /**
     * Função que modifica os Utilizadores do Catálogo.
     * @param u Novos utilizadores.
     */
    private void setUtilizadores(Map<String, Utilizador> u) {
        this.utilizadores = u.entrySet()
                             .stream()
                             .collect(Collectors.toMap(Map.Entry::getKey,
                                                       v -> v.getValue().clone()));
    }

    /**
     * Função que modifica as credenciais dos Utilizadores do Catálogo.
     * @param credenciais Novas credenciais.
     */
    private void setCredenciais(Map<String, String> credenciais) {
        this.credenciais = credenciais.entrySet()
                                      .stream()
                                      .collect(Collectors.toMap(Map.Entry::getKey,
                                                                Map.Entry::getValue));
    }

    //

    /**
     * Função que cria um clone de um CatalogoUtilizadores.
     * @return CatalogoUtilizadores clonado.
     */
    public CatalogoUtilizadores clone() {
        return new CatalogoUtilizadores(this);
    }

    /**
     * Função que compara CatalogoUtilizadores.
     * @param o Objeto a comparar.
     * @return true se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        CatalogoUtilizadores c = (CatalogoUtilizadores) o;
        return this.utilizadores.equals(c.getUtilizadores()) && 
               this.credenciais.equals(c.getCredenciais());
    }

    /**
     * Função que converte os parâmetros de um CatalogoUtilizadores em String.
     * @return String com os parâmetros de um CatalogoUtilizadores.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        this.utilizadores.values().forEach(u -> sb.append(u).append("\n"));
        return sb.toString();
    }
    
    //
    /**
     * Função que adiciona um Utilizador, assim como as suas credenciais, ao CatalogoUtilizadores.
     * @param u Utlizador a adicionar.
     */
    public void add(Utilizador u) {
        this.utilizadores.put(u.getCodigo(), u.clone());
        this.credenciais.put(u.getMail(), u.getCodigo());
    }

    /**
     * Função que devolve um Utilizador a partir de um código.
     * @param codigo Código dado.
     * @return Utilizador correspondente ao código.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    private Utilizador getUtilizador(String codigo) throws UtilizadorNotFoundException{
        Utilizador u = this.utilizadores.get(codigo);
        if(u == null) throw new UtilizadorNotFoundException(codigo);
        return u;
    }

    /**
     * Função que verifica se a Password correspondente a um Mail bate certo.
     * @param mail Mail a pesquisar.
     * @param password Password a verificar.
     * @return true se coincidir.
     * @throws MailNotRegisteredException Quando o Mail não estiver registado.
     */
    public boolean checkPassword(String mail, String password) throws MailNotRegisteredException {
        String u = this.credenciais.get(mail);
        if(u == null) throw new MailNotRegisteredException("O mail não está associado a nenhum utilizador!");
        return this.utilizadores.get(u).checkPassword(password);
    }

    /**
     * Função que gera um código.
     * @return String com o código gerado.
     */
    public String codeGenerator(){
        String s = null;
        while(s == null || this.utilizadores.containsKey(s)){
            StringBuilder sb = new StringBuilder();
            s = sb.append('u')
                  .append(ThreadLocalRandom.current()
                                           .nextInt(1,1000))
                  .toString();
        }
        return s;
    }

    /**
     * Função que regista um Utilizador.
     * @param nome Nome do Utilizador.
     * @param gps Localização do Utilizador.
     * @param mail Mail do Utilizador.
     * @param password Password do Utilizador.
     * @return Código do Utilizador.
     * @throws MailAlreadyRegisteredException Quando o Mail já estiver registado.
     */
    public String regista(String nome, Localizacao gps, String mail, String password) throws MailAlreadyRegisteredException {
        if(this.credenciais.containsKey(mail)) throw new MailAlreadyRegisteredException();
        String codigo = this.codeGenerator();
        this.add(new Utilizador(codigo, nome, mail, password, gps));
        return codigo;
    }

    /**
     * Função que devolve o código de um Utilizador a partir das suas credenciais.
     * @param mail Mail do Utilizador.
     * @param password Password do utilizador.
     * @return Código do Utilizador.
     * @throws CredenciaisErradasException Quando ou o Mail ou a Password estiverem incorretos.
     */
    public String getFromMailAndPassword(String mail, String password) throws CredenciaisErradasException{
        boolean correto;
        try { correto = this.checkPassword(mail,password); }
        catch(MailNotRegisteredException e) { throw new CredenciaisErradasException(); }
        if(!correto) throw new CredenciaisErradasException();
        return this.utilizadores.get(this.credenciais.get(mail))
                                .getCodigo();
    }

    /**
     * Função que verifica se um código está presente no Catálogo.
     * @param codigo Código a pesquisar.
     * @return true se estiver presente.
     */
    public boolean contains(String codigo){
        return this.utilizadores.containsKey(codigo);
    }

    /**
     * Função que devolve a Localização de um Utilizador, dado o seu código.
     * @param codigo Código do Utilizador.
     * @return Localização do dado Utilizador.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public Localizacao getGPS(String codigo) throws UtilizadorNotFoundException {
        return this.getUtilizador(codigo).getLocalizacao();
    }

    /**
     * Função que adiciona um pedido de Encomenda ao seu respetivo Utilizador.
     * @param pe Pedido de Encomenda.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public void addPedidoEncomenda(PossibilidadeEntrega pe) throws UtilizadorNotFoundException{
        this.getUtilizador(pe.getCodigoUtilizador()).addPedidoEncomenda(pe);
    }

    /**
     * Função que altera o Mail de um Utilizador.
     * @param cod Código do Utilizador.
     * @param newMail Novo Mail.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public void alterarMailUtilizador(String cod, String newMail) throws UtilizadorNotFoundException{
        Utilizador u = this.getUtilizador(cod);
        String mail = u.getMail();
        u.setMail(newMail);
        this.credenciais.remove(mail);
        this.credenciais.put(newMail,cod);
    }

    /**
     * Função que altera a Password de um Utilizador.
     * @param cod Código do Utilizador.
     * @param newPass Nova Password.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public void alterarPasswordUtilizador(String cod, String newPass) throws UtilizadorNotFoundException{
        this.getUtilizador(cod).setPassword(newPass);
    }

    /**
     * Função que devolve as Encomendas pendentes feitas por um Utilizador.
     * @param cod Código do Utilizador.
     * @return Map com as Encomendas pendentes e seu custo.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public Map<String,Double> getListPendentes(String cod) throws UtilizadorNotFoundException{
        return this.getUtilizador(cod).getListPendentes();
    }

    /**
     * Função que atribui uma resposta a uma Encomenda Pendente.
     * @param codUtilizador Código do Utilizador.
     * @param codEncomenda Código da Encomenda.
     * @return Informação sobre a encomenda pendente.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     */
    public PossibilidadeEntrega responder(String codUtilizador, String codEncomenda) throws UtilizadorNotFoundException,
                                                                                            EncomendaNotFoundException{
        return this.getUtilizador(codUtilizador).responder(codEncomenda);
    }

    /**
     * Função que adiciona a informação sobre uma Encomenda entregue à lista de encomendas entregues.
     * @param infoHistorico Informação sobre a Encomenda entregue.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public void entregue(InfoHistorico infoHistorico) throws UtilizadorNotFoundException{
        this.getUtilizador(infoHistorico.getCodUtilizador()).entregue(infoHistorico);
    }

    /**
     * Função que classifica a entrega de uma Encomenda.
     * @param utilizador Código do Utilizador que vai classificar.
     * @param encomenda Encomenda a ser classificada.
     * @param classificacao Classificação a atribuir.
     * @return Código do Transportador que entregou a Encomenda.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     * @throws EncomendaNotFoundException Quando a encomenda é inválida ou não for encontrada.
     * @throws ClassificacaoInvalidaException Quando a classificação não estiver entre 1 e 10, inclusive.
     */
    public String classificar(String utilizador, String encomenda, int classificacao) throws UtilizadorNotFoundException,
                                                                                             EncomendaNotFoundException,
                                                                                             ClassificacaoInvalidaException {
        return this.getUtilizador(utilizador).classificar(encomenda,classificacao);
    }

    /**
     * Função que determina o top10 utilizadores que fizeram mais Encomendas.
     * @return Lista com o top10 Utilizadores.
     */
    public List<String> listagemTop10Utilizadores(){
        return this.utilizadores.values()
                                .stream()
                                .map(u -> (Utilizador) u.clone())
                                .collect(Collectors.toMap(Utilizador::getCodigo,
                                                          Utilizador::getSize))
                                .entrySet()
                                .stream()
                                //Comparar int reversed e em caso de iguais, String natural
                                .sorted(Map.Entry.<String,Integer>comparingByValue(Comparator.reverseOrder())
                                                 .thenComparing(Map.Entry.comparingByKey()))
                                .limit(10)
                                .map(e -> String.format("%s %d",e.getKey(),e.getValue()))
                                .collect(Collectors.toList());
    }

    /**
     * Função que devolve a lista de Encomendas entregues, não classificadas, por parte de um Utilizador.
     * @param utilizador Código do Utilizador.
     * @return lista de Encomendas entregues, não classificadas, por parte de um Utilizador.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public List<String> getListNaoClassificados(String utilizador) throws UtilizadorNotFoundException {
        return this.getUtilizador(utilizador).getEntreguesNaoClassificados();
    }

    /**
     * Função que devolve a informação das Encomendas feitas ,por um Utilizador ,entregues num determinado intervalo de tempo.
     * @param utilizador Código do Utilizador.
     * @param inf Limite inferior.
     * @param sup Limite superior.
     * @param transportador Código do Transportador que efetuou a entrega.
     * @return Informação das encomendas entregues num determinado intervalo de tempo.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     * @throws OrdemCronologicaErradaException Quando a ordem cronológica estiver errada.
     */
    public Collection<String> getInfoHistoricoUtilizador(String utilizador,
                                                         LocalDateTime inf,
                                                         LocalDateTime sup,
                                                         String transportador) throws UtilizadorNotFoundException,
                                                                                      OrdemCronologicaErradaException {
        return this.getUtilizador(utilizador).getInfoHistoricoIntervaloUti(inf,sup,transportador);
    }

    /**
     * Função que devolve os dados de um Utilizador.
     * @param codigo Codigo do utilizador.
     * @return Dados do utilizador.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public String getDadosUtilizador(String codigo) throws UtilizadorNotFoundException {
        return this.getUtilizador(codigo).toString();
    }
}