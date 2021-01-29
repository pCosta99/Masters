import java.time.LocalDateTime;
import java.util.*;
import java.io.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import Enums.Estado;
import Enums.Tipo;
import Exception.*;

/**
 * Escreva a descrição da classe BaseDeDados aqui.
 * 
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class BaseDeDados implements Serializable {
    // Instance Variable
    private CatalogoUtilizadores  utilizadores;
    private CatalogoLojas         lojas;
    private CatalogoEncomendas    encomendas;
    private CatalogoTransportador transportadores;

    private Map<String,List<String>>   aceites;           //Loja - Encomendas a confirmar pelas lojas
    private List<String>               availables;        //Voluntário e Empresas que podem fazer entrega
    private List<PossibilidadeEntrega> aEntregar;         //Códigos de Encomendas a entregar
    private List<String>               entregues;         //Códigos de Encomendas entregues
    private List<String>               invalidas;         //Lista de Encomendas com algum erro(loja, utilizador)

    // Constructors

    /**
     * Construtor de uma BaseDeDados.
     */
    public BaseDeDados() {
        this.utilizadores    = new CatalogoUtilizadores();
        this.lojas           = new CatalogoLojas();
        this.encomendas      = new CatalogoEncomendas();
        this.aceites         = new HashMap<>();
        this.availables      = new ArrayList<>();
        this.aEntregar       = new ArrayList<>();
        this.entregues       = new ArrayList<>();
        this.invalidas       = new ArrayList<>();
        this.transportadores = new CatalogoTransportador();

    }

    /**
     * Construtor de uma BaseDeDados.
     * @param bd, BaseDeDados a construir.
     */
    public BaseDeDados(BaseDeDados bd) {
        this.utilizadores    = bd.getUtilizadores();
        this.lojas           = bd.getLojas();
        this.encomendas      = bd.getEncomendas();
        this.aceites         = bd.getAceites();
        this.availables      = bd.getAvailables();
        this.aEntregar       = bd.getAEntregar();
        this.entregues       = bd.getEntregues();
        this.invalidas       = bd.getInvalidas();
        this.transportadores = bd.getTransportadores();
    }

    /**
     * Construtor de uma BaseDeDados.
     * @param fileName, FilePath a partir do qual se irá construir uma BaseDeDados.
     */
    public BaseDeDados(String fileName) {
        this();
        List<String> linhas = Reader.readFile(fileName);
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            try {
                if(linhaPartida.length != 2) throw new NumberArgumentsLineException();
                switch (linhaPartida[0]) {
                    case "Utilizador":
                        Utilizador u = Utilizador.parse(linhaPartida[1]);
                        this.utilizadores.add(u);
                        break;
                    case "Voluntario":
                        Voluntario v = Voluntario.parse(linhaPartida[1]);
                        this.transportadores.add(v);
                        break;
                    case "Transportadora":
                        Empresa emp = Empresa.parse(linhaPartida[1]);
                        this.transportadores.add(emp);
                        break;
                    case "Loja":
                        Loja l = Loja.parse(linhaPartida[1]);
                        this.lojas.add(l);
                        this.aceites.put(l.getCodigo(),new ArrayList<>());
                        break;
                    case "Encomenda":
                        Encomenda enc = Encomenda.parse(linhaPartida[1]);
                        this.add(enc);
                        break;
                    case "Aceite":
                        this.entregarEncomenda(linhaPartida[1]);
                        break;
                }
            } catch(NumberArgumentsLineException  |
                    TypeConvertionException       |
                    EncomendaNotFoundException    |
                    UtilizadorNotFoundException   |
                    LojaNotFoundException ignored){} //Ignorar linhas com erro(s)
        }
    }

    // Gets

    /**
     * Função que retorna um catalogo de utilizadores.
     * @return CatalogoUtilizadores.
     */
    private CatalogoUtilizadores getUtilizadores() {
        return this.utilizadores.clone();
    }

    /**
     * Função que retorna um catálogo de lojas.
     * @return CatalogoLojas.
     */
    private CatalogoLojas getLojas() {
        return this.lojas.clone();
    }

    /**
     * Função que retorna um catalogo de trasnportadores.
     * @return CatalogoTransportadores.
     */
    private CatalogoTransportador getTransportadores(){
        return this.transportadores.clone();
    }

    /**
     * Função que retorna um catalogo de encomendas.
     * @return CatalogoEncomendas.
     */
    private CatalogoEncomendas getEncomendas() {
        return this.encomendas.clone();
    }

    /**
     * Função que retorna um Map, onde a String diz respeito ao código de lojas e a List<String> diz respeito às encomendas a confirmar pelas lojas.
     * @return Map.
     */
    private Map<String,List<String>> getAceites(){
        return this.aceites.entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getKey,
                        l -> new ArrayList<>(l.getValue())));
    }

    /**
     * Função que retorna uma List<String> onde as string dizem respeito aos codigos de empresas e voluntarios disponiveis.
     * @return List.
     */
    private List<String> getAvailables(){
        return new ArrayList<>(this.availables);
    }

    /**
     * Função que retorna uma List<PossibilidadeEntrega> que diz respeito às encomendas que estão por entregar.
     * @return List.
     */
    private List<PossibilidadeEntrega> getAEntregar(){
        return new ArrayList<>(this.aEntregar);
    }

    /**
     * Função que retorna uma List<String> que diz respeito às encomendas que já foram entregues.
     * @return List.
     */
    private List<String> getEntregues(){
        return new ArrayList<>(this.entregues);
    }

    /**
     * Função que retorna uma List<String> que diz respeito às encomendas que contêm algum erro.
     * @return List.
     */
    private List<String> getInvalidas(){
        return new ArrayList<>(this.invalidas);
    }

    //

    /**
     * Função que cria um clone de uma BaseDeDados.
     * @return BaseDeDados clonada.
     */
    public BaseDeDados clone() {
        return new BaseDeDados(this);
    }

    /**
     * Função que compara parametros da BaseDeDados.
     * @param o, Objeto a comparar.
     * @return 'true' se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        BaseDeDados bd = (BaseDeDados) o;
        return this.utilizadores.equals(bd.getUtilizadores()) &&
                this.lojas.equals(bd.getLojas())               &&
                this.encomendas.equals(bd.getEncomendas())     &&
                this.aceites.equals(bd.getAceites())           &&
                this.availables.equals(bd.getAvailables())     &&
                this.aEntregar.equals(bd.getAEntregar())       &&
                this.entregues.equals(bd.getEntregues())       &&
                this.invalidas.equals(bd.getInvalidas())       &&
                this.transportadores.equals(bd.getTransportadores());

    }

    /**
     * Função que converte os parametros da BaseDeDados para string.
     * @return String com os parametros convertidos.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.utilizadores).append('\n')
                .append(this.lojas).append('\n')
                .append(this.transportadores).append('\n')
                .append(this.encomendas).append('\n');
        return sb.toString();
    }

    //

    /**
     * Função que grava o estado.
     * @param filename, Nome do ficheiro onde será guardado.
     * @throws IOException, Quando ocorre um erro de IO.
     */
    public void gravarEstado(String filename) throws IOException{
        ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(filename));
        o.writeObject(this);
        o.flush();
        o.close();
    }

    /**
     * Função que carrega um estado e a partir do qual se irá construir uma BaseDeDados que será retornada.
     * @param filename, Nome do ficheiro a partir do qual o estado será carregado.
     * @return BaseDeDados.
     * @throws IOException, Quando ocorre um erro de IO.
     * @throws ClassNotFoundException, Quando a classe não é encontrada.
     * @throws ClassCastException, Quando ocorre um erro de cast da classe.
     */
    public static BaseDeDados carregarEstado(String filename) throws IOException, ClassNotFoundException, ClassCastException {
        ObjectInputStream o = new ObjectInputStream(new FileInputStream(filename));
        BaseDeDados bd = (BaseDeDados) o.readObject();
        o.close();
        return bd;
    }

    //////////////////////////////////////////////////////// ADDS //////////////////////////////////////////////////////

    /**
     * Função que adiciona uma encomenda ao CatalogoEncomenndas e caso seja válida aceita. Caso contrário adiciona-a também às encomendas inválidas.
     * @param e, Encomenda a adicionar.
     */
    public void add(Encomenda e) {
        this.encomendas.add(e);
        if(this.encomendaValida(e))
            try { this.aceitar(e.getCodLoja(),e.getCodEncomenda()); }
            catch (LojaNotFoundException ignored) {} //Nao acontece porque foi verificado
        else
            this.invalidas.add(e.getCodEncomenda());
    }

    ////////////////////////////////////////////////////// REGISTA /////////////////////////////////////////////////////

    /**
     * Função que regista um utilizador.
     * @param nome, Nome do utilizador a registar.
     * @param gps, Localização do utilizador a registar.
     * @param mail, Mail do utilizador a registar.
     * @param password, Password do utilizador a registar.
     * @return Código do utilizador.
     * @throws MailAlreadyRegisteredException, Quando o Mail já estiver registado.
     */
    public String registaUtilizador(String nome, Localizacao gps, String mail, String password) throws MailAlreadyRegisteredException {
        return this.utilizadores.regista(nome,gps,mail,password);
    }

    /**
     * Função que regista um voluntário.
     * @param nome, Nome do voluntário a registar.
     * @param gps, Localização do voluntário a registar.
     * @param mail, Mail do voluntário a registar.
     * @param password, Password do voluntário a registar.
     * @return Código do voluntário.
     * @throws MailAlreadyRegisteredException, Quando o Mail já estiver registado.
     */
    public String registaVoluntario(String nome, Localizacao gps, String mail, String password) throws MailAlreadyRegisteredException{
        return this.transportadores.registaVoluntario(nome,gps,mail,password);
    }

    /**
     * Função que regista uma empresa.
     * @param nome, Nome da empresa a registar.
     * @param gps, Localização da empresa a registar.
     * @param mail, Mail da empresa a registar.
     * @param password, Password da empresa a registar.
     * @return Código da empresa.
     * @throws MailAlreadyRegisteredException, Quando o Mail já estiver registado.
     */
    public String registaEmpresa(String nome, Localizacao gps, String mail, String password) throws MailAlreadyRegisteredException{
        return this.transportadores.registaEmpresa(nome,gps,mail,password);
    }

    /**
     * Função que regista uma loja.
     * @param nome, Nome da loja a registar.
     * @param gps, Localização da loja a registar.
     * @param mail, Mail da loja a registar.
     * @param password, Password da loja a registar.
     * @return Código da loja.
     * @throws MailAlreadyRegisteredException, Quando o Mail já estiver registado.
     */
    public String registaLoja(String nome, Localizacao gps, String mail, String password) throws MailAlreadyRegisteredException{
        return this.lojas.regista(nome,gps,mail,password);
    }

    ///////////////////////////////////////////// Get com mail e password) /////////////////////////////////////////////

    /**
     * Função que devolve o código de um Utilizador a partir das suas credenciais.
     * @param mail Mail do Utilizador.
     * @param password Password do utilizador.
     * @return Código do Utilizador.
     * @throws CredenciaisErradasException Quando ou o Mail ou a Password estiverem incorretos.
     */
    public String getUtilizador(String mail, String password) throws CredenciaisErradasException {
        return this.utilizadores.getFromMailAndPassword(mail,password);
    }

    /**
     * Função que devolve o código de um Voluntario a partir das suas credenciais.
     * @param mail Mail do Voluntario.
     * @param password Password do Voluntario.
     * @return Código do Voluntario.
     * @throws CredenciaisErradasException Quando ou o Mail ou a Password estiverem incorretos.
     */
    public String getVoluntario(String mail, String password) throws CredenciaisErradasException {
        return this.transportadores.getVoluntarioFromMailAndPassword(mail,password);
    }

    /**
     * Função que devolve o código de uma empresa a partir das suas credenciais.
     * @param mail Mail da Empresa.
     * @param password Password da Empresa.
     * @return Código da Empresa.
     * @throws CredenciaisErradasException Quando ou o Mail ou a Password estiverem incorretos.
     */
    public String getEmpresa(String mail, String password) throws CredenciaisErradasException {
        return this.transportadores.getEmpresaFromMailAndPassword(mail,password);
    }

    /**
     * Função que devolve o código de uma loja a partir das suas credenciais.
     * @param mail Mail da Loja.
     * @param password Password da Loja.
     * @return Código da Loja.
     * @throws CredenciaisErradasException Quando ou o Mail ou a Password estiverem incorretos.
     */
    public String getLoja(String mail, String password) throws CredenciaisErradasException {
        return this.lojas.getFromMailAndPassword(mail,password);
    }

    /////////////////////////////////////////////////  Alterar mail  ///////////////////////////////////////////////////

    /**
     * Função que altera o Mail de um Utilizador.
     * @param cod Código do Utilizador.
     * @param newMail Novo Mail.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public void alterarMailUtilizador(String cod, String newMail) throws UtilizadorNotFoundException{
        this.utilizadores.alterarMailUtilizador(cod,newMail);
    }

    /**
     * Função que altera o Mail de um Voluntario.
     * @param cod Código do Voluntario.
     * @param newMail Novo Mail.
     * @throws VoluntarioNotFoundException Quando o voluntario é inválido ou não for encontrado.
     */
    public void alterarMailVoluntario(String cod, String newMail) throws VoluntarioNotFoundException{
        this.transportadores.alterarMailVoluntario(cod,newMail);
    }

    /**
     * Função que altera o Mail de uma Empresa.
     * @param cod Código da Empresa.
     * @param newMail Novo Mail.
     * @throws EmpresaNotFoundException Quando a Empresa é inválida ou não for encontrada.
     */
    public void alterarMailEmpresa(String cod, String newMail) throws EmpresaNotFoundException{
        this.transportadores.alterarMailEmpresa(cod,newMail);
    }

    /**
     * Função que altera o Mail de uma Loja.
     * @param cod Código da Loja.
     * @param newMail Novo Mail.
     * @throws LojaNotFoundException Quando a Loja é inválida ou não for encontrada.
     */
    public void alterarMailLoja(String cod, String newMail) throws LojaNotFoundException{
        this.lojas.alterarMailLoja(cod,newMail);
    }

    /////////////////////////////////////////////  Alterar password  ///////////////////////////////////////////////////

    /**
     * Função que altera a password de um Utilizador.
     * @param cod Código do Utilizador.
     * @param newPass Nova password.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public void alterarPasswordUtilizador(String cod, String newPass) throws UtilizadorNotFoundException{
        this.utilizadores.alterarPasswordUtilizador(cod,newPass);
    }

    /**
     * Função que altera a password de um Voluntario.
     * @param cod Código do Voluntario.
     * @param newPass Nova password.
     * @throws VoluntarioNotFoundException Quando o voluntario é inválido ou não for encontrado.
     */
    public void alterarPasswordVoluntario(String cod, String newPass) throws VoluntarioNotFoundException{
        this.transportadores.alterarPasswordVoluntario(cod,newPass);
    }

    /**
     * Função que altera a password de uma empresa.
     * @param cod Código da Empresa.
     * @param newPass Nova password.
     * @throws EmpresaNotFoundException Quando a Empresa é inválida ou não for encontrada.
     */
    public void alterarPasswordEmpresa(String cod, String newPass) throws EmpresaNotFoundException{
        this.transportadores.alterarPasswordEmpresa(cod,newPass);
    }

    /**
     * Função que altera a password de uma Loja.
     * @param cod Código da Loja.
     * @param newPass Nova password.
     * @throws LojaNotFoundException Quando a Loja é inválida ou não for encontrada.
     */
    public void alterarPasswordLoja(String cod, String newPass) throws LojaNotFoundException{
        this.lojas.alterarPasswordLoja(cod,newPass);
    }

    //////////////////////////////////////////////////////  LOJAS  /////////////////////////////////////////////////////

    /**
     * Função que descreve a situação de um Encomenda ser aceite por uma Loja.
     * @param codLoja Código da Loja.
     * @param codEncomenda Código da Encomenda.
     * @throws LojaNotFoundException Quando a Loja é inválida ou não for encontrada.
     */
    private void aceitar(String codLoja, String codEncomenda) throws LojaNotFoundException{
        List<String> loja = this.aceites.get(codLoja);
        if(loja == null) throw new LojaNotFoundException(codLoja);
        loja.add(codEncomenda);
        this.lojas.incCliente(codLoja);
    }

    /**
     * Funçao que devolve a loja de uma Encomenda.
     * @param encomenda Código da Encomenda.
     * @return Código da Loja.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     */
    private String getLojaEncomenda(String encomenda) throws EncomendaNotFoundException{
        Iterator<String> it = this.aceites.keySet().iterator();
        boolean found = false;
        String loja = null;
        while(!found && it.hasNext()){
            loja = it.next();
            found = this.aceites.get(loja).contains(encomenda);
        }
        if(!found) throw new EncomendaNotFoundException();
        assert(loja!=null);
        return loja;
    }

    /**
     * Função responsável pela entrega de uma Encomenda.
     * @param encomenda Código da Encomenda.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     * @throws LojaNotFoundException Quando a Loja é inválida ou não for encontrada.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    private void entregarEncomenda(String encomenda) throws EncomendaNotFoundException,
                                                            LojaNotFoundException,
                                                            UtilizadorNotFoundException {
        String loja = this.getLojaEncomenda(encomenda);
        this.entregarEncomenda(loja,encomenda);
    }

    /**
     * Função que devolve a Localização de uma loja.
     * @param codigo Código da Loja.
     * @return Localização da Loja.
     * @throws LojaNotFoundException Quando a Loja é inválida ou não for encontrada.
     */
    private Localizacao getGPSLoja(String codigo) throws LojaNotFoundException{
        return this.lojas.getGPS(codigo);
    }

    /**
     * Função que devolve os códigos de todas as lojas.
     * @return List com os códigos das lojas.
     */
    public List<String> getAllCodigoLoja(){
        return this.lojas.getAllCodigoLoja();
    }

    /**
     * Função que adiciona uma Encomenda ao catálogo.
     * @param codUtilizador Código do Utilizador.
     * @param codLoja Código da Loja.
     * @param peso Peso da Encomenda.
     * @param les LinhasEncomenda.
     * @param certificado Diz se a encomenda é médica ou não.
     * @return Código da Encomenda.
     * @throws LojaNotFoundException Quando a Loja é inválida ou não for encontrada.
     * @throws EncomendaInvalidaException Quando a Encomenda é inválida ou não for encontrada.
     */
    public String add(String codUtilizador,String codLoja, double peso, List<LinhaEncomenda> les, boolean certificado) throws LojaNotFoundException,
                                                                                                                              EncomendaInvalidaException {
        if (!this.lojas.contains(codLoja)) throw new LojaNotFoundException(codLoja);
        String codEncomenda = this.encomendas.add(codUtilizador, codLoja, peso, les, certificado);
        this.aceitar(codLoja, codEncomenda);
        return codEncomenda;
    }

    /**
     * Função que devolve todas as Lojas com entregas nas aceites.
     * @param codLoja Código da Loja.
     * @return List com os códigos das Lojas.
     * @throws LojaNotFoundException Quando a Loja é inválida ou não for encontrada.
     */
    public List<String> getEntregasLoja(String codLoja) throws LojaNotFoundException{
        List<String> entregas = this.aceites.get(codLoja);
        if(entregas == null) throw new LojaNotFoundException(codLoja);
        return new ArrayList<>(entregas);
    }

    /**
     * Função que altera o tempo médio de uma loja.
     * @param cod Código da Loja.
     * @param tempoMedio Novo tempo médio.
     * @throws LojaNotFoundException Quando a Loja é inválida ou não for encontrada.
     * @throws NotPositiveNumberException Quando o número é menor que 0.
     */
    public void alterarTempoMedioLoja(String cod, double tempoMedio) throws LojaNotFoundException, NotPositiveNumberException {
        this.lojas.alterarTempoMedioLoja(cod,tempoMedio);
    }

    /**
     * Função que devolve os dados de um utilizador.
     * @param codigo Código do utilizador.
     * @return Dados do utilizador.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public String getDadosLoja(String codigo) throws UtilizadorNotFoundException {
        return this.utilizadores.getDadosUtilizador(codigo);
    }

    ////////////////////////////////////////////////////  ENCOMENDAS  //////////////////////////////////////////////////

    /**
     * Função que verifica se uma encomenda é válida.
     * @param e  Encomenda a verificar
     * @return true se for válida.
     */
    public boolean encomendaValida(Encomenda e){
        return this.utilizadores.contains(e.getCodUtilizador()) &&
               this.lojas.contains(e.getCodLoja());
    }

    /**
     * Função que retrata a entrega de uma encomenda.
     * @param loja Código da Loja.
     * @param encomenda Código da Encomenda.
     * @throws LojaNotFoundException Quando a Loja é inválida ou não for encontrada.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     * @throws UtilizadorNotFoundException Quando o utilizador é inválido ou não for encontrado.
     */
    public void entregarEncomenda(String loja, String encomenda) throws LojaNotFoundException,
                                                                        EncomendaNotFoundException,
                                                                        UtilizadorNotFoundException {
        List<String> aceitesLoja = this.aceites.get(loja);
        if(aceitesLoja == null) throw new LojaNotFoundException(loja);
        if(!aceitesLoja.remove(encomenda)) throw new EncomendaNotFoundException(encomenda);
        Encomenda e = this.encomendas.getEncomenda(encomenda);
        this.aEntregar.add(new PossibilidadeEntrega(e,
                                                    this.getGPSLoja(loja),
                                                    this.getGPSUser(e.getCodUtilizador()),
                                                    this.lojas.decCliente(e.getCodLoja())));
        this.atribuirEncomendas();
    }

    /**
     * Função que altera a disponibilidade de um transportador.
     * @param disp Nova disponibilidade.
     * @param codigo Código do Transportador.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    public void alterarDisponibilidade(boolean disp, String codigo) throws TransportadorNotFoundException{
        boolean anterior = this.transportadores.alterarDisponibilidade(disp, codigo);
        if(disp)
            if(!anterior){
                this.availables.add(codigo);
                atribuirEncomendas();
            }
        else this.availables.remove(codigo);
    }

    /**
     * Função responsável por atribuir uma encomenda, relativamente ao seu estado (entregue, a entregar, aceite...)
     */
    private void atribuirEncomendas() {
        int disponiveis = this.availables.size();
        for(PossibilidadeEntrega pe : this.aEntregar)                           //Percorrer todas as encomendas para entregar
            for(int i = 0; i < disponiveis; i++){                               //Para todos os transportadores disponiveis
                if (!pe.isAceite()) continue;                                   //Se ja estiver atribuida ignora
                String transport = this.availables.get(i);                      //Retirar codigo do transportador
                if (pe.isBlocked(transport)) continue;                          //Se transportador nao pode entregar esta ignora
                try {
                    if(pe.isMedica() && !this.transportadores.isCertificado(transport))
                        continue;
                    Localizacao gpsTransp = this.getGPSTransportador(transport);//GPS transportador
                    double raio = this.getRaioTransportador(transport);         //Raio de atuação
                    Localizacao gpsUser = pe.getGpsUtilizador();                //GPS utilizador
                    Localizacao gpsLoja = pe.getGpsLoja();                      //GPS loja
                    if (acessivel(gpsTransp,raio,gpsUser,gpsLoja)){             //Se consegue entregar esta encomenda
                        pe.block(transport);                                    //Marca como bloqueado, para caso tente outravez nao seja possivel
                        this.availables.remove(transport);                      //Remove transportador para nao poder transportar mais
                        Tipo t = this.transportadores.tipoTransportador(transport);
                        if(t == Tipo.VOLUNTARIO)                                //Se for voluntário...
                            pe.setAEntregar(transport);                         //Marca que alguem ja esta a tratar dela
                        else {                                                  //Se for empresa...
                            pe.setEsperar(transport,                            //Colocar à espera de resposta
                                    this.transportadores.getCusto(transport,
                                            pe.getPesoEncomenda(),
                                            gpsTransp.distance(gpsLoja) +
                                                    gpsLoja.distance(gpsUser),
                                            pe.getDemora()));
                            this.utilizadores.addPedidoEncomenda(pe.clone());   //Colocar na lista do utilizador;
                        }
                        this.entregar(transport,pe.clone());                    //Colocar possibilidade de entrega no transportador
                        i--; disponiveis--;                                     //Avancar (reduzir tamanho)
                    }
                } catch (TransportadorNotFoundException | UtilizadorNotFoundException ignored) {}   //Nao deve acontecer
            }
    }

    /**
     * Função que devolve a PossibilidadeEntrega a partir de uma encomenda.
     * @param encomenda Código da Encomenda.
     * @return PossibilidadeEntrega com a Encomenda.
     */
    private PossibilidadeEntrega getPossibilidadeEncomenda(String encomenda){
        boolean found = false;
        Iterator<PossibilidadeEntrega> it = this.aEntregar.iterator();
        PossibilidadeEntrega pe = null;
        while(!found && it.hasNext()){
            pe = it.next();
            found = pe.getCodigoEncomenda().equals(encomenda);
        }
        return pe;
    }

    /**
     * Função que aplica uma dada funcao à primeira Possibilidade de Entrega em aEntregar
     * @param p PossibilidadeEntrega.
     * @param consumer Consumer.
     */
    private void aplicarAEntregar(PossibilidadeEntrega p, Consumer<PossibilidadeEntrega> consumer){
        Iterator<PossibilidadeEntrega> it = this.aEntregar.iterator();
        boolean found = false;
        while(!found && it.hasNext()) {
            PossibilidadeEntrega pe = it.next();
            if (found = pe.equals(p))
                consumer.accept(pe);
        }
    }

    /////////////////////////////////////////////////  TRANSPORTADOR  //////////////////////////////////////////////////

    /**
     * Função que devolve a Localização de um Transporador.
     * @param codigo Código do Transportador.
     * @return Localização do Transportador.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    private Localizacao getGPSTransportador(String codigo) throws TransportadorNotFoundException {
        return this.transportadores.getGPS(codigo);
    }

    /**
     * Função que devolve o raio de um Transportador.
     * @param codigo Código do Transportador.
     * @return Raio do Transportador.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    private double getRaioTransportador(String codigo) throws TransportadorNotFoundException{
        return this.transportadores.getRaio(codigo);
    }

    /**
     * Função que verifica se uma Localização encontra-se no raio.
     * @param l Localização.
     * @param raio Raio.
     * @param user Localização do utilizador.
     * @param store Localização da loja.
     * @return true se for acessivel.
     */
    private boolean acessivel(Localizacao l, double raio, Localizacao user, Localizacao store){
        return l.closerOrEqual(user,raio) && l.closerOrEqual(store,raio);
    }

    /**
     * Função que atribui uma entrega a um Transportador.
     * @param codTransporter Código do Transportador.
     * @param pe PossibilidadeEntrega.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    private void entregar(String codTransporter, PossibilidadeEntrega pe) throws TransportadorNotFoundException {
        this.transportadores.entregar(codTransporter, pe);
    }

    /**
     * Função que sucede uma entrega por parte de um transportador.
     * @param codTransportadora Código do Transporador.
     * @param codEncomenda Código da Encomenda.
     * @param tempo Tempo da entrega.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    public void entregue(String codTransportadora, String codEncomenda, double tempo) throws TransportadorNotFoundException,
                                                                                             EncomendaNotFoundException,
                                                                                             UtilizadorNotFoundException {
        PossibilidadeEntrega pe = getPossibilidadeEncomenda(codEncomenda);
        if(pe == null) throw new EncomendaNotFoundException(codEncomenda);
        InfoHistorico ih = this.transportadores.entregue(codTransportadora, tempo);
        this.utilizadores.entregue(ih);
        this.entregues.add(pe.getCodigoEncomenda());
        this.aEntregar.remove(pe);
    }

    /**
     * Função que devolve a disponibilidade de um Transportador.
     * @param transportador Código do Transportador.
     * @return true se estiver disponível.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    public boolean getDisponibilidade(String transportador) throws TransportadorNotFoundException{
        return this.transportadores.getDisponibilidade(transportador);
    }

    /**
     * Função que devolve o top10 Empresas em realação à distância percorrida.
     * @return List com o top10 das Empresas.
     */
    public List<String> listagemTop10Empresas(){
        return this.transportadores.listagemTop10Empresas();
    }

    /**
     * Função que devolve o total faturado por um Transportador num intervalo de tempo.
     * @param codEmpresa Código da Empresa.
     * @param inf Limite inferior.
     * @param sup Limite superior.
     * @return total faturado por um Transportador num intervalo de tempo.
     * @throws EmpresaNotFoundException Quando a Empresa é inválida ou não for encontrada.
     * @throws OrdemCronologicaErradaException Quando a ordem cronológica está trocada.
     */
    public double getFaturadoIntervalo(String codEmpresa, LocalDateTime inf, LocalDateTime sup) throws EmpresaNotFoundException,
                                                                                                       OrdemCronologicaErradaException {
        return this.transportadores.getFaturadoIntervalo(codEmpresa,inf,sup);
    }

    /**
     * Função que altera o raio de um voluntário.
     * @param cod Código do Voluntário.
     * @param raio Novo Raio.
     * @throws VoluntarioNotFoundException Quando o Voluntário é inválido ou não for encontrado.
     * @throws NotPositiveNumberException Quando o raio é menor que 0.
     */
    public void alterarRaioVoluntario(String cod, double raio) throws VoluntarioNotFoundException, NotPositiveNumberException{
        this.transportadores.alterarRaioVoluntario(cod,raio);
    }

    /**
     * Função que altera o nif de uma empresa.
     * @param cod Código da Empresa.
     * @param nif Novo nif.
     * @throws EmpresaNotFoundException Quando a Empresa é inválida ou não for encontrada.
     * @throws NotPositiveNumberException Quando o nif é menor que 0.
     */
    public void alterarNifEmpresa(String cod, int nif) throws EmpresaNotFoundException, NotPositiveNumberException{
        this.transportadores.alterarNifEmpresa(cod,nif);
    }

    /**
     * Função que altera o raio de uma empresa.
     * @param cod Código da Empresa.
     * @param raio Novo raio.
     * @throws EmpresaNotFoundException Quando a Empresa é inválida ou não for encontrada.
     * @throws NotPositiveNumberException Quando o raio é menor que 0.
     */
    public void alterarRaioEmpresa(String cod, double raio) throws EmpresaNotFoundException, NotPositiveNumberException{
        this.transportadores.alterarRaioEmpresa(cod,raio);
    }

    /**
     * Função que altera o preço por km de uma Empresa.
     * @param cod Código da Empresa.
     * @param precoPorKm Novo preço por km.
     * @throws EmpresaNotFoundException Quando a Empresa é inválida ou não for encontrada.
     * @throws NotPositiveNumberException Quando o preço é menor que 0.
     */
    public void alterarPrecoPorKmEmpresa(String cod, double precoPorKm) throws EmpresaNotFoundException, NotPositiveNumberException{
        this.transportadores.alterarPrecoPorKmEmpresa(cod,precoPorKm);
    }

    /**
     * Função que alter o preço por peso de uma Empresa.
     * @param cod Código da Empresa.
     * @param precoPorPeso Novo preço por peso.
     * @throws EmpresaNotFoundException Quando a Empresa é inválida ou não for encontrada.
     * @throws NotPositiveNumberException Quando o preço é menor que 0.
     */
    public void alterarPrecoPorPesoEmpresa(String cod, double precoPorPeso) throws EmpresaNotFoundException, NotPositiveNumberException{
        this.transportadores.alterarPrecoPorPesoEmpresa(cod,precoPorPeso);
    }

    /**
     * Função que altera o preço por hora de uma Empresa.
     * @param cod Código da Empresa.
     * @param precoPorHora Novo preço por hora.
     * @throws EmpresaNotFoundException Quando a Empresa é inválida ou não for encontrada.
     * @throws NotPositiveNumberException Quando o preço é menor que 0.
     */
    public void alterarPrecoPorHoraEmpresa(String cod, double precoPorHora) throws EmpresaNotFoundException, NotPositiveNumberException{
        this.transportadores.alterarPrecoPorHoraEmpresa(cod,precoPorHora);
    }

    /**
     * Função que altera o estado de aceitação de encomendas médicas.
     * @param cod Código do Transportador.
     * @param state Novo estado de aceitação.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    public void alterarCertificado(String cod, boolean state) throws TransportadorNotFoundException{
        this.transportadores.alterarCertificado(cod,state);
        if(state)
            this.atribuirEncomendas();
    }

    /**
     * Função que descreve a situação de um Transportador aceitar uma entrega.
     * @param p PossibilidadeEntrega.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     */
    private void aceitar(PossibilidadeEntrega p) throws TransportadorNotFoundException, EncomendaNotFoundException {
        aplicarAEntregar(p, pe -> pe.setAEntregar(pe.getTransportador()));
        this.transportadores.aceitar(p.getTransportador());
    }

    /**
     * Função que descreve a situação de um transportador recusar uma entrega.
     * @param p PossibilidadeEntrega.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     */
    private void recusar(PossibilidadeEntrega p) throws TransportadorNotFoundException, EncomendaNotFoundException {
        aplicarAEntregar(p,pe -> {try {this.alterarDisponibilidade(true,p.getTransportador()); }
                                  catch (TransportadorNotFoundException ignored) {}
                                  pe.setAceite();
                                  this.atribuirEncomendas();});
        this.transportadores.recusar(p.getTransportador());
    }

    /**
     * Função que descreve a situação de um utilizador recusar um Transportador.
     * @param codigoTransportador Código do Transportador.
     * @throws TransportadorNotFoundException  Quando o Transportador é inválido ou não for encontrado.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    public void recusarTransportador(String codigoTransportador) throws TransportadorNotFoundException,
                                                                        EncomendaNotFoundException,
                                                                        UtilizadorNotFoundException {
        PossibilidadeEntrega p = this.transportadores.recusar(codigoTransportador);
        this.utilizadores.responder(p.getCodigoUtilizador(),p.getCodigoEncomenda());
        aplicarAEntregar(p,pe -> {try {this.alterarDisponibilidade(true,p.getTransportador()); }
                                  catch (TransportadorNotFoundException ignored) {}
                                  pe.setAceite();
                                  this.atribuirEncomendas();});
    }

    /**
     * Devolve informação relativa às encomendas tranportadas pelo transportador.
     * @param codigo Código do Transportador.
     * @return Estrutura com informação sobre as encomendas transportadas.
     * @throws TransportadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    public List<String> encomendasTransportadas(String codigo) throws TransportadorNotFoundException{
        return this.transportadores.encomendasTransportadas(codigo);
    }

    /**
     * Função que retorna o estado de entrega de um Transportador.
     * @param transportador Código do Transportador.
     * @return Estado de entrega de um transportador.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     */
    public Estado getEstadoEntrega(String transportador) throws TransportadorNotFoundException,
                                                                EncomendaNotFoundException {
        return this.transportadores.getEstadoEntrega(transportador);
    }

    /**
     * Função que devolve o código de entrega de uma encomenda por parte do transportador.
     * @param transportador Código do Transportador.
     * @return Código da Encomenda.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     */
    public String getCodigoEncomendaTransportador(String transportador) throws TransportadorNotFoundException,
                                                                               EncomendaNotFoundException {
        return this.transportadores.getCodigoEncomendaTransportador(transportador);
    }

    /**
     * Função que devolve a classificação média de um transportador.
     * @param transportador Código do Transportador.
     * @return classificação média de um transportador.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    public double classificacaoTransportador(String transportador) throws TransportadorNotFoundException {
        return this.transportadores.classificacaoMedia(transportador);
    }

    /**
     * Função que dado um código devolve os parâmetros do transportador correspondente.
     * @param codigo Código do Transportador.
     * @return String com  os parâmetros do transportador .
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    public String getDadosTransportador(String codigo) throws TransportadorNotFoundException {
        return this.transportadores.getDadosTransportador(codigo);
    }

    /////////////////////////////////////////////////  UTILIZADOR  /////////////////////////////////////////////////////

    /**
     * Função que devolve a Localização de um utilizador.
     * @param codigo Código do Utilizador.
     * @return Localização do Utilizador.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    private Localizacao getGPSUser(String codigo) throws UtilizadorNotFoundException{
        return this.utilizadores.getGPS(codigo);
    }

    /**
     * Função que devolve o top10 Utilizadores que mais encomendas fizeram.
     * @return List com o top10 utilizadores.
     */
    public List<String> listagemTop10Utilizadores(){
        return this.utilizadores.listagemTop10Utilizadores();
    }

    /**
     * Função que devlve as encomendas pendentes feitas por um utilizador.
     * @param cod Código do Utilizador.
     * @return Map com as encomendas pendentes e seu custo.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    public Map<String,Double> getListPendentes(String cod) throws UtilizadorNotFoundException{
        return this.utilizadores.getListPendentes(cod);
    }

    /**
     * Função que atribui uma resposta a uma Encomenda Pendente.
     * @param resposta true -> aceitar  false->recusar
     * @param codUtilizador Código do Utilizador.
     * @param codEncomenda Códiga da Encomenda.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    public void responder(boolean resposta, String codUtilizador, String codEncomenda) throws UtilizadorNotFoundException,
                                                                                              EncomendaNotFoundException,
                                                                                              TransportadorNotFoundException {
        PossibilidadeEntrega pe = this.utilizadores.responder(codUtilizador,codEncomenda);
        if (resposta) aceitar(pe);
        else recusar(pe);
    }


    /**
     * Função que classifica uma entrega.
     * @param utilizador Código do Utilizador.
     * @param encomenda Códiga da Encomenda.
     * @param classificacao Classificação atribuida.
     * @throws EncomendaNotFoundException Quando a Encomenda é inválida ou não for encontrada.
     * @throws ClassificacaoInvalidaException Quando a classificação não está entre 1 e 10, inclusive.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     * @throws TransportadorNotFoundException Quando o Transportador é inválido ou não for encontrado.
     */
    public void classificar(String utilizador, String encomenda, int classificacao) throws EncomendaNotFoundException,
                                                                                           ClassificacaoInvalidaException,
                                                                                           UtilizadorNotFoundException,
                                                                                           TransportadorNotFoundException {
        String transportador = this.utilizadores.classificar(utilizador,encomenda,classificacao);
        this.transportadores.classificar(transportador,encomenda,classificacao);
    }

    /**
     * Função que devolve a lista de encomendas entregues, não classificadas, por parte de um utilizador.
     * @param utilizador Código do Utilizador.
     * @return lista de encomendas entregues, não classificadas, por parte de um utilizador.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    public List<String> getListNaoClassificados(String utilizador) throws UtilizadorNotFoundException {
        return this.utilizadores.getListNaoClassificados(utilizador);
    }

    /**
     * Função que devolve a informação das encomendas feitas, por um utilizador, num intervalo de tempo.
     * @param utilizador Código Utilizador.
     * @param transportador Código Transportador.
     * @param inf Limite inferior.
     * @param sup Limite superior.
     * @return Informação das encomendas entregues num determinado intervalo de tempo.
     * @throws OrdemCronologicaErradaException Quando a ordem cronológica estiver incorreta.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    public Collection<String> getInfoUtilizadorTransportador(String utilizador,
                                                             String transportador,
                                                             LocalDateTime inf,
                                                             LocalDateTime sup) throws OrdemCronologicaErradaException,
                                                                                       UtilizadorNotFoundException {
        return this.utilizadores.getInfoHistoricoUtilizador(utilizador,inf,sup,transportador);
    }

    /**
     * Função que devolve os dados de um utilizador.
     * @param codigo Código do utilizador.
     * @return String com os dados do utilizador.
     * @throws UtilizadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    public String getDadosUtilizador(String codigo) throws UtilizadorNotFoundException {
        return this.utilizadores.getDadosUtilizador(codigo);
    }
}
