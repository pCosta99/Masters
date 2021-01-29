import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class Sistema implements Serializable
{ 
    // Utilizadores do sistema
    private Map<String, AppUser> utilizadoresRegistados;

    private Map<String,Loja> lojas;
    private Map<String,Utilizador> utilizadores;
    private Map<String,Voluntario> voluntarios;
    private Map<String,Transportadora> transportadoras;

    // Encomendas
    private List<Encomenda> encomendasPorAceitar;
    private Map<Encomenda, TempoCustoEncomenda> historicoEncomendas;
    private Map<String, String> quemEntrega;
    private String ficheiro;



    /**
     * Constutor por omissão da classe Sistema.
     */
    public Sistema(){
        this.utilizadoresRegistados = new HashMap<>();
        this.lojas = new HashMap<>();
        this.utilizadores = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.transportadoras = new HashMap<>();
        this.quemEntrega = new HashMap<>();
        this.encomendasPorAceitar = new ArrayList<>();
        this.historicoEncomendas = new HashMap<>();
        this.ficheiro = "estado.obj";
    }

    public void setSistema(Sistema sistema){
        this.utilizadoresRegistados = sistema.getUtilizadoresRegistados();
        this.lojas = sistema.getLojasMap();
        this.utilizadores= sistema.getUtilizadores();
        this.voluntarios = sistema.getVoluntariosMap();
        this.transportadoras = sistema.getTransportadorasMap();
        this.quemEntrega = sistema.getQuemEntrega();
        this.encomendasPorAceitar = sistema.getEncomendasPorAceitar();
        this.historicoEncomendas = sistema.getHistoricoEncomendas();
        this.ficheiro = sistema.getFicheiro();
    }


    public Map<String, AppUser> getUtilizadoresRegistados() {
        return utilizadoresRegistados;
    }

    public void setUtilizadoresRegistados(Map<String, AppUser> utilizadoresRegistados) {
        this.utilizadoresRegistados = utilizadoresRegistados;
    }

    public Map<String, Loja> getLojasMap(){
        return this.lojas;
    }

    public void setLojas(Map<String, Loja> lojas) {
        this.lojas = lojas;
    }

    public Map<String, Utilizador> getUtilizadores() {
        return utilizadores;
    }

    public void setUtilizadores(Map<String, Utilizador> utilizadores) {
        this.utilizadores = utilizadores;
    }

    public Map<String , Voluntario> getVoluntariosMap(){
        return this.voluntarios;
    }

    public void setVoluntarios(Map<String, Voluntario> voluntarios) {
        this.voluntarios = voluntarios;
    }

    public Map<String, Transportadora> getTransportadorasMap(){
        return this.transportadoras;
    }

    public void setTransportadoras(Map<String, Transportadora> transportadoras) {
        this.transportadoras = transportadoras;
    }

    public List<Encomenda> getEncomendasPorAceitar() {
        return encomendasPorAceitar;
    }

    public void setEncomendasPorAceitar(List<Encomenda> encomendasPorAceitar) {
        this.encomendasPorAceitar = encomendasPorAceitar;
    }

    public Map<Encomenda, TempoCustoEncomenda> getHistoricoEncomendas() {
        return historicoEncomendas;
    }

    public void setHistoricoEncomendas(Map<Encomenda, TempoCustoEncomenda> historicoEncomendas) {
        this.historicoEncomendas = historicoEncomendas;
    }

    public String getFicheiro() {
        return ficheiro;
    }

    public void setFicheiro(String ficheiro) {
        this.ficheiro = ficheiro;
    }


    /**
     * Retorna um AppUser caso esteja no Sistema.
     * @param username um nome de utilizddor
     * @return AppUser
     */
    public AppUser getUser(String username){
        return this.utilizadoresRegistados.get(username);
    }



    /**
     * Retorna uma cópia de um objeto da classe Loja caso esteja no Sistema.
     * @param cod código da loja
     * @return cópia do objeto da classe Loja
     */
    public Loja getLoja(String cod) {
        return this.lojas.get(cod);
    }

    /**
     * Retorna um Utilizador caso esteja no Sistema.
     * @param cod código de um utilizador
     * @return cópia do objeto da classe Utilizador
     */
    public Utilizador getUtilizador(String cod) {
        return this.utilizadores.get(cod);
    }

    /**
     * Retorna uma Voluntario caso esteja no Sistema.
     * @param cod código de um voluntário
     * @return cópia do objeto da classe Voluntario
     */
    public Voluntario getVoluntario(String cod) {
        return this.voluntarios.get(cod);
    }

    /**
     * Retorna uma cópia de um obejto da classe Transportadora, caso esteja no Sistema.
     * @param cod código da transportadora
     * @return cópia do objeto da classe TRansportadora
     */
    public Transportadora getTransportadora(String cod) {
        return this.transportadoras.get(cod).clone();
    }


    public List<Loja> getLojas() {
        return this.lojas.values().stream().map(l -> l.clone()).collect(Collectors.toList());
    }

    /**
     * Retorna uma cópia do conjunto de transportadoras do Sistema.
     * @return conjunto de transportadoras
     */
    public Set<Transportadora> getTransportadoras(){
        return this.transportadoras.values().stream().map(v->v.clone()).collect(Collectors.toSet());
    }

    /**
     * Retorna uma cópia do conjunto de voluntários do Sistema.
     * @return conjunto de voluntários
     */
    public Set<Voluntario> getVoluntarios(){
        return this.voluntarios.values().stream().map(v->v.clone()).collect(Collectors.toSet());
    }

    /**
     * Devolve uma cópia das encomendas que estão à espera de ser aceites.
     * Devolve cópias das encomendas originais, para seguir o encapsulamento. VER ISTO MELHOR
     * @return
     */
    public List<Encomenda> getEncomendas() {
        return this.encomendasPorAceitar.stream().map(Encomenda::clone).collect(Collectors.toList());
    }


    /**
     * Verifica se um determinado id já existe no sistema.
     * @param username um id
     * @return true se existir e false caso contrário
     */
    public boolean userExiste(String username){
        return this.utilizadoresRegistados.containsKey(username);
    }



    public Map<String, String> getQuemEntrega() {
        return this.quemEntrega.entrySet().stream().collect(Collectors.toMap(entry->entry.getKey(), entry-> entry.getValue()));
    }

    public void setQuemEntrega(Map<String, String> quemEntrega) {
        this.quemEntrega = quemEntrega.entrySet().stream().collect(Collectors.toMap(entry->entry.getKey(), entry->entry.getValue()));
    }

    public boolean temLoja(String cod){
        return this.lojas.containsKey(cod);
    }

    /**
     * Verifica se existe um voluntário com dado código no Sistema.
     * @param cod um código de voluntário
     * @return true se existir, fal    private Object comparatorTransporta;
se caso contrário
     */
    public boolean eVoluntario(String cod){
        return this.voluntarios.containsKey(cod);
    }

    /**
     * Verifica se existe uma transportadora com dado código
     * @param cod um código de transportadora
     * @return true se existir, false caso contrário
     */
    public boolean eTransportadora(String cod){
        return this.transportadoras.containsKey(cod);
    }


    /**
     * Adiciona uma Encomenda ao Sistema.
     * @param enc uma encomenda
     */
    public void addEncomenda(Encomenda enc) {
        this.encomendasPorAceitar.add(enc);
    }

    /**
     * Adiciona uma Loja ao Sistema.
     * @param l uma loja
     */
    public void addLoja (Loja l) {
        this.lojas.putIfAbsent(l.getCodLoja(), l);
    }

    /**
     * Adiciona um perfil ao Sistema.
     * @param u um perfil
     */
    public void addUser (AppUser u) { 
        this.utilizadoresRegistados.putIfAbsent(u.getUsername(), u); 
    }

    /**
     * Adiciona um Utilizador ao Sistema
     * @param u um utilizador
     */
    public void addUtilizador (Utilizador u) { 
        this.utilizadores.putIfAbsent(u.getCodigo(), u); 
    }

    /**
     * Adiciona um Voluntário ao Sistema.
     * @param v um voluntário
     */
    public void addVoluntario (Voluntario v) {
        this.voluntarios.putIfAbsent (v.getCodigo(), v);
    }

    /**
     * Adiciona uma Transportadora ao Sistema.
     * @param t uma transportadoras
     */
    public void addTransportadora (Transportadora t) {
        this.transportadoras.putIfAbsent (t.getCodigo(), t);
    }


    public void carregaEstado() throws FileNotFoundException, IOException, ClassNotFoundException {

        FileInputStream fis = new FileInputStream (this.ficheiro);
        ObjectInputStream ois = new ObjectInputStream (fis);
        //this = (Sistema) ois.readObject();
        Sistema sistema  = (Sistema) ois.readObject();
        this.setSistema(sistema);

       }



    public void guardaEstado() throws FileNotFoundException, IOException, ClassNotFoundException{
        
       FileOutputStream fos = new FileOutputStream (this.ficheiro);
       ObjectOutputStream oos = new ObjectOutputStream (fos);

       oos.writeObject(this);
       oos.close();
    }

    /**
     *  Criação de nova encomenda. É adicionada ao conjunto das encomendas por aceitar.
     */
    public void novaEncomenda(String user, String loja, Set<LinhaEncomenda> compras) {
        Encomenda encomenda = new Encomenda(compras, user, loja);
        this.encomendasPorAceitar.add(encomenda);
    }

    
    /**
     * Usada quando um voluntário ou empresa decidem transportar uma encomenda.
     * Não esquecer: no caso das empresas, é preciso que o utilizador aceite o serviço.
     * @param index
     */
    public Encomenda eliminaEncomenda(Integer index) {
        Encomenda e = this.encomendasPorAceitar.remove((int) index); // remove a encomenda do conjunto de encomendas
        Encomenda.removeCodigo(e.getCodEnc()); // remove o código do conjunto dos códigos a serem usados

        return e;
    }



    public String quemEntregaEncomenda(String codEncomenda){
        return this.quemEntrega.getOrDefault(codEncomenda, "");
    }


    public void classificar(String codigoEncomenda, int classificacao){

        if(this.quemEntrega.containsKey(codigoEncomenda)) {
            String codigo = this.quemEntrega.get(codigoEncomenda);

            if (eVoluntario(codigo))
                this.voluntarios.get(codigo).classifica(classificacao);

            else if (eTransportadora(codigo)) this.transportadoras.get(codigo).classifica(classificacao);
        }
    }

    public List<Encomenda> extratoVoluntario(String codigo, LocalDateTime inicio, LocalDateTime fim){
        return this.voluntarios.get(codigo).ListaExtratoViagens(inicio,fim);
    }

    public List<Encomenda> extratoTransportadora(String codigo, LocalDateTime inicio, LocalDateTime fim){
        return this.transportadoras.get(codigo).ListaExtratoViagens(inicio,fim);
    }



    public void parse_logs() throws FileNotFoundException, IOException{

            FileReader fr = new FileReader("Docs/logs.txt");
            BufferedReader bf = new BufferedReader(fr);
            String[] linhaPartida;
            String linha;

            while ((linha = bf.readLine()) != null) {

                linhaPartida = linha.split(":", 2);
                switch (linhaPartida[0]) {
                    case "Utilizador":
                        Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                        this.addUser((AppUser) u);
                        this.addUtilizador(u);
                        break;
                    case "Loja":
                        Loja l = parseLoja(linhaPartida[1]);
                        this.addLoja(l);
                        break;
                    case "Voluntario":
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        this.addVoluntario(v);
                        this.addUser((AppUser) v);
                        break;
                    case "Transportadora":
                        Transportadora t = parseTransportadora(linhaPartida[1]);
                        this.addTransportadora(t);
                        this.addUser((AppUser) t);
                        break;
                    case "Encomenda":
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        this.encomendasPorAceitar.add(e);
                        break;
                    case "Aceite":
                        //Encomenda e = parseAceite(linhaPartida[1]);
                        break;
                    default:
                        System.out.println("Linha invalida.");
                        break;
                }

            }
            System.out.println("done!");
            //PrintSistema();
    }

    public void PrintSistema(){
        this.utilizadores.values().stream().forEach(a->System.out.println(a.toString()));
        this.voluntarios.values().stream().forEach(a->System.out.println(a.toString()));
        this.transportadoras.values().stream().forEach(a->System.out.println(a.toString()));
        this.encomendasPorAceitar.stream().forEach(a->System.out.println(a.toString()));
        this.transportadoras.values().stream().forEach(a->a.PrintTrasportes());
    }


    /**
     * Dada uma string lida do ficheiro de logs, transforma-a num objeto da classe Utilizador.
     * @param input linha do ficheiro de logs
     * @return um objeto da classe Utilizador
     */
    private Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas gps = new Coordenadas (gpsx, gpsy);

        return new Utilizador(this, codUtilizador, nome, codUtilizador, codUtilizador, gps);
    }

    /**
     * Dada uma linha do ficheiro de logs, transforma-a num objeto da classe Loja.
     * @param input linha do ficheiro de logs
     * @return um objeto da classe Loja
     */
    private Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas gps = new Coordenadas (gpsx, gpsy);

        return new Loja(codLoja, nomeLoja, gps);
    }

    /**
     * Dada uma linha do ficheiro de logs, transforma-a num objeto da classe Voluntario.
     * @param input linha do ficheiro de logsn
     * @return um objeto da classe Voluntario
     */
    private Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas gps = new Coordenadas (gpsx, gpsy);
        double raio = ThreadLocalRandom.current().nextDouble(2.0, 10.0);


        return new Voluntario(codVoluntario,codVoluntario, codVoluntario, nome, gps, raio);
    }

    /**
     * Dada uma linha do ficheiro de logs, transforma-a num objeto da classe Transportadora.
     * @param input linha do ficheiro de logs
     * @return um objeto da classe Transportadora
     */
    private Transportadora parseTransportadora(String input) {
        String[] campos = input.split(",");
        String codEmpresa = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas gps = new Coordenadas (gpsx, gpsy);
        String NIF = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double preco_km = Double.parseDouble(campos[6]);

        return new Transportadora(codEmpresa, codEmpresa, codEmpresa, nome, gps, raio, NIF, preco_km);
    }

    /**
     * Dada uma linha do ficheiro de logs, transforma-a num objeto da classe Encomenda.
     * @param input linha do ficheiro de logs
     * @return um objeto da classe Encomenda
     */
    private Encomenda parseEncomenda(String input) {
        Encomenda res;

        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        //System.out.println(campos[3]);
        double peso = Double.parseDouble(campos[3]);

        Set<LinhaEncomenda> produtos = new HashSet<>();
        for (int i = 4; i < campos.length; i+=4) {
            LinhaEncomenda le = parseLinhaEncomenda(campos[i], campos[i+1], campos[i+2], campos[i+3]);
            produtos.add(le);
        }
        res = new Encomenda(codEncomenda, codUtilizador, codLoja, (float)peso, produtos);
        res.foiAceite();

        return res;
    }

    private LinhaEncomenda parseLinhaEncomenda(String codigoPoduto, String descricao, String quant, String valor) {
        double quantidade = Double.parseDouble(quant);
        double valorUnitario = Double.parseDouble(valor);
        return new LinhaEncomenda(codigoPoduto, descricao, quantidade, valorUnitario);
    }



    /**
     * Valida um login.
     * @param username um id
     * @param password uma passe
     * @return return true se o id e a passe estiverem corretos e coincidirem e false caso contrário.
     */
    public boolean validaLogin(String username, String password){
        AppUser userDoSistema = this.utilizadoresRegistados.get(username);
        return userDoSistema.getUsername().equals(username) && userDoSistema.getPassword().equals(password);
    }

    /**
     * Regista um utilizador.
     * @param username id do utilizador
     * @param password passe para fazer login
     */
    public void registarUtilizador(String nome, String username, String password){

        Random random = new Random();
        int randomNum = 1 + random.nextInt(1000);
        String codigoUtilizador = "c".concat(Integer.toString(randomNum));

        while(this.utilizadores.containsKey(codigoUtilizador)){ // criar um codigo nao existente
            randomNum = 1 + random.nextInt(1000);
            codigoUtilizador = "c".concat(Integer.toString(randomNum));
        }
        double longitude = -100.0 + random.nextDouble()* 200.0;
        double latitude = -100.0 + random.nextDouble()* 200.0;
        Coordenadas cordenadas = new Coordenadas(longitude, latitude);
        Set<EncomendaEntregue> historico = new TreeSet<>();

        Utilizador newUtilizador = new Utilizador(this, codigoUtilizador, nome, username, password, cordenadas, historico);
        this.utilizadores.put(codigoUtilizador, newUtilizador);
        this.utilizadoresRegistados.put(username, newUtilizador);
    }

    /**
     * Regista um voluntário.
     * @param username id voluntário
     * @param password passe para fazer login
     * @param nome nome do voluntário
     * @param raio raio de atuação
     */
    public void registarVoluntario(String username, String password, String nome, double raio){

        Random random = new Random();
        int randomNum = 1 + random.nextInt(1000);
        String codigoVoluntario = "v".concat(Integer.toString(randomNum));
        while(this.utilizadores.containsKey(codigoVoluntario)){
            randomNum = 1 + random.nextInt(1000);
            codigoVoluntario = "v".concat(Integer.toString(randomNum));
        }
        double longitude = -100.0 + random.nextDouble()* 200.0;
        double latitude = -100.0 + random.nextDouble()* 200.0;
        Coordenadas cordenadas = new Coordenadas(longitude, latitude);


        Voluntario newVoluntario = new Voluntario(username, password, codigoVoluntario, nome, cordenadas, raio);
        this.voluntarios.put(codigoVoluntario, newVoluntario);
        this.utilizadoresRegistados.put(username, newVoluntario);
    }


    /**
     * REgista uma transportadora.
     * @param username id da transportadora
     * @param password passe para fazer login
     * @param nome nome da transportadora
     * @param nif o NIF
     * @param raio o raio de atuação
     * @param precoKm o preço por km de transporte
     */
    public void registarTransportadora(String username, String password, String nome,String nif, double raio, double precoKm){

        Random random = new Random();
        int randomNum = 1 + random.nextInt(1000); //numero random de 0 a 999
        String codigoTransportadora = "t".concat(Integer.toString(randomNum));
        while(this.utilizadores.containsKey(codigoTransportadora)){
            randomNum = 1 + random.nextInt(1000); //numero random de 0 a 999
            codigoTransportadora = "t".concat(Integer.toString(randomNum));
        }
        double longitude = -100.0 + random.nextDouble()* 200.0;
        double latitude = -100.0 + random.nextDouble()* 200.0;
        Coordenadas cordenadas = new Coordenadas(longitude, latitude);


        Transportadora newTransportadora = new Transportadora(username, password, codigoTransportadora, nome, cordenadas, raio, nif, precoKm);
        this.transportadoras.put(codigoTransportadora, newTransportadora.clone());
        this.utilizadoresRegistados.put(username, newTransportadora.clone());
    }


    public List<Utilizador> top_utilizadores (int top) {
        return this.utilizadores.values().stream().map(u -> u.clone()).sorted().collect(Collectors.toList());
    }

    public List<Transportadora> top_empresas (int top) {
       return this.transportadoras.values().stream().map(u -> u.clone()).sorted(new ComparatorTransporta()).collect(Collectors.toList());
    }

    /**
     * Distribui as encomendas à espera no sistema, pelas várias entidades de transporte.
     */
    public void distribuiEncomendas () {
        while (!this.encomendasPorAceitar.isEmpty()) {
            Set<Transporta> vaoTransportar = atribuiTransportes();
            vaoTransportar.forEach(Transporta::trataEncomendas);
        }
    }


    /**
     * Atribui as encomendas presentes no sistema às entidades transportadoras.
     * Depois da execução deste método, cada uma das entidades tranportadoras (voluntários e transportadoras) deverão ter um conjunto de encomendas (ou uma apenas) por transportar (ou tratar).
     */
    public Set<Transporta> atribuiTransportes() {
        Set<Transporta> res = new TreeSet<>();
        int i = 0;
        Encomenda e;
        while (i < this.encomendasPorAceitar.size()) {
            e = this.encomendasPorAceitar.get(i);

            try {
                Voluntario voluntario = atribuiVoluntarioAleatoriamente(e);
                res.add(voluntario);
                this.quemEntrega.put(e.getCodEnc(), voluntario.getCodigo());
                eliminaEncomenda(i);
            } 

            catch(VoluntariosIndisponíveis exception1) {

            try {
                Transportadora transportadora = atribuiTransportadoraAleatoriamente(e);
                res.add(transportadora);
                this.quemEntrega.put(e.getCodEnc(), transportadora.getCodigo());
                eliminaEncomenda(i); // n faz sentido estar a repetir este bocado de código! arranjar isto mais tarde !! <-----------
            } 
            
            catch (TransportadorasIndisponíveis exception2) {
                i++;
            }

            }
        }
        return res;
    }

    private Voluntario atribuiVoluntarioAleatoriamente(Encomenda e) throws VoluntariosIndisponíveis {
        Voluntario res;
        Stream<Voluntario> stream = voluntarios.values().stream();
        if (e.isMed()) stream = stream.filter(v -> v.aceitoTransporteMedicamentos());
        List<Voluntario> voluntarios_disponiveis = stream.filter(v -> v.estaDisponivel() && v.aceitaEncomenda(e)).collect(Collectors.toList()); // Não está a fazer clone, mas acho que faz mais sentido ter o apontador para o próprio voluntário (depois confirmar)

        int num_voluntarios_disponiveis = voluntarios_disponiveis.size();
        if (num_voluntarios_disponiveis == 0) {
            throw new VoluntariosIndisponíveis();
        }
        Random gerador = new Random();
        res = voluntarios_disponiveis.get(gerador.nextInt(num_voluntarios_disponiveis-1)); // tem de ser menos um, não tem?? CONFIRMAR
        res.addEncomenda(e);; // Importante: é aqui que ele passa a estar indisponível!!

        return res;
    }

    private Transportadora atribuiTransportadoraAleatoriamente(Encomenda e) throws TransportadorasIndisponíveis {
        Transportadora res = null;
        Stream<Transportadora> stream = transportadoras.values().stream();
        if (e.isMed()) stream = stream.filter(t -> t.aceitoTransporteMedicamentos());
        List<Transportadora> empresas = stream.filter(t -> t.isTransportesDisponiveis() && t.aceitaEncomenda(e)).collect(Collectors.toList()); // Não faz clones, mas acho que não é suposto.

        int num_transportadoras = empresas.size();
        if (num_transportadoras == 0) {
            throw new TransportadorasIndisponíveis();
        }
        Random gerador = new Random();
        boolean userAceitou = false;
        while (!userAceitou) {
            res = empresas.get(gerador.nextInt(num_transportadoras - 1));
            userAceitou = res.utilizadorAceitaEncomenda(e);
        }

        res.addEncomenda(e);

        return res;
    }


}
