package src.model;

import src.exceptions.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que alimenta toda a informação na aplicação
 */
public class TrazAqui implements Serializable{

    //variáveis de instância
    private Map<String,Utilizador> utilizadores;
    private Map<String,Loja> lojas;
    private Map<String,Produto> produtos;
    private Map<String,Distribuidor> distribuidores;

    /**
     * Métodos de Classe
     */

    /**
     * Método que carrega o estado da aplicação a partir de um ficheiro objeto
     * @param nfich String com o nome do ficheiro a carregado
     * @return Instância da classe TrazAqui com o estado salvo no ficheiro objeto
     */
    public static TrazAqui carregaEstado(String nfich) throws IOException,ClassNotFoundException{
        FileInputStream fis = new FileInputStream(nfich);
        ObjectInputStream ois = new ObjectInputStream(fis);
        TrazAqui sys = (TrazAqui) ois.readObject();
        sys.upLoja();
        sys.upCodProd();
        sys.upUtilz();
        sys.upCodEnc();
        sys.upDistr();
        ois.close();
        fis.close();
        return sys;
    }

    /**
     * Métodos de instância
     */

    /**
     * Construtor vazio da classe
     */
    public TrazAqui(){
        this.utilizadores = new HashMap<>();
        this.lojas = new HashMap<>();
        this.produtos = new HashMap<>();
        this.distribuidores = new HashMap<>();
    }

    /**
     * Construtor classe a partir de logs
     * @param nfich String com o nome do ficheiro de logs a ser utilizado na construção da classe
     */
    public TrazAqui(String nfich) throws LogsInvalidosException{
        this.utilizadores = new HashMap<>();
        this.lojas = new HashMap<>();
        this.produtos = new HashMap<>();
        this.distribuidores = new HashMap<>();
        this.parse(nfich);
    }


    /**
     * Método que salva o estado da aplicação em um ficheiro objeto
     * @param nfich String com o nome do ficheiro onde o estado deverá ser salvo
     */
    public void salvaEstado(String nfich) throws IOException{
        FileOutputStream fos = new FileOutputStream(nfich);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.close();
        fos.close();
    }




    /**
     * Método que regista um produto no sistema da aplicação 
     * Adiciona uma instancia da classe Produto com as caracteristicas apresentadas nos parâmetros recebidos como argumento
     */
    public void registaProduto(String nome, double custo, double peso, boolean isMedicamento) throws ValorInvalidoException{
        if(peso <= 0 || custo <= 0) throw  new ValorInvalidoException();
        Produto p = new Produto(nome,custo,peso, isMedicamento);
        this.produtos.put(p.getCodProd(),p);
    }




    /**
     * Método que regista um utilizador no sistema da aplicação 
     * Adiciona uma instancia da classe Utilizador com as caracteristicas apresentadas nos parâmetros recebidos como argumento
     * @return String com o código do utilizador
     */
    public String registaUtilizador(String email, String password, String nome, Ponto p) throws LoginErrorException {
        if (this.buscaUser(email,0).isPresent()) {
            throw new LoginErrorException("o email: " + email + "já está em uso!" );
        }
        Utilizador u = new Utilizador(password,nome,email,p,new ArrayList<>());
        this.utilizadores.put(u.getUsername(),u);
        return u.getUsername();
    }




    /**
     * Método que regista um voluntário no sistema da aplicação 
     * Adiciona uma instancia da classe voluntário com as caracteristicas apresentadas nos parâmetros recebidos como argumento
     * @return String com o código do voluntário registado
     */
    public String registaVoluntario(String email, String password, String nome, Ponto p, double raio) throws LoginErrorException,ValorInvalidoException{
        if (this.buscaUser(email,2).isPresent()) throw new LoginErrorException("o email: " + email + "já está em uso!" );

        if(raio <= 0) throw new ValorInvalidoException();

        Voluntario v = new Voluntario(password,nome,email,raio, new Classificacao(),false,p,new ArrayList<>());
        this.distribuidores.put(v.getUsername(),v);
        return v.getUsername();
    }

    /**
     * Método que regista um voluntário médico no sistema da aplicação 
     * Adiciona uma instancia da classe voluntárioMédico com as caracteristicas apresentadas nos parâmetros recebidos como argumento
     * @return String com o código do voluntário médico registado
     */
    public String registaVoluntarioMedico(String email, String password, String nome, Ponto p, double raio) throws LoginErrorException,ValorInvalidoException{
        if (this.buscaUser(email,3).isPresent()) throw new LoginErrorException("o email: " + email + "já está em uso!" );

        if(raio <= 0) throw new ValorInvalidoException();

        Voluntario v = new VoluntarioMedico(password,nome,email,raio, new Classificacao(),false,p,new ArrayList<>(),false);
        this.distribuidores.put(v.getUsername(),v);
        return v.getUsername();
    }

    /**
     * Método que regista uma loja no sistema da aplicação 
     * Adiciona uma instancia da classe Loja com as caracteristicas apresentadas nos parâmetros recebidos como argumento
     * @return String com o código da loja registada
     */
    public String registaLoja(String email, String password, String nome, Ponto p) throws LoginErrorException{
        if (this.buscaUser(email,1).isPresent()) {
            throw new LoginErrorException("o email: " + email + "já está em uso!" );
        }
        Loja l = new Loja(password,nome,email,new HashMap<>(), p);
        this.lojas.put(l.getUsername(),l);
        return l.getUsername();
    }


    /**
     * Método que regista uma transportadora no sistema da aplicação 
     * Adiciona uma instancia da classe Transportadora com as caracteristicas apresentadas nos parâmetros recebidos como argumento
     * @return String com o código da transportadora registada
     */
    public String registaTransportadora(String email, String password, String nome, Ponto p, double precoPorKm, double precoPorMin, double raio) throws LoginErrorException,ValorInvalidoException{
        if (this.buscaUser(email,4).isPresent()) throw new LoginErrorException("o email: " + email + "já está em uso!" );

        if(raio <= 0) throw new ValorInvalidoException();

        Transportadora t = new Transportadora(password,nome,email,false,precoPorKm,precoPorMin,new ArrayList<>(), new Classificacao(), raio, p);
        this.distribuidores.put(t.getUsername(),t);
        return t.getUsername();
    }



    /**
     * Método que regista uma transportadora médica no sistema da aplicação 
     * Adiciona uma instancia da classe TransportadoraMédica com as caracteristicas apresentadas nos parâmetros recebidos como argumento
     * @return String com o código da transportadora médica registada
     */
    public String registaTransportadoraMedica(String email, String password, String nome, Ponto p, double precoPorKm, double precoPorMin, double raio) throws LoginErrorException,ValorInvalidoException{
        if (this.buscaUser(email,5).isPresent()) throw new LoginErrorException("o email: " + email + "já está em uso!" );

        if(raio <= 0) throw new ValorInvalidoException();

        TransportadoraMedica t = new TransportadoraMedica(password,nome,email,false,precoPorKm,precoPorMin,new ArrayList<>(),false, new Classificacao(), raio, p);
        this.distribuidores.put(t.getUsername(),t);
        return t.getUsername();
    }



    /**
     * Método que busca um email registado no sistema da aplicação 
     * @param email string a ser buscada como email na aplicação
     * @param flag indica em qual mapa a busca deve ocorrer
     * @return uma referencia ao user encontrado se o encontrar ou empty() se não
     */
    public Optional<User> buscaUser(String email, int flag){
        Collection<User> busca;

        switch (flag){
            case(7):
                busca = new ArrayList<>(this.getUtilizadores().values());
                break;

            case(8):
                busca = new ArrayList<>(this.getLojas());
                break;

            case(10):
                busca = this.getDistribuidores().stream().filter(v -> v instanceof Voluntario && !(v instanceof VoluntarioMedico)).collect(Collectors.toList());
                break;

            case(12):
                busca = this.getDistribuidores().stream().filter(v -> v instanceof VoluntarioMedico).collect(Collectors.toList());
                break;

            case(9):
                busca = this.getDistribuidores().stream().filter(v -> v instanceof Transportadora && !(v instanceof TransportadoraMedica)).collect(Collectors.toList());
                break;

            default:
                busca = this.getDistribuidores().stream().filter(v -> v instanceof TransportadoraMedica).collect(Collectors.toList());
                break;
        }

        return busca.stream().filter(u -> u.getEmail().equals(email)).findFirst();
    }

    /**
     * Método que valida uma tentativa de login na aplicação
     * @param email string com o email da tentativa de login
     * @param password string com a password da tentativa de login na aplicação
     * @param f flag que indica o tipo de login (utilizador, loja, etc) 
     * @return valor do username do user com o login apresentado
     */
    public String valida(String email, String password, int f) throws LoginErrorException{
        Optional<User> o = this.buscaUser(email,f);
        if(o.isEmpty()) throw new LoginErrorException("o email: " + email + " não existe!" );
        
        User u = o.get();
        if(u.getPassword().equals(password)){
            return u.getUsername();
        }

        else{
            throw new LoginErrorException("A senha indicada está incorreta!");
        }
    }



    /**
     * Método que retorna uma cópia de um dado produto presente na aplicação
     * @param codProd String com o código do produto a ser retornado
     * @return Produto requisitado
     */
    public Produto getProduto(String codProd) throws ProdutoInexistenteException {
        if (this.produtos.containsKey(codProd)) {
            return this.produtos.get(codProd).clone();
        }

        else {
            throw new ProdutoInexistenteException(codProd);
        }
        
    }


    /**
     * Método que retorna um set com os distribuidores da aplicação
     * @return set com todas as entradas do mapa de distribuidores
     */
    public Set<Distribuidor> getDistribuidores() {
        Set<Distribuidor> res= new TreeSet<>(new ComparatorDistribuidoresNome());

        for(Map.Entry<String,Distribuidor> e : this.distribuidores.entrySet()){
            res.add(e.getValue().clone());
        }

        return res;
    }


    /**
     * Método que retorna um map com os produtos da aplicação associados com seus respetivos códigos
     * @return cópia do map de produtos da aplicação
     */
    public Map<String, Produto> getProdutos() {
        Map<String,Produto> res = new HashMap<>();

        for(Map.Entry<String,Produto> e : this.produtos.entrySet()){
            res.put(e.getKey(),e.getValue().clone());
        }

        return res;
    }


    /**
     * Método que retorna um set com os lojas da aplicação
     * @return set com todas as entradas do mapa de lojas
     */
    public Set<Loja> getLojas() {
        Set<Loja> res = new TreeSet<>(new ComparatorLojaNome());

        for(Map.Entry<String,Loja> e : this.lojas.entrySet()){
            res.add(e.getValue().clone());
        }

        return res;
    }



    /**
     * Método que retorna um map com os utilizadores da aplicação associados com seus respetivos códigos
     * @return cópia do map de utilizadores da aplicação
     */
    public Map<String, Utilizador> getUtilizadores() {
        Map<String,Utilizador> res = new HashMap<>();

        for(Map.Entry<String,Utilizador> e : this.utilizadores.entrySet()){
            res.put(e.getKey(),e.getValue().clone());
        }

        return res;
    }

    /**
     * Método que retorna um set com os Voluntários da aplicação
     * @return set com todas as entradas de voluntários do mapa de distribuidores
     */
    public Set<Voluntario> getVoluntarios(){
        Set<Voluntario> res = new TreeSet<>(new ComparatorDistribuidoresNome());

        for(Map.Entry<String,Distribuidor> e : this.distribuidores.entrySet()){
            if (e.getValue() instanceof Voluntario){
                res.add(((Voluntario) e.getValue()).clone());
            }
        }

        return res;
    }


    /**
     * Método que retorna um set com os transportadoras da aplicação
     * @return set com todas as entradas de transportadoras do mapa de distribuidores
     */
    public Set<Transportadora> getTransportadoras(){
        Set<Transportadora> res = new TreeSet<>(new ComparatorDistribuidoresNome());

        for(Map.Entry<String,Distribuidor> e : this.distribuidores.entrySet()){
            if(e.getValue() instanceof Transportadora){
                res.add(((Transportadora) e.getValue()).clone());
            }
        }

        return res;
    }

    /**
     * Método que adiciona uma linha de encomenda para uma futura encomenda de um utilizador
     * @param codUtilizador String do código do utilizador onde a linha será adicionada
     * @param p Produto referenciado na linha
     * @param quant double com a quantidade do produto pedido
     */
    public void addLinhaEncomenda(String codUtilizador,Produto p, double quant) throws ValorInvalidoException{
        if(quant < 1) throw new ValorInvalidoException();
        this.utilizadores.get(codUtilizador).addLinhaEncomenda(p,quant);
    }

    /**
     * Método que retorna um set com os pedidos de encomenda de um dado utilizador
     * @param codUtilizador String com o código do utilizador a se receber os pedidos de encomenda
     * @return set com todos os pedidos de encomenda de um dado utilziador  
     */
    public Set<Encomenda> getPedidosEncomenda(String codUtilizador){
        return new TreeSet<>(this.utilizadores.get(codUtilizador).getPedidosEncomenda().values());
    }


    /**
     * Método que retorna um set com os Voluntários da aplicação capazes de realizar uma dada entrega
     * @param l loja de onde vem a encomenda
     * @param e Encomenda a ser realizada
     * @return set com todas as entradas de voluntários do mapa de distribuidores que realizem a entrega
     */
    private Set<Voluntario> getVoluntariosPossiveis(Loja l,Encomenda e){
        return this.distribuidores.values().stream()
                .filter(d -> d.podeTransportar(e))
                .filter(d -> d.getLocalizacao().distancia(l.getLocalizacao()) <= d.getRaio())
                .filter(d -> d instanceof Voluntario)
                .map( d -> (Voluntario) d)
                .collect(Collectors.toCollection(TreeSet::new));
    }


    /**
     * Método que retorna um set com os Transportadoras da aplicação capazes de realizar uma dada entrega
     * @param l loja de onde vem a encomenda
     * @param e Encomenda a ser realizada
     * @return set com todas as entradas de Transportadoras do mapa de distribuidores que realizem a entrega
     */
    private Map<String,Double> getTransportadorasPossiveis(Loja l, Encomenda e){
        Ponto p = this.utilizadores.get(e.getCodDest()).getLocalizacao();
        return this.distribuidores.values().stream()
                .filter(d -> d instanceof Transportadora)
                .map(t -> (Transportadora) t)
                .filter(t -> t.getLocalizacao().distancia(l.getLocalizacao()) <= t.getRaio())
                .filter(t -> t.podeTransportar(e))
                .collect(Collectors.toMap(User::getUsername, t -> t.calcCustoEntrega(l,p)));
    }


    /**
     * Método que retorna o tempo de espera de uma dada loja
     * @param codLoja código da loja para se pegar o tempo de espera
     * @return double com o tempo de espera da dada loja
     */
    public double getTempoDeEspera(String codLoja){
        return this.lojas.get(codLoja).getTempoEspera();
    }


    /**
     * Método que retorna as encomendas aceites de uma dada loja
     * @param codLoja código da loja para se pegar o tempo de espera
     * @return set com as encomendas aceites da dada loja
     */
    public Set<Entrega> getEncomendasAceites(String codLoja){
        return new TreeSet<>(this.lojas.get(codLoja).getEncomendasAceites().values());
    }



    /**
     * Método que atualiza o tempo de atendimento de uma dada loja
     * @param codLoja código da loja para se atualizar o tempo de espera
     * @param t double com o tempo de espera da dada loja
     */
    public void setTempoAtendimento(String codLoja, double t) throws ValorInvalidoException{
        if(t < 0) throw new ValorInvalidoException(t);
        this.lojas.get(codLoja).setTempoAtendimento(t);
    }


    /**
     * Método que cria uma encomenda para um dado utilizador
     * @param username string com o codigo do utilizador 
     * @param codLoja string com o codigo da loja para onde a encomenda será feita
     */
    public void criaEncomenda(String username, String codLoja) throws LojaInexistenteException, SemLinhasDeEncomendaException {
        if (this.lojas.containsKey(codLoja)){
            
            Utilizador u = this.utilizadores.get(username);
            List<LinhaEncomenda> linhas = u.getNovasLinhas();

            if (linhas.isEmpty()) {
                throw new SemLinhasDeEncomendaException();
            }
            
            Encomenda e = new Encomenda(linhas,username,codLoja);
            u.addPedidoEncomenda(e);
            u.setNovasLinhas(new ArrayList<>());            
        }


        else { throw new LojaInexistenteException(codLoja); }
    }


    /**
     * Método que classifica um distribuidor consoante uma dada entrega
     * @param codEnc código da encomenda pela qual o distribuidor será classificado
     * @param classificacao classificação dada ao distribuidor
     * @param codUtilizador String do codigo de utilizador a classificar o distribuidor
     */
    public void classificaDistribuidor(String codEnc, double classificacao,String codUtilizador) throws EncomendaInvalidaException{
       Entrega n = this.utilizadores.get(codUtilizador).getHistoricoEntregas().stream().filter( e -> e.getEncomenda().getCodEnc().equals(codEnc)).findFirst().orElse(null);
       try{
           Distribuidor d = this.distribuidores.get(n.getCodDistribuidor());
           d.addClassificacao(codEnc,classificacao);
       }

       catch (NullPointerException e){
           throw new EncomendaInvalidaException("A Encomenda de código " + codEnc + " não foi encontrada!"); }
    }


    /**
     * Método que atualiza o estado de recolha de um distribuidor
     * @param codDistribuidor código do distribuidor a ser atualizado
     * @param state boolean do estado de recolha do distribuidor
     */
    public void sinalizaRecolhaEncomendas(String codDistribuidor,boolean state) throws MaximaCapacidadeEntregasException{
        Distribuidor d = this.distribuidores.get(codDistribuidor);
        if(d.isOcupado()) throw new MaximaCapacidadeEntregasException();
        this.distribuidores.get(codDistribuidor).setRecolhe(state);
    }



    /**
     * Método que busca um distribuidor para a realização de uma encomenda
     * @param codUtilizador código do utilizador requisitando encomenda
     * @param codEnc código da encomenda cuja entrega foi requisitada
     * @return empty se um voluntário foi selecionado para realizar a entrega, e um map de transportadoras e seus custos se nenhum voluntario for selecionado
     */
    public Optional<Map<String,Double>> requisitaEntrega(String codUtilizador, String codEnc) throws EncomendaInvalidaException, SemDistribuidoresException{
        Utilizador u = this.utilizadores.get(codUtilizador);
        Encomenda e = u.getPedidosEncomenda().get(codEnc);
        try{
            Loja l = this.lojas.get(e.getCodLoja());

            Set<Voluntario> p = this.getVoluntariosPossiveis(l,e);

            if (!p.isEmpty()){
                Voluntario v = p.stream().findAny().get();
                Entrega n = new Entrega(0,LocalDateTime.now(),LocalDateTime.now(),e,v.getUsername());
                l.aceitaEncomenda(n);
                v.setOcupado(true);
                v.setRecolhe(false);
                u.removePedidoEncomenda(codEnc);
                return Optional.empty();
            }

            Map<String,Double> t = getTransportadorasPossiveis(l,e);

            if(!t.isEmpty()){
                return Optional.of(t);
            }

            else{ throw new SemDistribuidoresException(); }
            
        }

        catch (NullPointerException ex) {
            throw new EncomendaInvalidaException("A Encomenda de código " + codEnc + " não foi encontrada!");
        }

    }


    /**
     * Método que realiza a aceitação do transporte uma encomenda por uma transportadora
     * @param codDistribuidor String com o código da transportadora a realizar a entrega
     * @param codEnc String com o código da encomenda a ser aceite
     * @param codUtilizador String com o código do utilizador que requisitou a encomenda
     */
    public void transportadoraAceitaEntrega(String codDistribuidor, String codEnc, String codUtilizador){
        Utilizador u = this.utilizadores.get(codUtilizador);
        Encomenda e = u.removePedidoEncomenda(codEnc);
        Transportadora t = (Transportadora) this.distribuidores.get(codDistribuidor);
        Loja l = this.lojas.get(e.getCodLoja());
        Entrega n = new Entrega(t.calcCustoEntrega(l,u.getLocalizacao()),LocalDateTime.now(),LocalDateTime.now(),e,t.getUsername());
        l.aceitaEncomenda(n);
    }


    /**
     * Método que marca uma dada encomenda aceite como uma encomenda ativa em um distribuidor
     * @param codEnc String com o código da encomenda a ser levantada em uma loja
     * @param lojaname String com o código da loja associada à encomenda
     */
    public void sinalizaEncomendaPronta(String codEnc,String lojaname) throws EncomendaInvalidaException{
        Loja l = this.lojas.get(lojaname);
        Entrega e = l.removeEncomendaAceite(codEnc);
        e.setDataLevantamento(LocalDateTime.now());
        Distribuidor d = this.distribuidores.get(e.getCodDistribuidor());
        d.addEntregaAtiva(e);
    }


    /**
     * Método que finaliza uma entrega ativa de um voluntário
     * @param codVoluntario String com o código do voluntário que pretende finalizar a entrega
     */
    public void fazEntregaVoluntario(String codVoluntario) throws EncomendaInvalidaException{
        Distribuidor d = this.distribuidores.get(codVoluntario);
        Voluntario v = (Voluntario) d;
        try{
            Entrega e = v.getEntregaAtiva();
            e.setDataFinal(LocalDateTime.now());

            v.removeEntregaAtiva();
            v.addEntregaHistorico(e);

            Utilizador u = this.utilizadores.get(e.getEncomenda().getCodDest());

            u.addEntregaHistorico(e);
            d.setOcupado(false);
        }

        catch (NullPointerException e){
            throw new EncomendaInvalidaException("A Encomenda de código não foi encontrada!");
        }
    }



    /**
     * Método que finaliza uma entrega ativa de uma transportadora
     * @param codDistribuidor String com o código do transportadora que pretende finalizar a entrega
     * @param codEnc String com o código da encomenda a ser finalizada
     */
    public void fazEntregaTransportadora(String codDistribuidor, String codEnc) throws EncomendaInvalidaException{
        Transportadora t = (Transportadora) this.distribuidores.get(codDistribuidor);
        
        try{
            Entrega e = t.removeEntregaAtiva(codEnc);

            e.setDataFinal(LocalDateTime.now());

            Utilizador u = this.utilizadores.get(e.getEncomenda().getCodDest());
            u.addEntregaHistorico(e);

            t.addKmsPercorridos(this.getKmsAndados(e.getEncomenda()));
            t.addEntregaHistorico(e);
        }

        catch(RuntimeException e){
            throw new EncomendaInvalidaException("A Encomenda de código " + codEnc + " não foi encontrada!");
        }
    }

    /**
     * Método que atualiza o preço de transporte de uma transportadora
     * @param codTransportadora código da transportadora a atualizar
     * @param tempo double com o novo preço por minuto da transportadora
     * @param distancia double com o novo preço por km da transportadora
     */

    public void setPrecoTempoDistancia(String codTransportadora, double tempo, double distancia) throws ValorInvalidoException{
        if(tempo < 0 || distancia < 0) throw new ValorInvalidoException();

        Transportadora t = (Transportadora) this.distribuidores.get(codTransportadora);

        t.setPrecoPorKm(distancia);
        t.setPrecoPorMin(tempo);
    }

    /**
     * Método que atualiza a recolha de medicamentos de uma entidade médica
     * @param codTransportadora código da entidade
     * @param state novo estado de recolha de medicamentos
     */
    public void setRecolhaMedicamentos(String codTransportadora, boolean state){
        Distribuidor d = this.distribuidores.get(codTransportadora);

        if( d instanceof  TransportadoraMedica) {
            TransportadoraMedica tm = (TransportadoraMedica) d;

            tm.aceitaMedicamentos(state);
        }

        else{
            VoluntarioMedico vm = (VoluntarioMedico) d;

            vm.aceitaMedicamentos(state);
        }
    }


    
    /**
     * Método que retorna o estado da recolha de medicamentos de uma entidade médica
     * @param codTransportadora código da entidade 
     * @return estado de recolha de medicamentos
     */
    public boolean estouAAceitarMed(String codTransportadora){
        Distribuidor d = this.distribuidores.get(codTransportadora);

        if( d instanceof  TransportadoraMedica) {
            TransportadoraMedica tm = (TransportadoraMedica) d;

            return tm.aceitoTransporteMedicamentos();
        }

        else{
            VoluntarioMedico vm = (VoluntarioMedico) d;

            return vm.aceitoTransporteMedicamentos();
        }
    }

    public boolean estouARecolher(String codDistribuidor){
        return this.distribuidores.get(codDistribuidor).estaARecolher();
    }



    /**
     * Método que retorna a entrega ativa de um voluntário
     * @param codVol código do voluntário que possui a entrega
     * @return Entrega ativa do voluntário
     */
    public Entrega getEntregaAtiva(String codVol){
        Voluntario v = (Voluntario) this.distribuidores.get(codVol);

        return v.getEntregaAtiva();
    }


    /**
     * Método que retorna as entregas ativas de uma transportadora
     * @param codTrans código da transportadora da qual quer-se as entregas
     * @return Entregas ativas da transportadora
     */
    public Set<Entrega> getEntregasAtivas(String codTrans){
        Transportadora t = (Transportadora) this.distribuidores.get(codTrans);

        return t.getEntregasAtivas();
    }

    /**
     * Método que retorna o historico de entregas de um voluntario ou distribuidor em um dado período de tempo
     * @param cod código do user o qual será retornado o historico de entregas
     * @param aPartirDe data a partir de quando começa a busca por entregas no histórico
     * @return lista com as entregas do user indicado
     */
    public List<Entrega> getHistoricoEntregas(String cod, LocalDateTime aPartirDe, LocalDateTime ate){
        if( cod.charAt(0) == 'v' || cod.charAt(0) == 't'){
            return this.distribuidores.get(cod).getHistoricoEntregas().stream()
                    .filter(e -> e.getDataLevantamento().isAfter(aPartirDe))
                    .filter(e -> e.getDataFinal().isBefore(ate))
                    .collect(Collectors.toList());
        }
        else{
            return this.utilizadores.get(cod).getHistoricoEntregas().stream()
                    .filter(e -> e.getDataLevantamento().isAfter(aPartirDe))
                    .filter(e -> e.getDataFinal().isBefore(ate))
                    .collect(Collectors.toList());
        }
    }


    /**
     * Método que calcula os Kms feitos para se realizar uma entrega
     * @param e Encomenda para se calcular a distancia percorrida
     * @return double distancia percorrida para se realizar a encomenda
     */
    public double getKmsAndados(Encomenda e){
        Ponto a,b;
        a = this.lojas.get(e.getCodLoja()).getLocalizacao();
        b = this.utilizadores.get(e.getCodDest()).getLocalizacao();
        return a.distancia(b);
    }


    /**
     * Devolve o valor faturado por uma Empresa em um dado período de tempo
     * @param codTransportadora código da empresa a se verificar o valor faturado
     * @param aPartirDe valor da data da qual começa-se a buscar o faturamento
     * @param ate valor da data até quando se irá buscar o valor faturado
     */
    public double getValorFaturadoTrans(String codTransportadora,LocalDateTime aPartirDe,LocalDateTime ate){
        Transportadora trs = (Transportadora) this.distribuidores.get(codTransportadora);
        return trs.getValorFaturado(aPartirDe,ate);
    }


    /**
     * Devolve a cópia de um dado utilizador no sistema
     * @param codUtilizador código de utilizador a se obter a cópia
     * @return Instância de utilizador requerida
     */
    public Utilizador getPerfilUtilizador(String codUtilizador){
        return this.utilizadores.get(codUtilizador).clone();
    }

    /**
     * Devolve a cópia de um dado Distribuidor no sistema
     * @param codDistribuidor código de Distribuidor a se obter a cópia
     * @return Instância de Distribuidor requerida
     */
    public Distribuidor getPerfilDistribuidor(String codDistribuidor){
        return this.distribuidores.get(codDistribuidor).clone();
    }

    /**
     * Devolve a cópia de uma dada loja no sistema
     * @param codloja código de loja a se obter a cópia
     * @return Instância de loja requerida
     */
    public Loja getPerfilLoja(String codLoja){
        return this.lojas.get(codLoja).clone();
    }

    /**
     * Devolve uma lista com 10 transportadoras, ordenadas pela quantidade de kms percorridos para realizar entregas
     * @return lista com 10 transportadoras, ordenadas pela quantidade de kms percorridos para realizar entregas
     */
    public List<Transportadora> getTop10TransKms(){
        List<Transportadora> tr = this.distribuidores.values().stream().filter(t -> t instanceof Transportadora).map(t -> (Transportadora) t).map(Transportadora::clone).collect(Collectors.toList());
        TreeSet<Transportadora> r = new TreeSet<>(new ComparatorTransKms());
        r.addAll(tr);

        List<Transportadora> res = new ArrayList<>();

        int i = 0;
        for(Transportadora t : r){
            res.add(t.clone());
            i++;
            if(i == 10) break;
        }

    return res;
    }

    /**
     * Devolve uma lista com 10 utilizadores, ordenados pela quantidade de entregas solicitadas
     * @return lista com 10 utilizadores, ordenados pela quantidade de entregas solicitadas
     */
    public List<Utilizador> getTop10UserNumEntregas(){
        List<Utilizador> a = this.utilizadores.values().stream().map(Utilizador::clone).collect(Collectors.toList());

        TreeSet<Utilizador> r = new TreeSet<>(new ComparatorUtilizadoresEntregas());
        r.addAll(a);

        int i = 0;
        List<Utilizador> res = new ArrayList<>();
        for(Utilizador u : r){
            res.add(u.clone());
            i++;
            if(i == 10) break;
        }
        return res;
    }

    /**
     * Método que faz o parse de uma dada linha de logs para extrair informação a cerca de um utilizador
     * @param logLine String correspondente à linha do log que se deseja extrair informação
     */

    public void parseUtilizador(String logLine){

        String[] campos = logLine.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        String password = codUtilizador + "POO";
        String email = codUtilizador + "@emailTOP.com";
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        this.registaUtilizadorLogs(codUtilizador, email, password, nome, new Ponto(gpsx,gpsy));
    }


    /**
     * Método que faz o parse de uma dada linha de logs para extrair informação a cerca de uma loja
     * @param logLine String correspondente à linha do log que se deseja extrair informação
     */
    public void parseLoja(String logLine){

        String[] campos = logLine.split(",");
        String codLoja = campos[0];
        String nome = campos[1];
        String password = codLoja + "POO";
        String email = codLoja + "@emailTOP.com";
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        this.registaLojaLogs(codLoja , email, password, nome, new Ponto(gpsx,gpsy));
    }
    
    /**
     * Método que faz o parse de uma dada linha de logs para extrair informação a cerca de um voluntário
     * @param logLine String correspondente à linha do log que se deseja extrair informação
     */
    public void parseVoluntario(String logLine){

        String[] campos = logLine.split(",");
        String codVol = campos[0];
        String nome = campos[1];
        String password = codVol + "POO";
        String email = codVol + "@emailTOP.com";
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        this.registaVoluntarioLogs(codVol , email, password, nome, new Ponto(gpsx,gpsy),raio);
    }


    /**
     * Método que faz o parse de uma dada linha de logs para extrair informação a cerca de uma transportadora
     * @param logLine String correspondente à linha do log que se deseja extrair informação
     */
    public void parseTransportadora(String logLine){

        String[] campos = logLine.split(",");
        String codTrans = campos[0];
        String nome = campos[1];
        String password = codTrans + "POO";
        String email = codTrans + "@emailTOP.com";
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[5]);
        double precoKM = Double.parseDouble(campos[6]);
        this.registaTransportadoraLogs(codTrans, email, password, nome, new Ponto(gpsx,gpsy),precoKM,0,raio);
    }


    /**
     * Método que faz o parse de uma dada linha de logs para extrair informação a cerca de uma encomenda
     * @param logLine String correspondente à linha do log que se deseja extrair informação
     */
    public void parseEncomenda(String logLine) throws ProdutoInexistenteException, ValorInvalidoException{
        String[] campos = logLine.split(",");
        String codEnc = campos[0];
        String codDest = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);

            for (int i = 4; i < campos.length; i += 4) {
                this.registaProdutoLogs(campos[i], campos[i + 1], Double.parseDouble(campos[i + 3]), 42, false);
                this.addLinhaEncomenda(codDest, this.getProduto(campos[i]), Double.parseDouble(campos[1 + 2]));
            }

            this.criaEncomendaLogs(codEnc, peso, codDest, codLoja);
    }



    /**
     * Método que realiza a aceitação automática de encomendas marcadas como aceites nos logs
     * @param codEnc String correspondente ao código da encomenda a ser automaticamente aceita
     */
    public void aceitaEncomendasLog(String codEnc) throws NullPointerException,EncomendaInvalidaException,SemDistribuidoresException{
        Utilizador u = this.utilizadores.values().stream().filter(ut -> ut.getPedidosEncomenda().containsKey(codEnc)).findAny().orElse(null);
        String codDest = u.getUsername();
        Optional<Map<String,Double>> r = this.requisitaEntrega(codDest,codEnc);
        if (r.isPresent()) {
            String codDistribuidor = r.get().keySet().stream().findAny().orElse(null);
            this.transportadoraAceitaEntrega(codDistribuidor, codEnc, codDest);
        }
    }


    /**
     * Método que extrai as informações de um ficheiro de logs para a aplicação
     * @param nfich String com o nome do ficheiro de logs
     */
    public void parse(String nfich) throws LogsInvalidosException{
        List<String> linhas = lerFicheiro(nfich); 
        String[] linhaPartida;

        try {
            for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch (linhaPartida[0]) {
                    case "Utilizador":
                        this.parseUtilizador(linhaPartida[1]);
                        break;

                    case "Loja":
                        this.parseLoja(linhaPartida[1]);
                        break;

                    case "Voluntario":
                        this.parseVoluntario(linhaPartida[1]);
                        break;

                    case "Transportadora":
                        this.parseTransportadora(linhaPartida[1]);
                        break;

                    case "Encomenda":
                        this.parseEncomenda(linhaPartida[1]);
                        break;

                    case "Aceite":
                        this.aceitaEncomendasLog(linhaPartida[1]);
                        break;

                    default:
                        System.out.println("Linha inválida.");
                        break;
                }
            }
        }

        catch (Exception e){
            throw new LogsInvalidosException();
        }
        System.out.println("done!");
    }


    /**
     * Método que pré processa o input de um ficheiro de logs para realizar a extração da informação
     * @param nomeFich String com o nome do ficheiro de logs
     * @return Lista com cada uma das linhas do ficheiro de logs
     */

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }

    /**
     * Método que regista Produtos baseados em informações extraidas de um ficheiro de Logs
     */
    public void registaProdutoLogs(String cod, String nome, double custo, double peso, boolean isMedicamento){
        Produto p = new Produto(cod,nome,custo,peso, isMedicamento);
        this.produtos.put(p.getCodProd(),p);
    }


    /**
     * Método que regista Produtos baseados em informações extraidas de um ficheiro de Logs
     */
    public String registaUtilizadorLogs(String cod, String email, String password, String nome, Ponto p){
        Utilizador u = new Utilizador(cod,password,nome,email,p,new ArrayList<>());
        this.utilizadores.put(u.getUsername(),u);
        return u.getUsername();
    }
    

    /**
     * Método que regista uma Loja baseada em informações extraidas de um ficheiro de Logs
     */
    public String registaLojaLogs(String cod, String email, String password, String nome, Ponto p){
        Loja l = new Loja(cod,password,nome,email,new HashMap<>(), p);
        this.lojas.put(l.getUsername(),l);
        return l.getUsername();
    }
    

    /**
     * Método que regista uma Transportadora baseada em informações extraidas de um ficheiro de Logs
     */
    public String registaTransportadoraLogs(String cod, String email, String password, String nome, Ponto p, double precoPorKm, double precoPorMin, double raio){
        Transportadora t = new Transportadora(cod,password,nome,email,true,precoPorKm,precoPorMin,new ArrayList<>(), new Classificacao(), raio, p);
        this.distribuidores.put(t.getUsername(),t);
        return t.getUsername();
    }

    /**
     * Método que regista um Voluntario baseado em informações extraidas de um ficheiro de Logs
     */
    public String registaVoluntarioLogs(String cod ,String email, String password, String nome, Ponto p, double raio){
        Voluntario v = new Voluntario(cod,password,nome,email,raio, new Classificacao(),true,p,new ArrayList<>());
        this.distribuidores.put(v.getUsername(),v);
        return v.getUsername();
    }

    /**
     * Método que regista uma encomenda baseada em informações extraidas de um ficheiro de Logs
     */
    public void criaEncomendaLogs(String codEnc, double peso, String username, String codLoja){
        Utilizador u = this.utilizadores.get(username);
        List<LinhaEncomenda> linhas = u.getNovasLinhas();
        Encomenda e = new Encomenda(codEnc, peso, linhas,username,codLoja);
        u.addPedidoEncomenda(e);
        u.setNovasLinhas(new ArrayList<>());
    }

    /**
     * Método que verifica se um dado distribuidor é médico
     * @param cod Strin com o código do voluntário
     * @return boolean que indica se o dado distribuidor é médico
     */
    public boolean isMedico(String cod){
        if(!this.distribuidores.containsKey(cod)) return false;

        Distribuidor d = this.distribuidores.get(cod);

        return d instanceof TransportadoraMedica || d instanceof VoluntarioMedico;
    }

    /**
     * Método que atualiza o código global de um produto dado um ficheiro obj carregado
     */
    public void upCodProd(){
        for(Produto p : this.produtos.values()) {
            Produto.updateCodGlobal(p.getCodProd());
        }
    }

    /**
     * Método que atualiza o código global de uma loja dado um ficheiro obj carregado
     */
    public void upLoja(){
        for(Loja l : this.lojas.values()) {
            Loja.updateCodGlobal(l.getUsername());
        }
    }


    /**
     * Método que atualiza o código global dos utilizadores dado um ficheiro obj carregado
     */
    public void upUtilz(){
        for(Utilizador u : this.utilizadores.values()) {
            Utilizador.updateCodGlobal(u.getUsername());
        }
    }


    /**
     * Método que atualiza o código global dos distribuidores dado um ficheiro obj carregado
     */
    public void upDistr(){
        for(Distribuidor d : this.distribuidores.values()) {

            if (d instanceof Voluntario) {
                Voluntario.updateCodGlobal(d.getUsername());
            }

            if (d instanceof Transportadora) {
                Transportadora.updateCodGlobal(d.getUsername());
            }
            
        }
    }
    
    /**
     * Método que atualiza o código global de uma Encomenda dado um ficheiro obj carregado
     */
    public void upCodEnc(){
        ArrayList<Encomenda> all  = new ArrayList<>();
        for (Utilizador u: this.utilizadores.values()){
            all.addAll(u.getPedidosEncomenda().values());
            all.addAll(u.getHistoricoEntregas().stream().map(Entrega :: getEncomenda).collect(Collectors.toList()));
        }

        for (Loja l: this.lojas.values()){
            all.addAll(l.getEncomendasAceites().values().stream().map(Entrega :: getEncomenda).collect(Collectors.toList()));
        }

        for (Distribuidor d: this.distribuidores.values()){
            all.addAll(d.getHistoricoEntregas().stream().map(Entrega :: getEncomenda).collect(Collectors.toList()));

            if (d instanceof Voluntario){
                Voluntario v = (Voluntario) d;
                all.add(v.getEntregaAtiva().getEncomenda());
            }
            
            if (d instanceof Transportadora){
                Transportadora t = (Transportadora) d;
                all.addAll(t.getEntregasAtivas().stream().map(Entrega :: getEncomenda).collect(Collectors.toList()));
            }
        }

        for (Encomenda e: all){
            Encomenda.updateCodGlobal(e.getCodEnc());
        }
    }

    
}
