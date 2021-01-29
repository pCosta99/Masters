/**
 *
 */
package Modelo;

import java.lang.Math;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

public class SistemaGestaoEntregas implements ISistemaGestaoEntregas {
    private Map<String, Utilizador> utilizadores;
    private Map<String, Voluntario> voluntarios;
    private Map<String, Transportadora> empresas;
    private Map<String, Loja> lojas;
    private Map<String, Encomenda> encomendas;
    private Set<String> aceites;

    //CONTRUTORES

    /**
     * Contrutor vazio
     */
    public SistemaGestaoEntregas() {
        this.utilizadores = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.empresas = new HashMap<>();
        this.lojas = new HashMap<>();
        this.encomendas = new HashMap<>();
        this.aceites = new TreeSet();
    }

    /**
     * Contrutor parametrizado
     * @return String
     */
    @Override
    public String toString() {
        return "SistemaGestaoEntregas { \n" +
                " \n utilizadores = \n" + this.utilizadores.toString() +
                " \n voluntários = \n" + this.voluntarios.toString() +
                " \n empresas = \n" + this.empresas.toString() +
                " \n lojas = \n" + this.lojas.toString() +
                "\n encomendas = \n" + this.encomendas.toString() +
                " \n Aceite = \n" + this.aceites.toString() + "}";
    }

    /**
     * Contrutor clone
     * @param cod codigo
     * @param nome nome
     * @param gps cordenadas gps
     * @param raio raio de ação
     * @param encomendas encomendas
     * @param passe password
     * @param disponivel disponiblidade
     * @param med medico
     */
    public void newVoluntario(String cod, String nome, Gps gps, Double raio, Set<String> encomendas, String passe, Boolean disponivel, Boolean med){
        Voluntario u = new Voluntario(cod, nome, gps, raio, encomendas, passe, disponivel, med);
        this.voluntarios.put(cod,u);
    }

    /**
     * Método que cria um novo utilizador
     * @param codUtilizador é o codigo do utilizador
     * @param nome nome do utilizador
     * @param gps cordenadas do utilizador
     * @param pass password do utilizador
     * @param encomendas encomendas do utilizador
     */
    public void newUtilizador(String codUtilizador, String nome, Gps gps, String pass, Set<String> encomendas) {
        Utilizador u = new Utilizador(codUtilizador, nome, gps, pass, encomendas);
        this.utilizadores.put(codUtilizador, u);
    }

    //ADD

    /**
     * Método que adiciona um utilizador a lista de utilizadores
     * @param u utilizador
     */
    public void addUtilizador (Utilizador u ){
        this.utilizadores.put(u.getCodUtilizador() , u);
    }

    /**
     * Método que adiciona um voluntario a lista de voluntarios
     * @param v voluntario
     */
    public void addVoluntario (Voluntario v){
        this.voluntarios.put(v.getCodVoluntario() , v);
    }

    /**
     * Método que adiciona uma encomenda a lista de encomendas
     * @param e encomenda
     */
    public void addEncomenda (Encomenda e){
        this.encomendas.put(e.getCodEncomenda() , e);
        this.utilizadores.get(e.getCodUtilizador()).addEncomenda(e.getCodEncomenda());
    }

    /**
     * Método que adiciona uma Transportadora a lista de empresas
     * @param t transportadora
     */
    public void addEmpresa (Transportadora t){
        this.empresas.put(t.getCodEmpresa() , t);
    }

    /**
     * Método que adiciona uma loja a lista de lojas
     * @param l loja
     */
    public void addLoja (Loja l){
        this.lojas.put(l.getCodLoja() , l);
    }

    /**
     * Método que cria uma loja nova
     * @param codLoja odigo da loja
     * @param nome nome da loja
     * @param gps cordenadas do gps
     * @param disponivel disponiblidade
     * @param pass password
     * @param catalogo caatalogo da loja
     */
    public void newLoja (String codLoja , String nome , Gps gps , boolean disponivel , String pass , Set<Produto> catalogo ) {
        Loja l = new Loja(codLoja, nome, gps, disponivel, pass, catalogo, new ArrayList<>());
        this.lojas.put(codLoja, l);
    }

    /**
     * Método que adiciona uma encomenda a lista de encomendas do utilizador e a lista de encomendas da loja
     * @param codUtilizador codigo utilizador
     * @param codLoja codigo da loja
     * @param e encomenda
     */
    public void addEncomenda (String codUtilizador , String codLoja , Encomenda e){
        this.utilizadores.get(codUtilizador).addEncomenda(e.getCodEncomenda());
        this.lojas.get(codLoja).addEncomenda(e.getCodEncomenda());
    }

    /**
     * Método que adiciona a encomenda a lista de encomendas entreges da empresa e a lista de encomendas entregues
     * @param codTransportadora codigo da transportadora
     * @param codEncomenda codigo da encomenda
     */
    public void addEntregueTransportadora (String codTransportadora , String codEncomenda){
        encomendasEntregues(codEncomenda);
        this.empresas.get(codTransportadora).addEntregues(codEncomenda);
    }

    /**
     * Método que adiciona a encomenda a lista de encomendas entreges pelo voluntario e a lista de encomendas entregues
     * @param codVoluntario codigo do voluntario
     * @param encCode codigo da encomenda
     */
    public void addEntregueVoluntario (String codVoluntario , String encCode){
        encomendasEntregues(encCode);
        this.voluntarios.get(codVoluntario).addEncomenda(encCode);
    }

    /**
     * Método que adiciona uma encomenda a lista de encomendas aceites
     * @param codEncomenda codigo da encomenda
     */
    public void addAceite (String codEncomenda ){
        this.aceites.add(codEncomenda);
        this.encomendas.get(codEncomenda).setAceite(true);
    }

    /**
     * Método que adiciona um produto a uma loja
     * @param codLoja codigo da loja
     * @param produtos produto
     */
    public void addProduto(String codLoja , Set<Produto> produtos){
        this.lojas.get(codLoja).addProdutos(produtos);
    }

    /**
     * Método que adiciona uma encomenda a lista de encomendas de uma loja
     * @param codLoja codigo loja
     * @param codEnc codigo encomenda
     */
    public void addEncomendas (String codLoja, String codEnc){
        lojas.get(codLoja).addEncomenda(codEnc);

    }
    //EXISTE

    /**
     * Método que determina se existe, ou não, o utilizador com a password "pass"
     * @param codUtilizador codigo utilizado para identificar o utilizador
     * @param pass password do input do utilizador
     * @return boolean
     */
    public boolean existeUtilizador(String codUtilizador, String pass) {
        if(!this.utilizadores.containsKey(codUtilizador)) return false;
        else
            return this.utilizadores.get(codUtilizador).getPassword().equals(pass);

    }

    /**
     * Método que determina se existe uma loja, ou não, com a password "pass"
     * @param codLoja codigo utilizado para identificar a loja
     * @param pass password do input da loja
     * @return boolean
     */
    public boolean existeLoja(String codLoja, String pass) {
        if(!this.lojas.containsKey(codLoja)) return false;
        else
            return this.lojas.get(codLoja).getPassword().equals(pass);

    }

    /**
     * Método que determina se existe um voluntario, ou não, com a password "pass"
     * @param codVol codigo utilizado para identificar ovoluntario
     * @param pass password do input do voluntario
     * @return boolean
     */
    public boolean existeVoluntario(String codVol , String pass){
        if(this.voluntarios.get(codVol).getPass().equals(pass)){
            return true;
        }
        else return false;
    }

    /**
     * Método que determina se existe, ou não, uma transportadora com a password "pass"
     * @param cod codigo utilizado para identificar a transportadora
     * @param pass password do input da transportadora
     * @return boolean
     */
    public boolean existeTransportadora(String cod, String pass) {
        if (!this.empresas.containsKey(cod)) return false;
        else
            return this.empresas.get(cod).getPassword().equals(pass);
    }

    public void newLoja (String codLoja , String nome , Gps gps , boolean disponivel , String pass){
        Loja l = new Loja(codLoja , nome , gps, disponivel, pass, new TreeSet<>(), new ArrayList<>());
        this.lojas.put(codLoja, l);
    }

    //MÉTODOS DO UTILIZADOR

    /**
     * Método que altera a password do Utilizador
     * @param password nova pass
     * @param codUtilizador codigo do utilizador
     */
    public void alterarPassword (String password , String codUtilizador){
        this.utilizadores.get(codUtilizador).setPassword(password);
    }

    /**
     * Método que mostra as encomendas que foram aceites
     * @param codUtilizador codigo utilizador
     * @return List<String> encomendas
     */
    public List<String> encomendasUtilizador(String codUtilizador){
        List<String> ret = new ArrayList<>();
        for (String cod : aceites) {
            Encomenda e = encomendas.get(cod);
            if (e.getCodUtilizador().equals(codUtilizador))
                ret.add(e.getCodEncomenda());
        }
        return ret;
    }

    /**
     * Método que devolve as encomendas que foram aceites mas ainda não foram entregues
     * @param codUtilizador codigo utilizador
     * @return List<String> Encomendas
     */
    public List<String> encomendasPorEntregar (String codUtilizador){
        List<String> encomendas = new ArrayList<>();
        for (Encomenda e: this.encomendas.values()) {
            if(e.getCodUtilizador().equals(codUtilizador) && !e.isEntregue() && e.isAceite()){
                encomendas.add(e.getCodEncomenda());
            }
        } return encomendas;
    }

    /**
     * Método que devolve as encomendas que já foram entregues ao utilizador
     * @param codUtil codigo utilizador
     * @return List<String> encomendas
     */
    public List<String> utilizadorEncomendasNEntregues(String codUtil) {
        return encomendas.values().stream().filter(e -> (e.getCodUtilizador().equals(codUtil))
                && !e.isEntregue()).map(Encomenda::getCodEncomenda).collect(Collectors.toList());
    }

    // MÉTODOS LOJA

    /**
     * Método que torna a encomenda entregue e adiciona a encomenda as encomendas do utilizador
     * @param codEnc codigo encomenda
     */
    public void encomendasEntregues(String codEnc){
        this.encomendas.get(codEnc).setEntregue(true);
        this.utilizadores.get(this.encomendas.get(codEnc).getCodUtilizador()).addEncomenda(codEnc);
    }
    /**
     * Método que muda a disponiblidade da loja, se tiver disponivel, fica indesponivel e vice-versa
     * @param codLoja codigo que identifica a loja
     */
    public void sinalizarDisponibilidade (String codLoja){
        this.lojas.get(codLoja).sinalizarDisponibilidade();
    }

    /**
     *Método que devolve o codigo da loja que contem a encomenda e
     * @param e encomenda
     * @return loja
     */
    public String qualLoja(Encomenda e){
        return e.getCodLoja();
    }

    /**
     * Método que devolve as lojas disponiveis
     * @return lojas
     */
    public Map<String , String> lojasDisponiveis(){
        Map<String , String> lojas = new HashMap<>();
        for (Loja l : this.lojas.values()) {
            if(l.isDisponivel()){
                lojas.put(l.getCodLoja() , l.getNomeLoja());
            }
        } return lojas;
    }

    /**
     * Método que imprime o catalogo da loja
     * @param codLoja
     * @return
     */
    public Set<Produto> imprimeCatalogo (String codLoja){
        return this.lojas.get(codLoja).getCatalogo();
    }

    /**
     * Método que devolve as Encomendas que foram aceites pela loja
     * @param codLoja codigo loja
     * @return encomendas aceites
     */
    public List<String> lojaEncomendasNAceites(String codLoja) {
        return encomendas.values().stream().filter(e -> (e.getCodLoja().equals(codLoja))
                && !e.isAceite()).map(Encomenda::getCodEncomenda).collect(Collectors.toList());
    }

    //MÉTODOS TRANSPORTADORA

    /**
     * Método que determina se existe uma transportadora com o codigo "cod"
     * @param cod codigo transportadora
     * @return transportadora
     */
    public boolean containsTransportadora(String cod){
        return this.empresas.containsKey(cod);
    }

    /**
     *
     * @param codEmpresa codigo empresa
     * @param nomeEmpresa nome empresa
     * @param lat latitude
     * @param lon longitude
     * @param nif nif
     * @param raio raio
     * @param precokm preço por km
     * @param password pass
     * @param precokg preço/kg
     * @param medico medico
     * @param disponivel disponiblidade
     * @param entregues
     * @param rating rating
     * @param nrating numero de ratings
     */
    public void newTransportadora(String codEmpresa, String nomeEmpresa,double lat, double lon, String nif, double raio,
                                  double precokm, String password, Double precokg , boolean medico , boolean disponivel, TreeSet<String> entregues, double rating , int nrating){
        Gps g = new Gps(lat,lon);
        Transportadora t = new Transportadora(codEmpresa, nomeEmpresa, g, nif, raio, precokm, password, precokg, medico , disponivel, new TreeSet<String>(), rating, nrating );

    }

    /**
     * Método que determina as possiveis transportadoras
     * @param
     * @return
     */
    public Set<String> possiveisTransportadoras(String e) {
        boolean medico = this.encomendas.get(e).isMedico();
        Gps gps = this.utilizadores.get(this.encomendas.get(e).getCodUtilizador()).getGps();
        Gps gpsLoja = this.lojas.get(this.encomendas.get(e).getCodLoja()).getGps();
        return this.empresas.values().stream().filter(transportadora -> ((transportadora.isMedico() || !medico) && (transportadora.getGps().distanciaCoordenadas(gps)) < transportadora.getRaio()
                && (transportadora.getGps().distanciaCoordenadas(gpsLoja) < transportadora.getRaio()))).map(Transportadora::getCodEmpresa).collect(Collectors.toSet());
    }

    /**
     * Método que determina a distancia percorrida pela transportadora para receber a entrega e entrega-la
     * @param codEncomenda codigo encomenda
     * @param codEmpresa codigo empresa
     * @return distanciaa
     */
    public double distanciaTransportadora (String codEncomenda , String codEmpresa){
        return this.empresas.get(codEmpresa).getGps().distanciaCoordenadas(this.lojas.get(this.encomendas.get(codEncomenda).getCodLoja()).getGps())
                + this.lojas.get(this.encomendas.get(codEncomenda).getCodLoja()).getGps().distanciaCoordenadas(this.utilizadores.get(this.encomendas.get(codEncomenda).getCodUtilizador()).getGps());
    }

    /**
     * Método que determnina o preço da encomenda, valor o qual o utilizador tem de concordar pagar
     * @param codEncomenda codigo encomenda
     * @param codEmpresa codigo empresa
     * @return preço encomenda
     */
    public double precoEncomenda(String codEncomenda, String codEmpresa ) {
        double distancia = distanciaTransportadora(codEncomenda , codEmpresa);
        return distancia * this.empresas.get(codEmpresa).getPrecoKm() + this.encomendas.get(codEncomenda).getPeso() * this.empresas.get(codEmpresa).getPrecokg() + this.encomendas.get(codEncomenda).precoEncomenda();
    }

    /**
     * Método que devolve a transportadora que percorre a menor distancia
     * @param codEncomenda codigo encomenda
     * @param transportadoras transportadoras
     * @return transportadora
     */
    public String escolheTransportadora (String codEncomenda,Set<String > transportadoras){
        String minimo = "";
        double minimoDist = 0;
        double distancia =0;
        for (String s : transportadoras) {
            distancia = distanciaTransportadora(codEncomenda , s);
            if(distancia<minimoDist){
                minimo = s ;
                minimoDist = distancia;
            }
        } return minimo;
    }

    /**
     * Método que remove uma transportadora
     * @param transportadoras transportadoras
     * @param codeTransportadora cod transportadora
     */
    public void removeTransportadora (Set<String> transportadoras , String codeTransportadora){
        transportadoras.remove(codeTransportadora);
    }

    /**
     * Devolve a lista de empresas transportadoras
     * @return empresas
     */
    public Map<String, Transportadora> getEmpresas(){
        return new HashMap<>(this.empresas);
    }
    /*
    public void encomendasEntregues(String codEnc){
        this.encomendas.get(codEnc).setEntregue(true);
    }

     */

    /**
     * Método que atualiza a classificação da empresa de 0 a 5
     * @param codEmpresa codigo empresa
     * @param rating rating
     * @return classificação
     */
    public double classificacao(String codEmpresa, int rating){
        return this.empresas.get(codEmpresa).classificacao(rating);
    }

    /**
     * Método que determina o tempo que demorou a fazer a encomenda
     * @param codEncomenda codigo encomenda
     * @param codEmpresa codigo empresa
     * @return tempo da encomenda
     */
    public double tempoVoluntario(String codEncomenda , String codEmpresa){
        double dist = this.distanciaVoluntario(codEncomenda, codEmpresa);
        return dist * Math.random()*((120-60)+1)+60 * transitoetempo();
    }

    //MÉTODOS VOLUNTARIOS

    /**
     *Método que devolve possiveis voluntarios
     * @param e
     * @return
     */
    public Set<String> possiveisVoluntarios (String e){
        boolean medico = this.encomendas.get(e).isMedico();
        Gps gpsLoja = this.lojas.get(this.encomendas.get(e).getCodLoja()).getGps();
        Gps gps = this.utilizadores.get(this.encomendas.get(e).getCodUtilizador()).getGps();
        return this.voluntarios.values().stream().filter(voluntario -> ((voluntario.isMedico() || !medico) && voluntario.isDisponivel() && (voluntario.getGps().distanciaCoordenadas(gps)) < voluntario.getRaio()
                                                                    && (voluntario.getGps().distanciaCoordenadas(gpsLoja) < voluntario.getRaio()))).map(Voluntario::getCodVoluntario).collect(Collectors.toSet());
    }

    /**
     * Método que determina a distancia que o voluntario tem que percorrer para entregar a encomenda
     * @param codEncomenda codigo encomenda
     * @param codVoluntario codigo voluntario
     * @return distancia
     */
    public double distanciaVoluntario(String codEncomenda , String codVoluntario){
        return this.voluntarios.get(codVoluntario).getGps().distanciaCoordenadas(this.lojas.get(this.encomendas.get(codEncomenda).getCodLoja()).getGps())
                + this.lojas.get(this.encomendas.get(codEncomenda).getCodLoja()).getGps().distanciaCoordenadas(this.utilizadores.get(this.encomendas.get(codEncomenda).getCodUtilizador()).getGps());
    }

    /**
     * Método que escolhe o Voluntario que percorre a menor distancia para entregar a encomenda
     * @param codEncomenda codigo encomenda
     * @param voluntarios voluntarios
     * @return voluntario
     */
    public String escolheVoluntario (String codEncomenda,Set<String> voluntarios){
        String minimo = "";
        double minimoDist = 1000000000;
        double distancia =0;
        for (String s : voluntarios) {
            distancia = distanciaVoluntario(codEncomenda , s);
            if(distancia<minimoDist){
                minimo = s ;
                minimoDist = distancia;
            }
        } return minimo;
    }

    /**
     * Método que imprime as encomendas do voluntario
     * @param codVoluntario codigo voluntario
     */
    public void imprimeEncomendas (String codVoluntario){
        this.voluntarios.get(codVoluntario).encomendasToString();
    }

    /**
     * Método que modifica a palavra passe do voluntario
     * @param codVoluntario coidgo voluntario
     * @param pass password
     */
   public void setVoluntariosPass( String codVoluntario, String pass) {
        this.voluntarios.get(codVoluntario).setPass(pass);
   }
   //MÉTODOS ENCOMENDA

    /**
     *Método que cria e devolve uma nova linha de encomendas
     * @param produtos produtos
     * @param codProduto codigo produtos
     * @param quantidade quantidade
     * @return linha de encomendas
     */
    public LinhaEncomenda novaLinhaEncomenda(Set<Produto> produtos , String codProduto , double quantidade){
        LinhaEncomenda l = new LinhaEncomenda();
        for (Produto p : produtos) {
            if(p.getCodProduto().equals(codProduto)){
                l.setCodProduto(codProduto);
                l.setDescricao(p.getDescricao());
                l.setValorUnitario(p.getValorUnitario());
                l.setQuantidade(quantidade);
                l.setMedico(p.isMedico());
            }
        } return l;
    }

    /**
     *Método que devolve true se o produto for medico e false se não for
     * @param produtos produtos
     * @param codProduto codigo de produtos
     * @return boolean
     */
    public boolean produtoMedico (Set<Produto> produtos , String codProduto ){
        for (Produto p :
                produtos) {
            if(p.getCodProduto().equals(codProduto)){
                if(p.isMedico()) return true;
                else return  false;
            }
        } return false;
    }

    /**
     *Método que devolve a lista de encomendas
     * @return lista de encomendas
     */
    public Map<String , Encomenda> getEncomendas(){
        return new HashMap<>(this.encomendas);
    }

    /**
     * Método que garante que há uma key que corresnponde a encomenda
     * @param codEncomenda codigo de encomenda
     * @return boolean
     */
    public boolean containsKey(Double codEncomenda){
       return this.encomendas.containsKey("e" + codEncomenda);
    }

    /**
     *
     * @param codEncomenda cod encomenda
     * @param codVoluntario cod voluntario
     */
    public void addTempoVol(String codEncomenda , String codVoluntario){
        this.encomendas.get(codEncomenda).setTempoEntrega(tempoVoluntario(codEncomenda,codVoluntario));
    }

    /**
     *
     * @param codEncomenda codigo de encomendas
     * @param codEmpresa codigo de empresa
     * @return double
     */
    public double tempoTransportadora(String codEncomenda , String codEmpresa){
        double dist = this.distanciaTransportadora(codEncomenda, codEmpresa);
        return dist * Math.random()*((120-60)+1)+60 * transitoetempo();
    }

    /**
     * Calcula o tempo de entrega
     * @param codEncomenda codigo encomendas
     * @param codEmpresa codigo empresa
     */
    public void addTempoTrans(String codEncomenda , String codEmpresa){
        this.encomendas.get(codEncomenda).setTempoEntrega(tempoTransportadora(codEncomenda,codEmpresa));
    }

    /**
     * Método que calcula um double a partir de probabilidades
     * @return double
     */
    public double transitoetempo(){
        double goodtimes = Math.random() * ((50)+1);
        if(goodtimes < 1.5) return 4;       //catastrofre natural
        else if(goodtimes < 5) return 1.8;  //mau tempo e transito muito congestionado
        else if(goodtimes < 20) return 1.4; //mau tempo e/ou transito congestionado
        else if(goodtimes < 45) return 1.2; //bom tempo com pouco transito
        else return 1;                      // entregas sem transito
    }

    /**
     * Método que devolve o historico de encomendas entre 2 datas
     * @param codUtilizador codigo utilizador
     * @param max data max
     * @param min data min
     * @return encomendas
     */
    public Set<Encomenda> encomendasPorData (String codUtilizador, LocalDate max , LocalDate min){
        Set<Encomenda> res = this.encomendas.values().stream().filter(e -> e.getCodUtilizador().equals(codUtilizador)).filter(e -> e.getDataDeEntrega().isBefore(max) && e.getDataDeEntrega().isAfter(min)).collect(Collectors.toSet());
        return res;
    }

    /**
     * Método que recebe um cod encomenda e altera a data da encomenda
     * @param encCode cod encomenda
     */
    public void setDataEntrega (String encCode){
        this.encomendas.get(encCode).setDataDeEntrega(LocalDate.now());
    }

    /**
     * Método que retira a encomenda de um voluntario
     * @param encCode codigo encomenda
     * @param codvoluntario codigo voluntario
     */
    public void retirarEncVoluntario (String encCode , String codvoluntario){
        this.voluntarios.get(codvoluntario).removeEncomenda(encCode);
    }

    /**
     *  Método que adiciona uma encomenda a um voluntario
     * @param encCode codigo encomenda
     * @param codVoluntario codigo voluntario
     */
    public void adicionaEncVoluntario (String encCode , String codVoluntario){
        this.voluntarios.get(codVoluntario).addEncomenda(encCode);
    }

    public Map<String, Utilizador> getUtilizadores() {
        return new HashMap<>(this.utilizadores);
    }

    /**
     * Método que devolve os top 10 utilizadores
     * @param utilizadores utilizadores
     * @return utilizadores
     */
    public Set <Utilizador> top10Utilizadores(Map<String, Utilizador> utilizadores){
        return utilizadores.values().stream().sorted().map(Utilizador::clone).limit(10).collect(Collectors.toSet());
    }
    public Set<Transportadora> top10Transportadora(Map<String, Transportadora> transportadoras){
        return transportadoras.values().stream().sorted().map(transportadora -> transportadora.clone()).limit(10).collect(Collectors.toSet());
    }

    /**
     *
     * @return codigo utilizadores
     */
    public Set<String> codUtilizadores (){
        return this.utilizadores.values().stream().map(Utilizador::getCodUtilizador).collect(Collectors.toSet());
    }
    public boolean temCodUtilizador(String codUtilizador){
        Set<String> novo = new TreeSet<> (codUtilizadores());
        return novo.contains(codUtilizador);
    }

    /**
     * Método que retorna de um set com os códigos de voluntários
     * @return Set de Strings
     */
    public Set<String > codVoluntarios(){
        return this.voluntarios.values().stream().map(voluntario -> voluntario.getCodVoluntario()).collect(Collectors.toSet());
    }

    /**
     * Retorna um booleano que indica se um determinado código de voluntário já existe
     * @param codVol Código de voluntário
     * @return True ou false, se existir ou não , respetivamente
     */
    public boolean temCodVoluntarios(String codVol){
        Set<String> novo = new TreeSet<>(codVoluntarios());
        return novo.contains(codVol);
    }
    /**
     * Método que devolve o cod loja
     * @return codigo da loja
     */
    public Set<String> codLoja(){
        return this.lojas.values().stream().map(Loja::getCodLoja).collect(Collectors.toSet());
    }

    /**
     *
     * @param codLoja cod loja
     * @return boolean
     */
    public boolean temCodLoja (String codLoja){
        Set<String> novo = new TreeSet<>(codLoja());
        return novo.contains(codLoja);
    }

    /**
     * Método que adiciona uma encomenda a loja
     * @param codLoja cod loja
     * @param encCode cod encomenda
     */
    public void addEncomendaLoja (String codLoja , String encCode){
        this.lojas.get(codLoja()).addEncomenda(encCode);
    }

    /**
     * Método que devolve as encomendas por aceitar
     * @param codLoja codigo loja
     * @return encomendas
     */
    public List<String> encomedasPorAceitar (String codLoja){
        List<String> res = new ArrayList<>();
        for (String s : this.lojas.get(codLoja).getEncomendas()) {
            if(!this.encomendas.get(s).isAceite()){
                res.add(s);
            }
        } return res;
    }

    /**
     * M+etodo que devolve o total faturado
     * @param codLoja cod loka
     * @return total faturado
     */
    public double totalFaturado(String codLoja){
        double res = 0;
        for (String e : this.lojas.get(codLoja).getEncomendas()) {
           res+= this.encomendas.get(e).precoEncomenda();
        } return res;
    }

    /**
     * Método que devolve o tamanho da fila de espera
     * @param codLoja cod loja
     * @return tamanho da fila de espera
     */
    public int tamanhoFilaEspera(String codLoja){
        int res =0 ;
        res = this.lojas.get(codLoja).getEncomendas().size() - lojaEncomendasNAceites(codLoja).size();
        return res;
    }

    public Set<String> codProdutos(){
        Set<String> res = new HashSet<>();
        for (Loja loja: this.lojas.values()){
           res.addAll(loja.getCatalogo().stream().map(Produto::getCodProduto).collect(Collectors.toSet()));
        }
        return res;
    }
    public boolean temCodProduto(String codProduto){
        Set<String> novo = new TreeSet<>(codProdutos());
        return novo.contains(codProduto);
    }

    public Produto newProduto (String codProduto , String descricao , double valorUnitario , boolean medico){
        return new Produto(codProduto , descricao , valorUnitario , medico);
    }

    public void alterarPassTransportadora(String codEmp, String pass) {
        this.empresas.get(codEmp).setPassword(pass);
    }

    public void alterarPassVoluntario(String codVol, String pass) {
        this.voluntarios.get(codVol).setPass(pass);
    }

    public void alterarPassLoja(String codLoja, String pass) {
        this.lojas.get(codLoja).setPassword(pass);
    }

    public Set<String> historicoVol(String codVol) {
        return this.voluntarios.get(codVol).getEncomendas();
    }

    public List<String> voluntarioEncomendasNEntregues(String codVoluntario) {
        List<String> res = new ArrayList<>();
        for (String s : this.voluntarios.get(codVoluntario).getEncomendas()) {
            if (this.encomendas.get(s).isAceite() && !this.encomendas.get(s).isEntregue())
                res.add(s);
        }
        return res;
    }

}
