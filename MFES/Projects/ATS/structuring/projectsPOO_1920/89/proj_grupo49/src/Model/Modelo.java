package Model;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.*;

public class Modelo implements Serializable {
    private Lojas lojas;
    private Transportadoras transportadoras;
    private Voluntarios voluntarios;
    private Utilizadores utilizadores;
    private Map<String, Encomenda> encomendas;
    private Set<Produto> produtos;


    /**
     * Construtor parametrizado
     * @param lojas     Map com lojas a copiar
     * @param transportadoras Map com transportadoras
     * @param voluntarios     Map com voluntarios
     * @param utilizadores    Map com utilizadores
     * @param encomendas      Map com encomendas
     */
    public Modelo(Lojas lojas, Transportadoras transportadoras, Voluntarios voluntarios, Utilizadores utilizadores, Map<String, Encomenda> encomendas) {
        this.lojas = lojas;
        this.transportadoras = transportadoras;
        this.voluntarios = voluntarios;
        this.utilizadores = utilizadores;
        this.encomendas = encomendas;
        this.produtos = new TreeSet<>();
    }

    /**
     * Get Map de Loja
     * @return Map Lojas
     */
    public Lojas getLojas() {
        return lojas;
    }


    /**
     * Set Map de Loja
     */
    public Modelo setLojas(Lojas lojas) {
        this.lojas = lojas;
        return this;
    }

    /**
     * Get Map de Transportadoras
     * @return Map Transportadoras
     */
    public Transportadoras getTransportadoras() {
        return transportadoras;
    }

    /**
     * Set Map de Transportadoras
     */
    public Modelo setTransportadoras(Transportadoras transportadoras) {
        this.transportadoras = transportadoras;
        return this;
    }

    /**
     * Get Map de Voluntarios
     * @return Map Voluntarios
     */
    public Voluntarios getVoluntarios() {
        return voluntarios;
    }

    /**
     * Set Map de Voluntarios
     */
    public Modelo setVoluntarios(Voluntarios voluntarios) {
        this.voluntarios = voluntarios;
        return this;
    }

    /**
     * Get Map de Utilizadores
     * @return Map de Utilizadores
     */
    public Utilizadores getUtilizadores() {
        return utilizadores;
    }

    /**
     * Set Map de Utilizadores
     */
    public Modelo setUtilizadores(Utilizadores utilizadores) {
        this.utilizadores = utilizadores;
        return this;
    }

    /**
     * Get Map de Encomendas
     * @return Map de Encomendas
     */
    public Map<String, Encomenda> getEncomendas() {
        return encomendas;
    }

    /**
     * Get Map de Encomendas
     */
    public Modelo setEncomendas(Map<String, Encomenda> encomendas) {
        this.encomendas = encomendas;
        return this;
    }

    /**
     * Get Map de Produtos
     * @return Set Produtos do modelo
     */
    public Set<Produto> getProdutos() {
        return produtos;
    }


    /**
     * Set Map de Produtos
     */
    public Modelo setProdutos(Set<Produto> produtos) {
        this.produtos = produtos;
        return this;
    }

    /**
     * Adiciona Loja ao Map de Loja
     * @param l             Loja a inserir
     */
    public void addLoja(Loja l){
        this.lojas.addLoja(l);
    }

    /**
     * Adiciona Utilizadores ao Map de Utilizadores
     * @param u                 Codigo Utilizador
     */
    public void addUtilizador(Utilizador u){ this.utilizadores.addUtilizador(u);}

    /**
     * Adiciona Voluntario ao Map de Voluntarios
     * @param v            Codigo Voluntario
     */
    public void addVoluntario(Voluntario v) { this.voluntarios.addVoluntario(v);}

    /**
     * Adiciona Transportadora ao Map de Transportadoras
     * @param t            Codigo Tranportadora
     */
    public void addTransportadora(Transportadora t) { this.transportadoras.addTransportadora(t);}

    /**
     * Get Encomenda do Map de Encomendas
     * @param e        Codigo Encomenda
     * @return Encomenda com determinado Codigo
     */
    public Encomenda getEncomenda(String e) { return this.encomendas.get(e);}

    /**
     * Verifica se existe Loja no Map de Lojas
     * @param c      Codigo da Loja
     * @return Se existe True/False
     */
    public boolean existeLoja(String c){ return this.lojas.existeLoja(c);}

    /**
     * Verifica se existe Transportadora no Map de Transportadora
     * @param c      Codigo da Transportadora
     * @return Se existe True/False
     */
    public boolean existeTransp(String c){ return this.transportadoras.existeTransp(c);}

        /**
     * Verifica se existe Utilizador no Map de Utilizador
     * @param c      Codigo da Utilizador
     */
    public boolean existeUser(String c){ return this.utilizadores.existeUser(c);}

        /**
     * Verifica se existe Voluntario no Map deVoluntarioa
     * @param c      Codigo da Voluntario
     * @return boolean true se existe / False se não
     */
    public boolean existeVol(String c){ return this.voluntarios.existeVol(c);}

    /**
     * Verifica o Login de um determinado ...
     * @param u      Codigo a confirmar
     * @param p      Codigo pass
     * @param param     inteiro respetivo a Lojas Utilizadores...
     * @return boolean se o login é válido
     */
    public boolean verificaLogin(String u ,String p, int param){
        boolean ret = false;
        switch (param){
            case 0:
                ret = lojas.verificaLogin(u,p);
                break;
            case 1:
                ret = transportadoras.verificaLogin(u,p);
                break;
            case 2:
                ret = voluntarios.verificaLogin(u,p);
                break;
            case 3:
                ret = utilizadores.verificaLogin(u,p);
                break;
            default:
                ret=false;
                break;
        }
        return ret;
    }

    /*
    public void addUtilizador(Model.Utilizador u){
        utilizadors.addUtilizador(u);
    }
    */

    /**
     * Verifica se existe Transportadora no Map de Transportadora
     * @param e      Codigo Encomenda
     * @param l      Codigo Loja
     * @param u      Codigo Utilizador
     * @param ps     Codigos de Produtos
     * @param qts    Quantidades respetivas dos produtos string
     */
    public void addEncomendaLoja( String e,String l,String u,String[] ps,int[] qts){
        lojas.addEncomenda(e,l,u,ps,qts);
    }

    /**
     * Parse de um file .txt
     * @param filename             Ficheiro a dar parse
     */
    public void parse(String filename) throws IOException{
        List<String> linhas = lerFicheiro(filename); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Model.Utilizador
                    utilizadores.addUtilizador(u);
                   // System.out.println(u.toString()); //enviar para o ecrÃ¡n apenas para teste
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    lojas.addLoja(l);
                  //  System.out.println(l.toString());
                    break;
                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1]);
                    transportadoras.addTransportadora(t);
                   // System.out.println(t.toString());
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    encomendas.put(e.getCodenc(), e);

                   // System.out.println(e.toString());
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    voluntarios.addVoluntario(v);
                   // System.out.println(v.toString());
                    break;
                case "Aceite":
                    String aux = parseAceite(linhaPartida[1]);
                    if (encomendas.containsKey(aux)) encomendas.get(aux).setAceites(true);
                    addAceiteParse(encomendas.get(aux));
                   // System.out.println("Aceites: " + aux);
                    break;
                default:
                    System.out.println("Linha invÃ¡lida.");
                    break;
            }
            for (Map.Entry<String,Encomenda> e : this.encomendas.entrySet()){
                //if (e.getValue().getAceites()){
                    lojas.addEncomendaParse(e.getValue().getCodloja(), e.getValue());
                    utilizadores.addEncomendaParse(e.getValue().getCoduser(), e.getValue());
                //}
            }
            for (Map.Entry<String,Loja> l: lojas.getLojas().entrySet()){
                lojas.addProdutosLoja(l.getKey(), (TreeSet<Produto>) produtos);
            }
        }
        System.out.println("done!");
        System.out.println("\n\n\n\n");
        //System.out.println(encomendas.toString());
    }

    /**
     * Metodo que trabalha as encomendas aceites no parse
     * @param e             Encomenda
     */
    public void addAceiteParse(Encomenda e){
        Map<String,List<String>> entregadores =  getPossiveisEntregadores(e);
        boolean f = false;
        if(entregadores.get("V").size()>0){
            Iterator<String> it = entregadores.get("V").iterator();
            while(it.hasNext() && !f){
                String coisa = it.next();
                if(getVoluntario(coisa).getLivre()){
                    op1Voluntario_1(coisa,e);
                    f=true;
                }
            }
        }
        if(entregadores.get("T").size()>0 && !f){
            aceite(e.getCoduser(),entregadores.get("T").get(0),e.getCodenc());
            f=true;
        }

        if(!f) encomendas.get(e.getCodenc()).setAceites(false);
    }

    /**
     * Divide o Utilizador
     * @param input             linha respetiva ao User
     * @return Utilizador lido do parse
     */
    public static Utilizador parseUtilizador(String input) {
        String[] campos = input.split(",");
        String nome = campos[1];
        String codUtilizador = campos[0];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Utilizador(codUtilizador, nome, new GPS(gpsx, gpsy), new ArrayList<Encomenda>());


    }

    /**
     * Divide o Loja
     * @param input             linha respetiva ao Loja
     * @return Loja lida do parse
     */
    public static Loja parseLoja(String input) {
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Loja(codLoja, nomeLoja, new GPS(gpsx, gpsy), new ArrayList<Encomenda>(), new ArrayList<Encomenda>());
    }

    /**
     * Divide o Transportadora
     * @param input             linha respetiva ao Transportadora
     * @return Transportadora lida do parse
     */
    public static Transportadora parseTransportadora(String input) {
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        int nif = Integer.parseInt(campos[4]);
        double rai = Double.parseDouble(campos[5]);
        double taxa = Double.parseDouble(campos[6]);
        return new Transportadora(cod, nome, new GPS(gpsx, gpsy), nif, rai, taxa, 0.1, 100, new ArrayList<Encomenda>(), false);
    }

    /**
     * Divide o Encomenda e acrescenta Produtos
     * @param input             linha respetiva ao Encomenda e
     * @return Encomenda lida do parse
     */
    public Encomenda parseEncomenda(String input) {
        String[] campos = input.split(",");
        String cod = campos[0];
        String codUser = campos[1];
        String codLoja = campos[2];
        Double peso = Double.parseDouble(campos[3]);
        String codL;
        String desc;
        double q;
        double preco;
        int i = 4;
        List<LinhaEncomenda> res = new ArrayList<>();
        while (i + 3 <= campos.length) {
            codL = campos[i];
            desc = campos[i + 1];
            q = Double.parseDouble(campos[i + 2]);
            preco = Double.parseDouble(campos[i + 3]);
            i += 4;
            LinhaEncomenda a = new LinhaEncomenda(codL, desc, q, 0, preco);
            res.add(a);
            produtos.add(new Produto(codL,desc,false,q,preco));
        }
        Encomenda enc =new Encomenda(cod, codUser, codLoja, peso, res);
        //lojas.addEncomendaParse(codLoja, enc);

        return enc;
    }

    /**
     * Divide o Voluntario
     * @param input             linha respetiva ao Voluntario
     * @return Voluntario lido do parse
     */
    public Voluntario parseVoluntario(String input) {
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(cod, nome, new GPS(gpsx, gpsy), raio, true, new ArrayList<>(), false);
    }

    /**
     * Divide as Aceites
     * @param input             linha respetiva  as Aceites
     * @return String Codigo encomenda
     */
    public String parseAceite(String input) {
        String[] campos = input.split(",");
        String cod = campos[0];
        //encomendas.get(cod).setAceites(true);
        return cod;


    }

    public void addEncomenda (Encomenda e){
        this.encomendas.put(e.getCodenc(),e);
        this.lojas.getLoja(e.getCodloja()).addEncomenda(e);
    }

    /**
     * Le ficheiro .txt
     * @param nomeFich             nome do ficheiro
     * @throws IOException
     * @return List de linhas em logs
     */
    public static List<String> lerFicheiro(String nomeFich) throws IOException {
        List<String> lines = new ArrayList<>();
        lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        return lines;
    }
    /**
     * Metodo que divide ds entregadores e acrescenta a sua lista de pedidos uma respetiva encomenda, ca estes cumpram os parametros do raio e etc
     * @param e             Encomenda para a qual os entregadores vão ser atribuidos
     */
    public void setPossiveisEntregadores(Encomenda e){

        Loja l = this.lojas.getLoja(e.getCodloja());
        Utilizador u = this.utilizadores.getUtilizador(e.getCoduser());
        GPS cordl = l.getGPS().clone();
        GPS cordu = u.getGPS().clone();
        if(!e.getMedica()) {
            for (Map.Entry<String, Transportadora> t : this.transportadoras.getTransportadoras().entrySet()) {
                if (t.getValue().getGPS().isNear(cordl, t.getValue().getRaio()) && t.getValue().getGPS().isNear(cordu, t.getValue().getRaio())) {
                    t.getValue().addPedido(e);
                }
            }
            for (Map.Entry<String, Voluntario> v : this.voluntarios.getVoluntarios().entrySet()) {
                if (v.getValue().getGPS().isNear(cordl, v.getValue().getRaio()) && v.getValue().getGPS().isNear(cordu, v.getValue().getRaio()) && v.getValue().getLivre()) {
                    v.getValue().addPedido(e);
                }
            }
        }
        else {
            for (Map.Entry<String, Transportadora> t : this.transportadoras.getTransportadoras().entrySet()) {
                if (t.getValue().getGPS().isNear(cordl, t.getValue().getRaio()) && t.getValue().getGPS().isNear(cordu, t.getValue().getRaio()) && t.getValue().aceitoTransporteMedicamentos()) {
                    t.getValue().addPedido(e);
                }
            }
            for (Map.Entry<String, Voluntario> v : this.voluntarios.getVoluntarios().entrySet()) {
                if (v.getValue().getGPS().isNear(cordl, v.getValue().getRaio()) && v.getValue().getGPS().isNear(cordu, v.getValue().getRaio()) && v.getValue().getLivre() && v.getValue().aceitoTransporteMedicamentos()) {
                    v.getValue().addPedido(e);
                }
            }
        }

    }
    /**
     * Metodo que divide ds entregadores e acrescenta a sua lista de pedidos uma respetiva encomenda, ca estes cumpram os parametros do raio e etc.
     * Devolve O map.entry criado
     * @param e             Encomenda para a qual os entregadores vão ser atribuidos
     * @return Map com T caso Transportadora e V se Voluntario retorna a lista de possiveis utilizadores
     */
    public Map<String,List<String>> getPossiveisEntregadores(Encomenda e){
        Map<String,List<String>> aux = new HashMap<>();
        Loja l = this.lojas.getLoja(e.getCodloja());
        Utilizador u = this.utilizadores.getUtilizador(e.getCoduser());
        GPS cordl = l.getGPS().clone();
        GPS cordu = u.getGPS().clone();
        aux.put("T",new ArrayList<>());
        aux.put("V", new ArrayList<>());
        if(!e.getMedica()) {
            for (Map.Entry<String, Transportadora> t : this.transportadoras.getTransportadoras().entrySet()) {
                if (t.getValue().getGPS().isNear(cordl, t.getValue().getRaio()) && t.getValue().getGPS().isNear(cordu, t.getValue().getRaio())) {
                    aux.get("T").add(t.getValue().getCod());
                }
            }
            for (Map.Entry<String, Voluntario> v : this.voluntarios.getVoluntarios().entrySet()) {
                if (v.getValue().getGPS().isNear(cordl, v.getValue().getRaio()) && v.getValue().getGPS().isNear(cordu, v.getValue().getRaio()) && v.getValue().getLivre()) {
                    aux.get("V").add(v.getValue().getCod());
                }
            }
        }
        else {
            for (Map.Entry<String, Transportadora> t : this.transportadoras.getTransportadoras().entrySet()) {
                if (t.getValue().getGPS().isNear(cordl, t.getValue().getRaio()) && t.getValue().getGPS().isNear(cordu, t.getValue().getRaio()) && t.getValue().aceitoTransporteMedicamentos()) {
                    aux.get("T").add(t.getValue().getCod());
                }
            }
            for (Map.Entry<String, Voluntario> v : this.voluntarios.getVoluntarios().entrySet()) {
                if (v.getValue().getGPS().isNear(cordl, v.getValue().getRaio()) && v.getValue().getGPS().isNear(cordu, v.getValue().getRaio()) && v.getValue().getLivre() && v.getValue().aceitoTransporteMedicamentos()) {
                    aux.get("V").add(v.getValue().getCod());
                }
            }
        }
        return aux;
    }
    /**
     * Depois de os possiveis entregadores atribuidos  rejeita caso tenha sido aceito por um voluntario ou aceite por um user
     *
     * @param map           Possiveis entregadores
     * @param codt          Codigo do entregador
     * @param e             Encomenda ao qual vao rejeitas
     */
    public void rejeitaOutraTransp(Map<String,List<String>> map, String codt, Encomenda e){
        for(String t : map.get("T")){
            if(!t.equals(codt)){
                this.transportadoras.getTransportadora(t).rmPedido(e);
                this.transportadoras.getTransportadora(t).getEspera().remove(e);
            }
        }
        for(String v : map.get("V")){
            if(!v.equals(codt)) this.voluntarios.getVoluntario(v).rmPedido(e);
        }
        this.lojas.getLoja(e.getCodloja()).rmEncPronta(e);
        e.setAceites(true);
        e.setTransp(codt);
    }
    /**
     * Get Voluntario  de Map Voluntarios
     * @param cod       Codigo Voluntario
     * @return Voluntario respetivo ao cod
     */
    public Voluntario getVoluntario(String cod){
        return this.getVoluntarios().getVoluntario(cod);
    }


    /**
     * Get Loja  de Map  Lojas
     * @param cod       Codigo Loja
     * @return Loja referente ao codigo
     */
    public Loja getLoja(String cod){
        return this.getLojas().getLoja(cod);
    }

    /**
     * Get Transportadora  de Map Transportadoras
     * @param cod       Codigo Transportadora
     * @return Transportadora referente ao codigo
     */
    public Transportadora getTransportadora(String cod){
        return this.getTransportadoras().getTransportadora(cod);
    }


    /**
     * Get Utilizador  de Map Utilizadors
     * @param cod       Codigo Utilizador
     * @return Utilizador referente ao codigo dado
     */
    public Utilizador getUtilizador(String cod){
        return this.getUtilizadores().getUtilizador(cod);
    }

    /**
     * Existe Produto no set Produtos
     * @param cod       Codigo Produto
     * @param pr        Set de Produtos
     * @return boolean para se existe ou não
     */
    public boolean existeProd(String cod, Set<Produto> pr){
        Iterator<Produto> it = pr.iterator();
        boolean r = false;
        while (it.hasNext() && !r){
            Produto aux = it.next();
            if (aux.getCod().equals(cod)){
                r = true;
            }
        }
        return r;
    }
    //////////////////////////////////////////// Funções básicas //////////////////////////////////////////////////////////////////
        /** Funções Basicas **/


    /**
     * Get set ordenado de users em relação ao numero de encomendas feitas
     * @return Ser com os produtos mais usados
     */
    public Set<Utilizador> maisUsados(){
        Set<Utilizador> mais= new TreeSet<>(new ComparatorUtilizador());
        this.utilizadores.maisUsados(mais);
        return mais;
        }

    /**
     * Get set ordenado de transportadoras em relação ao numero de kms feitos
     * @return Set com as transportadoras mais usadas ordenadas
     */
    public Set<Transportadora> maisUsadosT(){
        Set<Transportadora> mais= new TreeSet<>(new ComparatorTransp());
        for(Map.Entry<String,Transportadora> t: transportadoras.getTransportadoras().entrySet()){
            mais.add(t.getValue().clone());
        }
        return mais;
    }


    //////////////////////////////////////////// Interpretador Model.Loja ////////////////////////////////////////////////////////////////
        /** Interpretador Lojas **/

    /**
     * Adiciona uma encomenda a lista das encomendas Sinalizadas e procura possiveis entregadores
     * @param e             Codigo Encomenda
     * @param l             Codigo loja
     * @return int se contem ou nao contem
     */
    public int op1Loja(String e, String l){
        int r = 0;
        if(this.encomendas.containsKey(e)) {
            Encomenda enc = this.encomendas.get(e);
            if (this.lojas.getLoja(l).getListaEnc().contains(enc) && !enc.getAceites()){
                this.lojas.getLoja(l).addEncPronta(enc);
                setPossiveisEntregadores(enc);
                r=1;
            }
        }
        return r;
    }

    /**
     * Adiciona Porduto ao Set Produts de uma loja E ao Set Produtos geral
     * @param p                 Produto a inserir
     * @param c                 COdigo loja
     */
    public void op2Loja(Produto p, String c){
        lojas.getLoja(c).addProduto(p);
        produtos.add(p);
    }

    /**
     * Set novo nome para loja
     * @param nome          String novo nome
     * @param c             Codigo Loja
     */
    public void op7LojaNome(String nome, String c){
        lojas.getLoja(c).setNome(nome);
    }

    /**
     * Set novo pass para loja
     * @param pass          String novo pass
     * @param c             Codigo Loja
     */
    public void op7LojaPass (String pass, String c){
        lojas.getLoja(c).setPass(pass);
    }
    /**
     * Set novo nome para loja
     * @param x          Latitude
     * @param y          Longitude
     * @param c          Codigo Loja
     */
    public void op7LojaGPS(double x, double y, String c){
        lojas.getLoja(c).setGPS(x,y);
    }

    //////////////////////////////////////////// Interpretador Voluntario ////////////////////////////////////////////////////////////////

    /** Interpretador Voluntarios **/

        /**
     * Aceita uma encomenda num determinado voluntario e altera os dados referentes a essa encomenda
     * @param cod           Codigo Voluntario
     * @param e             Encomenda aceite
     */
    public void op1Voluntario_1(String cod, Encomenda e){
        LocalDateTime i = LocalDateTime.now();
        this.getVoluntario(cod).aceitaPedido(e,i);
        this.encomendas.get(e.getCodenc()).setAceites(true);
        this.encomendas.get(e.getCodenc()).setTransp(cod);
        this.encomendas.get(e.getCodenc()).setDatai(i);
        this.getUtilizador(e.getCoduser()).encAceite(e,cod,i);
        rejeitaOutraTransp(getPossiveisEntregadores(e), cod, e);
    }


    /**
     * Rejeita uma encomenda dos VOluntario
     * @param cod          Codigo Voluntario
     * @param e            Encomenda
     */
    public void op1Voluntario_2(String cod, Encomenda e){
        this.voluntarios.getVoluntario(cod).rejeitaPedido(e);
    }

    /**
     * Altera Nome VOluntario
     * @param nome          Novo Nome
     * @param c             Codigo Voluntario
     */
    public void op3VolNome(String nome, String c){
        voluntarios.getVoluntario(c).setNome(nome);
    }

    /**
     * Altera pass VOluntario
     * @param pass          Novo pass
     * @param c             Codigo Voluntario
     */
    public void op3VolPass (String pass, String c){
        voluntarios.getVoluntario(c).setPass(pass);
    }

    /**
     * Altera gps VOluntario
     * @param x             Latitude
     * @param y             Longitude
     * @param c             Codigo Voluntario
     */
    public void op3VolGPS(double x, double y, String c){
        voluntarios.getVoluntario(c).setGPS(x,y);
    }

    /**
     * Altera raio VOluntario
     * @param x             Novo raio
     * @param c             Codigo Voluntario
     */
    public void op3VolRaio(double x, String c){
        voluntarios.getVoluntario(c).setRaio(x);
    }


    /**
     * Voluntario entrega encomenda
     * @param e             Codigo Encomenda
     * @param cod           Codigo Voluntario
     * @return int refente se existe o codigo ou nao
     */
    public int op5Vol(String e, String cod){
        int r=0;
        LocalDateTime f = LocalDateTime.now();
        if(this.encomendas.containsKey(e)){
            Encomenda enc = this.encomendas.get(e);
            if(this.voluntarios.getVoluntario(cod).getList().contains(enc)){
                this.encomendas.get(e).setEntregue(true);
                this.encomendas.get(e).setDataf(f);
                this.getVoluntario(cod).encEntregue(encomendas.get(e),f);
                this.lojas.getLoja(enc.getCodloja()).setEntregue(enc,f);
                this.getUtilizador(encomendas.get(e).getCoduser()).encEntregue(encomendas.get(e),f);
                r=1;
            }
        }
        return r;
    }

    //////////////////////////////////////////// Interpretador Transportadora ////////////////////////////////////////////////////////////////

    /**Interpretador Transportadora**/


    /**
     * Transportadora Aceita Encomenda
     * @param cod           Codigo Transportadora
     * @param e             Encomenda
     */
    public void op1Transp_1(String cod, Encomenda e){
        this.getTransportadora(cod).aceitaPedido(e);
        this.encomendas.get(e.getCodenc()).setAceites(true);
        this.getUtilizador(e.getCoduser()).addPedidos(e.getCodenc(),cod);
    }

    /**
     * Transportadora rejeita Encomenda
     * @param cod           Codigo Transportadora
     * @param e             Encomenda
     */
    public void opTransp_2(String cod, Encomenda e){ this.getTransportadora(cod).rejeitaPedido(e);}

    /**
     * Altera Nome Transportadora
     * @param nome          Novo Nome
     * @param c             Codigo Transportadora
     */
    public void op3TranspNome(String nome, String c){
        transportadoras.getTransportadora(c).setNome(nome);
    }


    /**
     * Altera pass Transportadora
     * @param pass          Novo pass
     * @param c             Codigo Transportadora
     */
    public void op3TranspPass (String pass, String c){
        transportadoras.getTransportadora(c).setPass(pass);
    }

    /**
     * Altera gps Transportadora
     * @param x             Latitude
     * @param y             Longitude
     * @param c             Codigo Transportadora
     */
    public void op3TranspGPS(double x, double y, String c){
        transportadoras.getTransportadora(c).setGPS(x,y);
    }

    /**
     * Altera raio Transportadora
     * @param x          Novo raio
     * @param c             Codigo Transportadora
     */
    public void op3TranspRaio(double x, String c){
        transportadoras.getTransportadora(c).setRaio(x);
    }

    /**
     * Altera taxa Transportadora
     * @param x             Nova taxa
     * @param c             Codigo Transportadora
     */
    public void op3TranspTaxa(double x, String c){
        transportadoras.getTransportadora(c).setTaxa(x);
    }

    /**
     * Altera taxa Transportadora
     * @param x             Novo raio
     * @param c             Codigo Transportadora
     */
    public void op3TranspTaxap(double x, String c){
        transportadoras.getTransportadora(c).setTaxaPeso(x);
    }

    /**
     * Transportadora entrega encomenda
     * @param e             Codigo Encomenda
     * @param cod           Codigo Transportadora
     */
    public int op5Transp(String e, String cod){
        LocalDateTime f = LocalDateTime.now();
        int r=0;
        if(this.encomendas.containsKey(e)){
            Encomenda enc = this.encomendas.get(e);
            if(this.getTransportadora(cod).getList().contains(enc)){
                this.encomendas.get(e).setEntregue(true);
                this.encomendas.get(e).setDataf(f);
                getTransportadora(cod).encEntregue(encomendas.get(e),f);
                this.lojas.getLoja(enc.getCodloja()).setEntregue(enc,f);
                getTransportadora(cod).addKms(getDT(getTransportadora(cod), encomendas.get(e)));
                getUtilizador(encomendas.get(e).getCoduser()).encEntregue(encomendas.get(e),f);
                r=1;
            }
        }
        return r;
    }
    /**
     * Distancia transportadora Loja Utilizador
     * @param t             Transportadora
     * @param e             Encomenda
     * @return Distancia entre Transportadora loja Utilizador
     */
    public double getDT(Transportadora t, Encomenda e){
        double d = 0;
        if(e.getEntregue()){
            d = t.getGPS().distanciaXY(getLoja(e.getCodloja()).getGPS()) + getLoja(e.getCodloja()).getGPS().distanciaXY(getUtilizador(e.getCoduser()).getGPS());
        }
        return d;
    }


    /**
     * Total Faturado entre duas datas
     * @param d1            Primeira data
     * @param d2            Segunda data
     * @param t             Transportadora
     * @return Total faturado por Trasnportadora no intervalo d1 d2
     */
    public double faturado(LocalDateTime d1, LocalDateTime d2, Transportadora t){
        double fat = 0;
        if(t.getList().size() <=0) return 0;
        else {
            for (Encomenda e : t.getList()) {
                if (e.getEntregue() && e.getDataf().compareTo(d1) <= 0 && e.getDataf().compareTo(d2) >= 0) {
                    fat += t.getPreço(getLoja(e.getCodloja()).getGPS(), getUtilizador(e.getCoduser()).getGPS(), e);
                }
            }
        }
        return fat;

    }

    //////////////////////////////////////////// Interpretador Utilizador ////////////////////////////////////////////////////////////////

    /**
    * Utilizador faz encoenda
     * @param e             Encomenda
     */
    public void op1User_3(Encomenda e){
        this.encomendas.put(e.getCodenc(),e.clone());
        getUtilizador(e.getCoduser()).addEncomenda(e);
        this.getLoja(e.getCodloja()).addEncomenda(e.clone());
    }

    /**
     * Verifica se uma certa encomenda é medica
     * @param l             List<LinhaEncomenda></LinhaEncomenda>
     * @return boolean se sim ou não
     */
    public boolean isMedica(List<LinhaEncomenda> l){
        Iterator<LinhaEncomenda> it = l.iterator();
        boolean f = false;
        while(it.hasNext() && !f){
            LinhaEncomenda linha = it.next();
            if(getProduto(linha.getCod()).getMedico())
                f=true;
        }
        return f;
    }

    /**
     * Calcula o preço para um determinado transporte
     * @param e             Codigo Encomenda
     * @param t             Codigo entregador
     * @return Preço da Transportadora
     */
    public double getPrecoTransp(String e, String t){
        String cl = encomendas.get(e).getCodloja();
        GPS l = lojas.getLoja(cl).getGPS().clone();
        String user = encomendas.get(e).getCoduser();
        GPS us = utilizadores.getUtilizador(user).getGPS().clone();
        return this.getTransportadora(t).getPreço(l,us,encomendas.get(e).clone());
    }
    /**
     * Calcula o tempo para um determinado transporte
     * @param e             Codigo Encomenda
     * @param t             Codigo entregador
     * @return Tempo estimado consoante se existe fila se nao e para as varias estaçoes do ano
     */
    public double[] getTempoEstimado(String e, String t){
        String cl = encomendas.get(e).getCodloja();
        GPS l = lojas.getLoja(cl).getGPS().clone();
        String user = encomendas.get(e).getCoduser();
        GPS us = utilizadores.getUtilizador(user).getGPS().clone();
        Month mes = getEncomenda(e).getDatai().getMonth();
        int fila = lojas.getLoja(cl).getFila();

        boolean verao = mes == Month.JUNE || mes == Month.JULY || mes == Month.AUGUST || mes == Month.SEPTEMBER;
        boolean inverno = mes == Month.DECEMBER || mes == Month.JANUARY || mes == Month.FEBRUARY || mes == Month.APRIL;

        double horas;

        if (fila == -1) {
            if (verao) {
                horas = this.getTransportadora(t).getTempoV(l, us);
            } else if (inverno) {
                horas = this.getTransportadora(t).getTempoI(l, us);
            } else horas = this.getTransportadora(t).getTempoPO(l, us);
            return getTime(horas);
        }
        else{
            if (verao) {
                horas = this.getTransportadora(t).getTempoVF(l, us, fila);
            } else if (inverno) {
                horas = this.getTransportadora(t).getTempoIF(l, us, fila);
            } else horas = this.getTransportadora(t).getTempoPOF(l, us, fila);
            return getTime(horas);
        }
    }

    /**
     * Encomenda aceite pelo utilizador e transportador
     * @param user          Codigo Utlizador
     * @param t             Codigo Transportadora
     * @param e             Codigo Encomenda
     */
    public void aceite(String user, String t, String e){
        LocalDateTime i = LocalDateTime.now();
        encomendas.get(e).setAceites(true);
        encomendas.get(e).setTransp(t);
        encomendas.get(e).setDatai(i);
        this.getUtilizador(user).getPedidos().remove(e);
        getUtilizador(user).encAceite(encomendas.get(e),t,i);
        rejeitaOutraTransp(getPossiveisEntregadores(encomendas.get(e)), t, encomendas.get(e));
        this.getTransportadora(t).addEncT(encomendas.get(e));
    }

    /**
     * Altera Nome Utilizador
     * @param nome          Novo Nome
     * @param c             Codigo Utilizador
     */
    public void opUNome(String nome, String c){
        utilizadores.getUtilizador(c).setNome(nome);
    }

    /**
     * Altera pass Utilizador
     * @param pass          Novo pass
     * @param c             Codigo Utilizador
     */
    public void opUPass (String pass, String c){
        utilizadores.getUtilizador(c).setPass(pass);
    }


    /**
     * Altera gps Utilizador
     * @param x             Latitude
     * @param y             Longitude
     * @param c             Codigo  Utilizador
     */
    public void opUGPS(double x, double y, String c){
        utilizadores.getUtilizador(c).setGPS(x,y);
    }

    /**
     * Atribui Classificação a Transportadora
     * @param e              Codigo Encomenda
     * @param cl             Classificação imposta
     */
    public void op4(String e, int cl) {
        encomendas.get(e).setClassificacao(cl);
        getUtilizador(encomendas.get(e).getCoduser()).setCl(encomendas.get(e), cl);
        char c = encomendas.get(e).getTransp().charAt(0);
        if (c == 'v') {
            getVoluntario(encomendas.get(e).getTransp()).setCl(encomendas.get(e), cl);
        } else {
            getTransportadora(encomendas.get(e).getTransp()).setCl(encomendas.get(e), cl);
        }
    }
    /**
     * Passa o tempo de horas para xDIAS yHORAS zMin
     * @param horas             Tempo em horas
     * @return double[] tempo em horas
     */
    public double[] getTime(double horas){
        double[] a = new double[3];
        double dias = horas/24;
        double rest = dias %1;
        horas = rest *24;
        a[0] = dias - rest;
        double restH = horas %1;
        horas = horas - restH;
        double min = restH * 60 - ((restH*60)%1);
        a[1] = horas;
        a[2] = min;
        return a;
    }

    /**
     * Encontra Produto
     * @param cod             Codigo  Produto
     * @return Produto obtido pelo codigo
     */
    public Produto getProduto(String cod) {
        Iterator<Produto> it = produtos.iterator();
        boolean f = false;
        Produto pr = new Produto();
        while (it.hasNext() && !f) {
            pr = it.next();
            if (pr.getCod().equals(cod))
                f=true;
        }
        return pr;
    }




        // Gravar para ficheiro
    /**
     * Grava Programa em Obj .dat
     */
    public void gravarObj() throws IOException {
        ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream("Dados.dat"));
        o.writeObject(this);
        o.flush();
        o.close();
    }


    //Ler de ficheiros bin
    /**
     * le .dat para sistema
     */
    public static Modelo lerObj() throws IOException, ClassNotFoundException {
        ObjectInputStream o = new ObjectInputStream(new FileInputStream("Dados.dat"));
        Modelo c = (Modelo) o.readObject();
        o.close();
        return c;
    }
}


