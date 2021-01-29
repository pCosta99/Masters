package Models;

import javax.swing.tree.TreeNode;
import java.io.*;
import java.time.LocalDateTime;
import java.util.*;


public class Sistema implements Serializable {

    private List<Utilizador> utilizadores;
    private List<Transportadora> empresas;
    private List<Voluntario> voluntarios;
    private List<Loja> lojas;
    private List<Encomenda> historicoEncomendas;
    private List<Encomenda> encomendasPorEnviar;
    private List<AceitaEncomenda> encomendasAceites;


    /**
     * Construtor por omissão.
     */
    public Sistema() {
        this.utilizadores = new ArrayList<>();
        this.empresas = new ArrayList<>();
        this.voluntarios = new ArrayList<>();
        this.lojas = new ArrayList<>();
        this.historicoEncomendas = new ArrayList<>();
        this.encomendasPorEnviar = new ArrayList<>();
        this.encomendasAceites = new ArrayList<>();

    }

    /**
     * Construtor parametrizado.
     */
    public Sistema(List<Utilizador> utilizadores, List<Transportadora> empresas, List<Voluntario> voluntarios, List<Loja> loja,
                   List<Encomenda> historicoEncomendas, List<Encomenda> encomendasPorEnviar, List<AceitaEncomenda> encomendasAceites) {

        this.utilizadores = new ArrayList<>();
        for(Utilizador u : utilizadores) this.utilizadores.add(u.clone());
        this.empresas = new ArrayList<>();
        for(Transportadora t : empresas) this.empresas.add(t.clone());
        this.voluntarios = new ArrayList<>();
        for(Voluntario v : voluntarios) this.voluntarios.add(v.clone());
        this.lojas = new ArrayList<>();
        for(Loja l : loja) this.lojas.add(l.clone());
        this.historicoEncomendas = new ArrayList<>();
        for(Encomenda e : historicoEncomendas) this.historicoEncomendas.add(e.clone());
        this.encomendasPorEnviar = new ArrayList<>();
        for(Encomenda en : encomendasPorEnviar) this.encomendasPorEnviar.add(en.clone());
        this.encomendasAceites = new ArrayList<>();
        for(AceitaEncomenda enc : encomendasAceites) this.encomendasAceites.add(enc.clone());

    }

    /**
     * Construtor por cópia.
     * @param - Sistema a copiar.
     */
    public Sistema(Sistema s) {
        this.utilizadores = s.getUtilizadores();
        this.empresas = s.getEmpresas();
        this.voluntarios = s.getVoluntarios();
        this.lojas = s.getLojas();
        this.historicoEncomendas = s.getHistorico();
        this.encomendasPorEnviar = s.getPorEnviar();
        this.encomendasAceites = s.getAceites();
    }

    /**
     * Método que dá a lista do utilizadores.
     * @return - Lista de utilizadores do sistema.
     */
    public List<Utilizador> getUtilizadores(){
        List<Utilizador> ret = new ArrayList<>();
        for (Utilizador u : this.utilizadores) ret.add(u.clone());
        return ret;
    }

    /**
     * Método que dá a lista de empresas transportadoras do sistema.
     * @return - Lista de empresas transportadoras do sistema.
     */
    public List<Transportadora> getEmpresas(){
        List<Transportadora> ret = new ArrayList<>();
        for(Transportadora t : this.empresas) ret.add(t.clone());
        return ret;
    }

    /**
     * Método que dá a lista de voluntários do sistema.
     * @return - Lista de voluntários do sistema.
     */
    public List<Voluntario> getVoluntarios(){
        List<Voluntario> ret = new ArrayList<>();
        for(Voluntario t : this.voluntarios) ret.add(t.clone());
        return ret;
    }

    /**
     * Método que dá a lista de lojas do sistema.
     * @return - Lojas do sistema.
     */
    public List<Loja> getLojas() {
        List<Loja> ret = new ArrayList<>();
        for (Loja l : this.lojas) ret.add(l.clone());
        return ret;
    }

    /**
     * Método que dá a lista do histórico de encomendas.
     * @return - Lista do histórico de encomendas.
     */
    public List<Encomenda> getHistorico() {
        List<Encomenda> ret = new ArrayList<>();
        for( Encomenda e : this.historicoEncomendas) ret.add(e.clone());
        return ret;
    }

    /**
     * Método que dá a lista de encomendas por enviar.
     * @return - Lista de encomendas por enviar.
     */
    public List<Encomenda> getPorEnviar() {
        List<Encomenda> ret = new ArrayList<>();
        for( Encomenda e : this.encomendasPorEnviar) ret.add(e.clone());
        return ret;
    }

    /**
     * Método que dá a lista de encomendas aceites, ou seja, a serem processadas.
     * @return - Lista de encomendas aceites.
     */
    public List<AceitaEncomenda> getAceites() {
        List<AceitaEncomenda> ret = new ArrayList<>();
        for( AceitaEncomenda a : this.encomendasAceites) ret.add(a.clone());
        return ret;
    }

    /**
     * Função que traduz a classe Sistema.
     * @return Devolve uma String com a respetiva tradução.
     */
    @Override
    public String toString() {
        return "Sistema{" +
                "utilizadores=" + utilizadores +
                ", empresas=" + empresas +
                ", voluntarios=" + voluntarios +
                ", lojas=" + lojas +
                ", historicoEncomendas=" + historicoEncomendas +
                ", encomendasPorEnviar=" + encomendasPorEnviar +
                ", encomendasAceites=" + encomendasAceites +
                '}';
    }

    /**
     * Função que verifica se um objeto recebido é idêntico ao da classe Sistema.
     * @param o Recebe um objeto.
     * @return Devolve um boolean com a respetiva verificação.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Sistema sistema = (Sistema) o;
        return Objects.equals(utilizadores, sistema.utilizadores) &&
                Objects.equals(empresas, sistema.empresas) &&
                Objects.equals(voluntarios, sistema.voluntarios) &&
                Objects.equals(lojas, sistema.lojas) &&
                Objects.equals(historicoEncomendas, sistema.historicoEncomendas) &&
                Objects.equals(encomendasPorEnviar, sistema.encomendasPorEnviar) &&
                Objects.equals(encomendasAceites, sistema.encomendasAceites);
    }


    /**
     * Função que faz um clone da classe Sistema.
     * @return Devolve esse clone.
     */
    @Override
    public Sistema clone() {
        return new Sistema(this);
    }

    /**
     * Função que classifica um Voluntário.
     * @param cod Recebe uma String que representa o código de um Voluntário.
     * @param classificacao Recebe um Inteiro representante da classificação.
     */
    public void classificarTransportadora(String cod, int classificacao){
        if(cod.charAt(0) == 'v') {
            for(Voluntario v : this.voluntarios){
                if(v.getId() == cod) v.addClassificacao(classificacao);
            }
        }
        if(cod.charAt(0) == 't'){
            for(Transportadora t : this.empresas){
                if(t.getId() == cod) t.addClassificacao(classificacao);
            }
        }

    }



    /**
     * Função que dá o username (id) da entidade a ser criada.
     * @param s - String identificadora.
     * @return - Id (username) da entidade.
     */
    public String getNewId(String s){
        StringBuilder sb = new StringBuilder();
        if(s.equals("u")) sb.append(s).append(this.utilizadores.size());
        else if (s.equals("l")) sb.append(s).append(this.lojas.size());
        else if (s.equals("e")) sb.append(s).append(this.historicoEncomendas.size() + this.encomendasPorEnviar.size()+ this.encomendasAceites.size());
        else if (s.equals("t")) sb.append(s).append(this.empresas.size());
        else if(s.equals("v")) sb.append(s).append(this.voluntarios.size());
        return sb.toString();
    }

    /**
     * Função que regista um utilizador no sistema.
     * @param id - Username do utilizador.
     * @param nome - Nome do utilizador.
     * @param email - Email do utilizador.
     * @param password - Password do utilizador.
     * @param x - Latitude do utilizador.
     * @param y - Longitude do utilizador.
     * @return - Utilizador criado.
     */
    public Utilizador registaUtilizador(String id, String nome, String email, String password, double x, double y){
        List<Encomenda> vazia = new ArrayList<>();
        GPS gps = new GPS(x,y);
        Utilizador user = new Utilizador(id,nome,gps,vazia,email,password);
        this.utilizadores.add(user);
        return user;
    }

    /**
     * Função que regista um utilizador no sistema.
     * @param id - Username do utilizador.
     * @param nome - Nome do utilizador.
     * @param x - Latitude do utilizador.
     * @param y - Longitude do utilizador.
     * @return - Utilizador criado.
     */
    public Utilizador registaUtilizador(String id, String nome, double x, double y){
        StringBuilder sb1 = new StringBuilder();
        sb1.append(id).append("@gmail.com");
        Utilizador user = new Utilizador(registaUtilizador(id,nome,sb1.toString(),id,x,y));
        return user;
    }

    /**
     * Função que regista uma loja no sistema com informação sobre a fila de espera.
     * @param id - Username da loja.
     * @param nome - Nome da loja.
     * @param x - Latitude da loja.
     * @param y - Longitude da loja.
     * @param email - Email da loja.
     * @param password - Password da loja.
     * @param atendimento - Tempo de atendimento da loja.
     * @param fila - Número de pessoas na fila da loja.
     */
    public Loja registaLoja(String id, String nome, String email, String password, double x, double y, int atendimento, int fila){
        GPS gps = new GPS(x,y);
        Loja l = new Loja(id,nome,gps,email,password,atendimento,fila);
        this.lojas.add(l);
        return l;
    }

    /**
     * Função que regista uma loja no sistema sem informação sobre a fila de espera.
     * @param id - Username da loja.
     * @param nome - Nome da loja.
     * @param x - Latitude da loja.
     * @param y - Longitude da loja.
     * @param email - Email da loja.
     * @param password - Password da loja.
     * @param atendimento - Tempo de atendimento da loja.
     */
    public Loja registaLoja(String id, String nome, String email, String password, double x, double y, int atendimento){
        GPS gps = new GPS(x,y);
        Loja l = new Loja(id,nome,gps,email,password,atendimento,0);
        this.lojas.add(l);
        return l.clone();
    }
    public Loja registaLoja(String id, String nome, double x, double y){
        StringBuilder sb1 = new StringBuilder();
        sb1.append(id).append("@gmail.com");
        Loja loja = new Loja(registaLoja(id,nome,sb1.toString(),id,x,y,3));
        return loja;
    }

    /**
     * Função que regista uma empresa transportadora no sistema.
     * @param id - Username da empresa.
     * @param nome - Nome da empresa.
     * @param email - Email da empresa.
     * @param password - Password da empresa.
     * @param x - Latitude da empresa.
     * @param y - Longitude da empresa.
     * @param nif - Nif da empresa.
     * @param raio - Raio de ação da empresa.
     * @param taxa - Taxa de cobrança por km da empresa.
     * @param numEnc - Número de encomendas que a empresa transporta de uma só vez.
     * @param velMedia - Velocidade média a que a empresa circula.
     * @param med - Boolean que indica se tem certificado médico ou não.
     */
    public Transportadora registaTransportadora(String id, String nome, String email, String password, double x,
                                                double y, int nif, double raio, double taxa, int numEnc, double velMedia,
                                                boolean med){
        GPS gps = new GPS(x,y);
        List<Integer> classif = new ArrayList<>();
        List<Encomenda> hist = new ArrayList<>();
        List<Encomenda> transp = new ArrayList<>();
        boolean bol = false;
        if(med){ bol = true;}
        Transportadora t = new Transportadora(id,nome,email,password,gps,nif,raio,true,taxa,numEnc,classif,hist,0,velMedia, LocalDateTime.now(),transp,med,bol);
        this.empresas.add(t);
        return t.clone();
    }

    public Transportadora registaTransportadora(String id, String nome, double x, double y, int nif, double raio, double precoKm){
        StringBuilder sb1 = new StringBuilder();
        sb1.append(id).append("@gmail.com");
        Transportadora trans = new Transportadora(registaTransportadora(id,nome,sb1.toString(),id,x,y,nif,raio,precoKm,1, 40,false));
        return trans;
    }

    /**
     * Função que regista um voluntário no sistema.
     * @param id - Id do voluntario.
     * @param nome - Nome da voluntario.
     * @param email - Email da voluntario.
     * @param password - Password da voluntario.
     * @param x - Latitude da voluntario.
     * @param y - Longitude da voluntario.
     * @param raio - Raio de ação da voluntario.
     * @param velocidadeMedia - Velocidade média a que a voluntario circula.
     * @param medica - Boolean que indica se tem certificado médico ou não.
     * @return
     */
    public Voluntario registaVoluntario(String id, String nome, String email, String password, double x,
                                        double y, double raio, double velocidadeMedia, boolean medica){
        boolean bol = false;
        if(medica) bol = true;
        GPS gps = new GPS(x,y);
        List<Integer> classif = new ArrayList<>();
        List<Encomenda> hist = new ArrayList<>();
        Encomenda enc = new Encomenda();
        Voluntario v = new Voluntario(id,nome,email,password,gps,raio,true,classif,hist,enc,velocidadeMedia,medica,bol);
        this.voluntarios.add(v);
        return v;

    }

    public Voluntario registaVoluntario(String id, String nome, double x, double y, double raio){
        StringBuilder sb1 = new StringBuilder();
        sb1.append(id).append("@gmail.com");
        Voluntario vol = new Voluntario(registaVoluntario(id,nome,sb1.toString(),id,x,y,raio,40,false));
        return vol;
    }

    /**
     * Função que dá uma lista de voluntários livres do sistema.
     * @return - Lista de voluntários livres.
     */
    public List<Voluntario> getVoluntariosLivres(){
        List<Voluntario> ret = new ArrayList<>();
        for(Voluntario v : this.voluntarios){
            if(v.isLivre()) ret.add(v.clone());
        }
        if(ret.size() < 1) ret = null;
        return ret;
    }

    /**
     * Função que dá uma loja do sistema através do seu id (username).
     * @param id - Id da loja.
     * @return - Loja do sistema.
     */
    public Loja getLoja(String id){
        Loja ret = null;
        for(Loja l : this.lojas){
            if(l.getId().equals(id)) ret = new Loja(l.clone());
        }
        return ret;
    }


    /**
     * Função que dá um utilizador do sistema através do seu id (username).
     * @param id - Id do utilizador.
     * @return - Utilizador do sistema.
     */
    public Utilizador getUtilizador(String id){
        Utilizador ret = null;
        for(Utilizador u : this.utilizadores){
            if(u.getId().equals(id)) ret = new Utilizador(u.clone());
        }
        return ret;
    }


    /**
     * Função que faz login de um utilizador.
     * @param email - Email do utilizador.
     * @param password - Password do utilizador.
     * @return - O utilizador que corresponde a essas credencias.
     */
    public Utilizador loginU(String email, String password){
        Utilizador ut = null;
        for(Utilizador u : this.utilizadores){
            if(u.getEmail().equals(email) && u.getPassword().equals(password)) {
                ut = new Utilizador(u.clone());
                break;
            }
        }
        return ut;
    }

    /**
     * Função que faz login de uma loja.
     * @param email - Email da loja.
     * @param password - Password da loja.
     * @return - A loja que corresponde a essas credencias.
     */
    public Loja loginL(String email, String password){
        Loja lj = null;
        for(Loja l : this.lojas){
            if(l.getEmail().equals(email) && l.getPassword().equals(password)){
                lj = new Loja(l.clone());
                break;
            }
        }
        return lj;
    }

    /**
     * Função que faz login de uma empresa transportadora.
     * @param email - Email da empresa.
     * @param password - Password da empresa.
     * @return - A empresa que corresponde a essas credencias.
     */
    public Transportadora loginE(String email, String password){
        Transportadora tp = null;
        for(Transportadora t : this.empresas){
            if(t.getEmail().equals(email) && t.getPassword().equals(password)) tp = new Transportadora(t.clone());
        }
        return tp;
    }

    /**
     * Função que faz login de um voluntário.
     * @param email - Email do voluntário.
     * @param password - Password do voluntário.
     * @return - O voluntário que corresponde a essas credencias.
     */
    public Voluntario loginV(String email, String password){
            Voluntario vl = null;
            for (Voluntario v : this.voluntarios) {
                if (v.getEmail().equals(email) && v.getPassword().equals(password)) {
                    vl = new Voluntario(v.clone());
                }

            }
            return vl;
    }

    /**
     * Função que determina o top 10 de utilizadores com mais encomendas.
     * @return - Lista com os nomes dos 10 utilizadores.
     */
    public List<String> top10Utilizador(){
        List<String> ret = new ArrayList<>();
        this.utilizadores.sort(new Comparator<Utilizador>() {
            @Override
            public int compare(Utilizador utilizador, Utilizador t1) {
                return t1.getEncomendas().size() - utilizador.getEncomendas().size();
            }
        });
        for(int i = 0; i < 10; i++){
            ret.add(this.utilizadores.get(i).getNome());
        }
        return ret;

    }

    /**
     * Função que determina o top 10 de empresas transportadoras com mais quilómetros percorridos.
     * @return - Lista com os nomes das 10 empresas.
     */
    public List<String> top10Empresas(){
        List<String> ret = new ArrayList<>();
        this.empresas.sort(new Comparator<Transportadora>() {
            @Override
            public int compare(Transportadora transportadora, Transportadora t1) {
                return (int) (t1.getKmPercorridos() - transportadora.getKmPercorridos());
            }
        });
        for(int i = 0; i < 10; i++){
            ret.add(this.empresas.get(i).getNome());
        }
        return ret;
    }

    /**
     * Função que cria uma encomenda para mais tarde ser adiciona ao sistema.
     * @param util - Utilizador que quer fazer encomenda.
     * @param loja - Loja da encomenda.
     * @param peso - Peso da encomenda.
     * @param med - Boolean que diz se é uma encomenda médica ou não.
     * @return - Encomenda criada.
     */
    public Encomenda fazerEncomenda(String util, String loja,double peso, boolean med){
        Encomenda ret = new Encomenda();
        ret.setId(getNewId("e"));
        ret.setUser(util);
        ret.setLoja(loja);
        ret.setPeso(peso);
        ret.setEncomendaMedica(med);
        return ret.clone();
    }

    /**
     * Função que cria uma encomenda para mais tarde ser adiciona ao sistema.
     * @param id - Id de encomenda.
     * @param util - Utilizador que quer fazer encomenda.
     * @param loja - Loja da encomenda.
     * @param peso - Peso da encomenda.
     * @param med - Boolean que diz se é uma encomenda médica ou não.
     * @return - Encomenda criada.
     */
    public Encomenda fazerEncomenda2(String id,String util, String loja,double peso, boolean med){
        Encomenda ret = new Encomenda();
        ret.setId(id);
        ret.setUser(util);
        ret.setLoja(loja);
        ret.setPeso(peso);
        ret.setEncomendaMedica(med);
        return ret.clone();
    }

    /**
     * Função que adiciona uma encomenda ao sistema, torando a numa encomenda por enviar.
     * @param e - Encomenda a adicionar.
     */
    public void finalizarEncomenda(Encomenda e){
        this.encomendasPorEnviar.add(e);
    }

    /**
     * Função que retorna a lista de voluntários livres e que têm certificado médico.
     * @return - Lista de voluntários livres e que têm certificado médico.
     */
    public List<Voluntario> getVoluntariosLivresMed(){
        List<Voluntario> ret = new ArrayList<>();
        for(Voluntario v : this.voluntarios){
            if(v.getLivreMed() && v.aceitoTransporteMedicamentos() && v.isLivre()) ret.add(v.clone());
        }
        if(ret.size() < 1) ret = null;
        return ret;
    }

    /**
     * Função que retorna a lista de empresas transportadoras livres.
     * @return - Lista de empresas transportadoras livres.
     */
    public List<Transportadora> getTransportadorasLivres(){
        List<Transportadora> ret = new ArrayList<>();
        for(Transportadora t : this.empresas){
            if(t.isLivre()) ret.add(t.clone());
        }
        if(ret.size() < 1) ret = null;
        return ret;
    }

    /**
     * Função que retorna a lista de empresas transportadoras livres e que têm certificado médico.
     * @return - Lista de empresas transportadoras livres e que têm certificado médico.
     */
    public List<Transportadora> getTransportadorasLivresMed(){
        List<Transportadora> ret = new ArrayList<>();
        for(Transportadora t : this.empresas){
            if(t.getLivreMed() && t.aceitoTransporteMedicamentos() && t.isLivre()) ret.add(t.clone());
        }
        if(ret.size() < 1) ret = null;
        return ret;
    }

    /**
     * Função que retorna o índice de uma encomenda a partir do seu id.
     * @param id - Id da encomenda.
     * @return - Índice da encomenda.
     */
    public int getIndiceEncomenda(String id){
        int ret = 0;
        for(Encomenda e : this.encomendasPorEnviar){
            if(e.getId().equals(id)) break;
            else ret++;
        }
        return ret;
    }

    /**
     * Função que retorna a encomenda por enviar através do seu id.
     * @param id - Id da encomenda.
     * @return - Encomenda por enviar.
     */
    public Encomenda getEncomendaPorEnviar(String id){
        Encomenda e = null;
        for(Encomenda enc : this.encomendasPorEnviar){
            if(enc.getId().equals(id)){
                e = new Encomenda(enc.clone());
                break;
            }
        }
        return e;
    }

    /**
     * Função que distribui uma encomenda a enviar a um voluntário.
     * @param idE - Id da encomenda.
     * @return - Voluntário que entrega a encomenda.
     */
    public Voluntario aceitaEncomendaV(String idE){
        Encomenda e = getEncomendaPorEnviar(idE);
        GPS gpsLoja = getLoja(e.getLoja()).getGps();
        GPS gpsUt = getUtilizador(e.getUser()).getGps();
        int info = getLoja(e.getLoja()).tempoDoPedido();
        int i = getIndiceEncomenda(idE);
        int ret = 0;
        Voluntario fn = null;
        if(e.getEncomendaMedica()) {
            if (getVoluntariosLivresMed() != null) {
                for (Voluntario v : getVoluntariosLivresMed()) {
                    if (v.dentroDoRaio(gpsLoja, gpsUt)) {
                        if (info > v.tempoDeIda(gpsLoja)) ret = info + v.tempoDeVolta(gpsLoja, gpsUt);
                        else ret = v.tempoDeIda(gpsLoja) + v.tempoDeVolta(gpsLoja, gpsUt);
                        e.setPrecoEntrega(0);
                        e.setTempoDeEspera(ret);
                        v.aceitaEncomenda(e.clone());
                        fn = new Voluntario(v.clone());
                        this.encomendasAceites.add(new AceitaEncomenda(idE));
                        this.encomendasPorEnviar.remove(i);
                        break;
                    }
                }
            }
        }
        else{
            if(getVoluntariosLivres() != null){
                for(Voluntario vt : getVoluntariosLivres()){
                    if(vt.dentroDoRaio(gpsLoja,gpsUt)){
                        if(info > vt.tempoDeIda(gpsLoja)) ret = info + vt.tempoDeVolta(gpsLoja,gpsUt);
                        else ret = vt.tempoDeIda(gpsLoja) + vt.tempoDeVolta(gpsLoja,gpsUt);
                        e.setPrecoEntrega(0);
                        e.setTempoDeEspera(ret);
                        vt.aceitaEncomenda(e.clone());
                        fn = new Voluntario(vt.clone());
                        this.encomendasAceites.add(new AceitaEncomenda(idE));
                        this.encomendasPorEnviar.remove(i);
                        break;
                    }
                }
            }
        }
        return fn;
    }

    /**
     * Função que distribui uma encomenda a enviar a uma empresa.
     * @param idE - Id da encomenda.
     * @return - Empresa que entrega a encomenda..
     */
    public Transportadora aceitaEncomendaT(String idE){
        Encomenda e = getEncomendaPorEnviar(idE);
        GPS gpsLoja = getLoja(e.getLoja()).getGps();
        GPS gpsUt = getUtilizador(e.getUser()).getGps();
        int info = getLoja(e.getLoja()).tempoDoPedido();
        int i = getIndiceEncomenda(idE);
        int ret = 0;
        Transportadora fn = null;
        if(e.getEncomendaMedica()){
            if(getTransportadorasLivresMed() != null){
                for(Transportadora t : getTransportadorasLivresMed()){
                    if(t.dentroDoRaio(gpsLoja,gpsUt)) {
                        if(info > t.tempoDeIda(gpsLoja)) ret = info + t.tempoDeVolta(gpsLoja,gpsUt);
                        else ret = t.tempoDeIda(gpsLoja) + t.tempoDeVolta(gpsLoja,gpsUt);
                        e.setPrecoEntrega(t.precoEntrega(gpsLoja,gpsUt));
                        e.setTempoDeEspera(ret);
                        t.aceitaEncomenda(e.clone());
                        fn = new Transportadora( t.clone());
                        this.encomendasAceites.add(new AceitaEncomenda(idE));
                        this.encomendasPorEnviar.remove(i);
                        break;
                    }
                }
            }
        }
        else{
            if(getTransportadorasLivres() != null){
                for(Transportadora tp : getTransportadorasLivres()){
                    if(tp.dentroDoRaio(gpsLoja,gpsUt)){
                        if(info > tp.tempoDeIda(gpsLoja)) ret = info + tp.tempoDeVolta(gpsLoja,gpsUt);
                        else ret = tp.tempoDeIda(gpsLoja) + tp.tempoDeVolta(gpsLoja,gpsUt);
                        e.setPrecoEntrega(tp.precoEntrega(gpsLoja,gpsUt));
                        e.setTempoDeEspera(ret);
                        tp.aceitaEncomenda(e.clone());
                        fn = new Transportadora(tp.clone());
                        this.encomendasAceites.add(new AceitaEncomenda(idE));
                        this.encomendasPorEnviar.remove(i);
                        break;
                    }
                }
            }
        }
        return fn;
    }


    /**
     * Função que entrega uma encomenda.
     * @param v - Voluntário que entrega a encomenda.
     */
    public void entregaEncomenda(Voluntario v){
        this.historicoEncomendas.add(v.entregaEncomenda());
        this.utilizadores.get(indiceUtil(v.entregaEncomenda().getUser())).addEncomenda(v.entregaEncomenda().clone());
    }

    public int indiceUtil(String id){
        int i = 0;
        for(Utilizador u : this.utilizadores){
            if(u.getId().equals(id)) break;
            i++;
        }
        return i;
    }

    /**
     * Função que entrega uma encomenda.
     * @param t - Transportadora que entraga a encomenda.
     */
    public void entregaEncomenda(Transportadora t){
        for(Encomenda e : t.entregaEncomenda()) {
            this.historicoEncomendas.add(e);
            this.utilizadores.get(indiceUtil(e.getUser())).addEncomenda(e.clone());
        }
    }

    /**
     * Função que dá uma empresa transportadora a partir do seu id.
     * @param id - Id da empresa.
     * @return - Empresa.
     */
    public Transportadora getEmpresa(String id){
        Transportadora ret = new Transportadora();
        for(Transportadora t : this.empresas){
            if(t.getId().equals(id)){
                ret = new Transportadora(t.clone());
                break;
            }
        }
        return ret;
    }

    /**
     * Função que calcula o total faturado por uma empresa transportador.
     * @param id - Id da empresa.
     * @return - Total faturado.
     */
    public int totalFaturadoEmpresa(String id){
        int ret = 0;
        Transportadora t = getEmpresa(id);
        for(Encomenda e : t.getEncomendasFeitas()){
            ret += e.getPrecoEntrega();
        }
        return ret;
    }

    /**
     * Grava o estado da aplicação num determinado ficheiro.
     *
     * @param nomeficheiro Recebe o nome do ficheiro.
     * @throws IOException           Exception.
     * @throws FileNotFoundException Exception.
     */
    public void grava(String nomeficheiro) throws IOException {
        FileOutputStream o = new FileOutputStream(nomeficheiro);
        ObjectOutputStream r = new ObjectOutputStream(o);
        r.writeObject(this);
        r.flush();
        r.close();
    }

    /**
     * Iniciar a aplicação com o estado guardado num determinado ficheiro.
     *
     * @param nomeficheiro Recebe o nome do ficheiro.
     * @return Devolve a aplicação inciada.
     * @throws IOException            Exception.
     * @throws ClassNotFoundException Exception.
     * @throws FileNotFoundException  Exception.
     */
    public static Sistema carrega(String nomeficheiro) throws IOException, ClassNotFoundException {
        FileInputStream r = new FileInputStream(nomeficheiro);
        ObjectInputStream o = new ObjectInputStream(r);
        Sistema g = (Sistema) o.readObject();
        o.close();
        return g;
    }



}


