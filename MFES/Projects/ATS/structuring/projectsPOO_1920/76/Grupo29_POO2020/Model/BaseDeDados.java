package Model;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import Exceptions.InvalidInputException;
import Exceptions.NoEntityException;
import Utilities.Ponto;
import Utilities.Rating;

/** 
 * Classe que representa uma base de dados. 
 * 
 * Armazena todo o tipo de entidades. 
 * (isto é, contas, lojas, transportadoras, utilizadores e voluntários.)
*/
public class BaseDeDados implements Serializable, IBaseDeDados{
    private static final long serialVersionUID = 123L;

    /** Dicionário de utilizadores. */
    private Map<String, IUtilizador> utilizadores;

    /** Dicionário de transportadoras. */
    private Map<String, ITransportadora> transportadoras;

    /** Dicionário de voluntarios. */
    private Map<String, IVoluntario> voluntarios;

    /** Dicíonário de lojas. */
    private Map<String, ILoja> lojas;

    /** Contas ordenadas por email. */
    private Map<String, IConta> contas;



    /** Construtor vazio. */
    public BaseDeDados() {
        this.utilizadores    = new HashMap<>();
        this.transportadoras = new HashMap<>();
        this.voluntarios     = new HashMap<>();
        this.lojas           = new HashMap<>();
        this.contas          = new HashMap<>();
    }

    /**
     * Construtor parametrizado.
     * @param u Dicionário de utilzadores.
     * @param t Dicionário de transportadoras.
     * @param v Dicionário de voluntários.
     * @param l Dicionário de lojas.
     * @param c Dicionário de contas.
     */
    public BaseDeDados(Map<String, IUtilizador> u, Map<String, ITransportadora> t, Map<String, IVoluntario> v, Map<String, ILoja> l, Map<String, IConta> c) {
        this.utilizadores    = new HashMap<>(u);
        this.transportadoras = new HashMap<>(t);
        this.voluntarios     = new HashMap<>(v);
        this.lojas           = new HashMap<>(l);
        this.contas          = new HashMap<>(c);
    }

    /**
     * Construtor de cópia.
     * @param db Base de dados a ser copiada.
     */
    public BaseDeDados(BaseDeDados db) {
        this.utilizadores    = new HashMap<>(db.getUtilizadores());
        this.transportadoras = new HashMap<>(db.getTransportadoras());
        this.voluntarios     = new HashMap<>(db.getVoluntarios());
        this.lojas           = new HashMap<>(db.getLojas());
        this.contas          = new HashMap<>(db.getContas());
    }


    public Map<String, IUtilizador> getUtilizadores() {
        return new HashMap<>(this.utilizadores);
    }

    public void setUtilizadores(Map<String, IUtilizador> u) {
        this.utilizadores = new HashMap<>(u);
    }

    public Map<String, ITransportadora> getTransportadoras() {
        return new HashMap<>(this.transportadoras);
    }

    public void setTransportadoras(Map<String, ITransportadora> t) {
        this.transportadoras = new HashMap<>(t);
    }

    public Map<String, IVoluntario> getVoluntarios() {
        return new HashMap<>(this.voluntarios);
    }

    public void setVoluntarios(Map<String, IVoluntario> v) {
        this.voluntarios = new HashMap<>(v);
    }

    public Map<String, ILoja> getLojas() {
        return new HashMap<>(this.lojas);
    }

    public void setLojas(Map<String, ILoja> l) {
        this.lojas = new HashMap<>(l);
    }

    public Map<String, IConta> getContas() {
        return new HashMap<>(contas);
    }

    public void setContas(Map<String, IConta> c) {
        this.contas = new HashMap<>(c);
    }


    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("Lojas: ").append(this.lojas);
        sb.append("\nTransportadoras: ").append(this.transportadoras);
        sb.append("\nUtilizadores: ").append(this.utilizadores);
        sb.append("\nVoluntarios: ").append(this.voluntarios);
        sb.append("\nContas: ").append(this.contas);

        return sb.toString();
    }

    public BaseDeDados clone() {
        return new BaseDeDados(this);
    }


    public void addUtilizador(String codigo, String nome, Ponto posicao) {
        IUtilizador u = new Utilizador(codigo, nome, posicao.clone(), 0);
        utilizadores.put(codigo, u);
    }
    
    public void addUtilizador(IUtilizador util) {
        utilizadores.put(util.getId(), util);
    }

    
    public void addTransportadora(String codigo, String nome, String nif, Ponto posicao, double raio, BigDecimal ppkm) {
        ITransportadora t = new Transportadora(codigo, nome, nif, posicao.clone(), raio, ppkm, new HashSet<>(), new Rating(), 0, new HashMap<>());
        transportadoras.put(codigo, t);
    }

    public void addTransportadora(ITransportadora transp) {
        transportadoras.put(transp.getId(), transp);
    }


    public void addVoluntario(String codigo, String nome, Ponto posicao, double raio) {
        IVoluntario v = new Voluntario(codigo, nome, posicao.clone(), raio, true, true, new Rating(), new HashSet<>(), 0, new HashMap<>());
        voluntarios.put(codigo, v);
    }

    public void addVoluntario(IVoluntario volun) {
        voluntarios.put(volun.getId(), volun);
    }


    public void addLoja(String codigo, String nome, Ponto posicao) {
        ILoja l = new Loja(codigo, nome, posicao.clone(), 0);
        lojas.put(codigo, l);
    }

    public void addLoja(ILoja loja) {
        lojas.put(loja.getId(), loja);
    }


    public void addConta(String codigo, String email, String password, List<String> notificacoes) {
        IConta c = new Conta(codigo, email, password, new ArrayList<>(notificacoes));
        contas.put(email, c);
    }

    public void addConta(String codigo, String email, String password) {
        IConta c = new Conta(codigo, email, password, new ArrayList<>());
        contas.put(email, c);
    }

    public void addConta(IConta c) {
        contas.put(c.getEmail(), c);
    }


    /* - - - remover elementos da base de dados - - - */

    public void delUtilizador(String codigo) {
        utilizadores.remove(codigo);
    }
    public void delTransportadora(String codigo) {
        transportadoras.remove(codigo);
    }
    public void delVoluntario(String codigo) {
        voluntarios.remove(codigo);
    }
    public void delLoja(String codigo) {
        lojas.remove(codigo);
    }

    public void delEntidade(String codigo) throws NoEntityException{
        switch(codigo.charAt(0)){
            case 'u': utilizadores.remove(codigo);      break;
            case 't': transportadoras.remove(codigo);   break;
            case 'v': voluntarios.remove(codigo);       break;
            case 'l': lojas.remove(codigo);             break;
            default: throw new NoEntityException("entidade \'" + codigo + "\' inexistente.");
        }
    }

    public void delConta(String email) throws NoEntityException{
        if(contas.remove(email) == null)
            throw new NoEntityException("nenhuma conta associada ao email \'" + email + "\'.");
    }


    /* - - - verificar se existem elementos na base de dados - - - */

    public boolean existeUtilizador(String codigo) {
        return utilizadores.containsKey(codigo);
    }
    public boolean existeTransportadora(String codigo) {
        return transportadoras.containsKey(codigo);
    }
    public boolean existeVoluntario(String codigo) {
        return voluntarios.containsKey(codigo);
    }
    public boolean existeLoja(String codigo) {
        return lojas.containsKey(codigo);
    }
    public boolean existeConta(String email) {
        return contas.containsKey(email);
    }


    /* - - - aceder a elementos da base de dados - - - */

    public IUtilizador getUtilizador(String codigo) {
        return utilizadores.get(codigo);
    }
    public ITransportadora getTransportadora(String codigo) {
        return transportadoras.get(codigo);
    }
    public IVoluntario getVoluntario(String codigo) {
        return voluntarios.get(codigo);
    }
    public ILoja getLoja(String codigo) {
        return lojas.get(codigo);
    }
    public IConta getConta(String email) {
        return contas.get(email);
    }



    public void addEncomendaDistribuidor(Distribuidor dist, String codigo) {
        if (dist instanceof IVoluntario) {
            IVoluntario v = (IVoluntario) dist;
            v.atualizaEncomendas(codigo);
        }
        else {
            ITransportadora t = (ITransportadora) dist;
            t.atualizaEncomendas(codigo);
        }
    }


    /**
     * Incrementa o número de encomendas efetuadas por um dado utilizador.
     * @param utilizador Utilizador ao qual será incrementado o número de encomendas.
     * @throws NoEntityException Caso o utilizador não exista.
     */
    public void incNEncUtilizador(IUtilizador utilizador) throws NoEntityException {
        IUtilizador user;
        if((user = this.utilizadores.get(utilizador.getId())) == null)
            throw new NoEntityException("utilizador inexistente.");
        user.incNEnc();
    }
    /**
     * Incrementa o número de encomendas efetuadas por um dado utilizador.
     * @param id de um utilizador ao qual será incrementado o número de encomendas.
     * @throws NoEntityException Caso o utilizador não exista.
     */
    public void incNEncUtilizador(String id) throws NoEntityException {
        IUtilizador user;
        if((user = this.utilizadores.get(id)) == null)
            throw new NoEntityException("utilizador \'" + id + "\' inexistente.");
        user.incNEnc();
    }

    public List<IUtilizador> getListUtilizadores() {
        return this.utilizadores.values().stream().collect(Collectors.toList());
    }

    public List<ITransportadora> getListTransportadoras() {
        return this.transportadoras.values().stream().collect(Collectors.toList());
    }

    public void addNotificacaoDistribuidoresMenosEste(String not, String distCod) throws NoEntityException{

        if(this.transportadoras.get(distCod) == null && this.voluntarios.get(distCod) == null)
            throw new NoEntityException("distribuidor \'" + distCod + "\' inexistente.");

        for(ITransportadora t : this.transportadoras.values())
            if(!t.getId().equals(distCod)) 
                for(IConta c: this.contas.values())
                    if(c.getId().equals(t.getId()))
                        c.addNotificacao(not); 

        for(IVoluntario v : this.voluntarios.values()) 
            if (!v.getId().equals(distCod)) 
                for(IConta c: this.contas.values())
                    if(c.getId().equals(v.getId()))
                        c.addNotificacao(not); 
        
    }

    public void addNotificacao(String not, String email) throws NoEntityException{
        IConta user;
        if((user = this.contas.get(email)) == null)
            throw new NoEntityException("nenhuma conta associada ao email \'" + email + "\'.");
        user.addNotificacao(not);
    }

    public void addNotificacaoPorCodigo(String noti, String cod) throws NoEntityException{
        List<IConta> cs = this.contas.values().stream().filter(x -> x.getId().equals(cod)).collect(Collectors.toList());
        if(cs.size() <= 0) throw new NoEntityException("nenhuma conta associada ao user id \'" + cod + "\'");
        cs.get(0).addNotificacao(noti);
    }

    public Set<String> encAceitesPorTransportadora (String transCod) throws NoEntityException{
        ITransportadora t;
        if((t = this.transportadoras.get(transCod)) == null)
            throw new NoEntityException("transportadora \'" + transCod + "\' inexistente.");
        return t.getEncomendas();
    }

    public Set<String> encAceitesPorVoluntario (String transCod) throws NoEntityException{
        IVoluntario v;
        if((v = this.voluntarios.get(transCod)) == null)
            throw new NoEntityException("voluntário \'" + transCod + "\' inexistente.");
        return v.getEncomendas();
    }

    public Set<String> getIDsContas(){
        return this.contas.keySet().stream().collect(Collectors.toSet());
    }

    public void entregue(String distribuidor, LocalDateTime tempoEntrega, double kms) throws NoEntityException, InvalidInputException{
        ITransportadora t;
        IVoluntario v;
        switch(distribuidor.charAt(0)){
           case 't': if((t = this.transportadoras.get(distribuidor)) == null) throw new NoEntityException("transportadora \'" + distribuidor + "\' inexistente."); else t.fezKm(tempoEntrega, kms); break;
           case 'v': if((v = this.voluntarios.get(distribuidor)) == null) throw new NoEntityException("voluntário \'" + distribuidor + "\' inexistente.");  else v.fezKm(tempoEntrega, kms); break;
           default: throw new NoEntityException("distribuidor \'" + distribuidor + "\' inexistente");
       }

    }

    public boolean aceitaEntregar(String distribuidor, String encCod) throws InvalidInputException, NoEntityException{
        ITransportadora t;
        IVoluntario v;
        if(encCod.charAt(0) != 'e') throw new InvalidInputException("\'" + encCod + "\' é um código de encomenda inválido.");
        switch(distribuidor.charAt(0)){
            case 't': if((t = this.transportadoras.get(distribuidor)) == null) throw new NoEntityException("transportadora \'" + distribuidor + "\' inexistente."); 
                      else if(t.getDisponivel()){t.atualizaEncomendas(encCod); t.inverteDisponivel(); } else return false;
                      break;
            case 'v': if((v = this.voluntarios.get(distribuidor)) == null) throw new NoEntityException("voluntário \'" + distribuidor + "\' inexistente."); 
                      else if(v.getDisponivel()){ v.atualizaEncomendas(encCod); v.inverteDisponivel();} else return false;
                      break;
            default: return false;
        }
        return true;
    }

    public BigDecimal calcularPortes(String dist, String util, String loja) throws NoEntityException{
        
        ITransportadora t = this.transportadoras.get(dist);
        if(t == null) throw new NoEntityException("transportadora \'" + transportadoras + "\' inexistente.");
        IUtilizador u = this.utilizadores.get(util);
        if(u == null) throw new NoEntityException("utilizador \'" + transportadoras + "\' inexistente.");
        ILoja l = this.lojas.get(loja);
        if(l == null) throw new NoEntityException("loja \'" + transportadoras + "\' inexistente.");

        double distanciaTotal = t.getPosicao().distancia(l.getPosicao());
        return t.getPpkm().multiply(BigDecimal.valueOf(distanciaTotal));
    }

    public void addNotificacaoDistribuidores(String notif){
        for(ITransportadora t : this.transportadoras.values())
                for(IConta c: this.contas.values())
                    if(c.getId().equals(t.getId()))
                        c.addNotificacao(notif); 

        for(IVoluntario v : this.voluntarios.values())
                for(IConta c: this.contas.values())
                    if(c.getId().equals(v.getId()))
                        c.addNotificacao(notif); 
    }

    public void transpMedicamentos(String vol, boolean b) throws NoEntityException{
        IVoluntario v = this.voluntarios.get(vol);
        if(v == null) throw new NoEntityException("voluntário \'" + vol + "\' inexistente.");
        v.setTransportaMed(b);
    }


    
    public void addClassificacao(String utilizador, int classificacao, String entregador) throws InvalidInputException{
        try{
            if(entregador.charAt(0)=='t') this.transportadoras.get(entregador).atualizaClassificacao(utilizador, classificacao);
            else this.voluntarios.get(entregador).atualizaClassificacao(utilizador, classificacao);
        }catch(NullPointerException e){
            throw new InvalidInputException("o distribuidor " + entregador + " não existe na base de dados.");
        }
    }

    public void rejeitada(String transportadora, String encomenda) throws NoEntityException{
        ITransportadora t;
        if((t = this.transportadoras.get(transportadora)) == null) throw new NoEntityException("transportadora \'" + transportadoras + "\' inexistente.");
        t.adicionaRejeitada(encomenda);
    }

    public void limpaRejeitadas(String encomenda){
        this.transportadoras.values().stream().forEach(t -> t.removeRejeitada(encomenda));
    }

    public Distribuidor getDistDaEncomenda(String enc) throws NoEntityException{
        for(IVoluntario v : this.voluntarios.values())
            if(v.temEncomenda(enc))
                return (Distribuidor) v;
        for(ITransportadora t : this.transportadoras.values())
            if(t.temEncomenda(enc))
                return (Distribuidor) t;
        
        throw new NoEntityException("encomenda \'" + enc +"\' sem distribuidor associado.");
    }


    public void aumentaFilaLoja(String encLoja){
        this.lojas.get(encLoja).incrementa();
    }

    public void diminuiFilaLoja(String encLoja){
        this.lojas.get(encLoja).decrementa();
    }

}