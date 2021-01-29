package Model;

import java.util.*;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.stream.Collectors;

import Exceptions.NoEntityException;

public class RegistoEncomendas implements Serializable, IRegistoEncomendas{

    /*
     * variaveis de classe
     */
    private static final long serialVersionUID = 131L;

    /*
     * variaveis de instancia
     */
    private Map<String, IEncomenda> encPendentes, encProntas, encAceites, encFinalizadas, encPorAceitar;

    /**
     * Construtores da classe RegistoEncomendas.
     */

    /**
     * Construtor por omissao de RegistoEncomendas.
     */
    public RegistoEncomendas() {
        this.encPendentes = new HashMap<> ();
        this.encProntas = new HashMap<> ();
        this.encAceites = new HashMap<> ();
        this.encPorAceitar = new HashMap<>();
        this.encFinalizadas = new HashMap<> ();
    }

    /**
     * Construtor parametrizado de RegistoEncomendas.
     */
    public RegistoEncomendas( Map<String, IEncomenda> encPendentes,
                              Map<String, IEncomenda> encProntas,
                              Map<String, IEncomenda> encAceites,
                              Map<String, IEncomenda> encPorAceitar,
                              Map<String, IEncomenda> encFinalizadas) {
        setEncPendentes(encPendentes);
        setEncProntas(encProntas);
        setEncAceites(encAceites);
        setEncFinalizadas(encFinalizadas);
    }

    /**
     * Construtor de copia de RegistoEncomendas.
     */
    public RegistoEncomendas(RegistoEncomendas umRegisto) {
        setEncPendentes(umRegisto.getEncPendentes());
        setEncProntas(umRegisto.getEncProntas());
        setEncAceites(umRegisto.getEncAceites());
        setEncPorAceitar(umRegisto.getEncAceites());
        setEncFinalizadas(umRegisto.getEncFinalizadas());
    }

    /**
     * metodos de instancia
     */

    //gets

    /**
     * Devolve o conjunto de encomendas que foram feitas mas que as lojas ainda não têm disponiveis para serem entregues.
     *
     * @return o conjunto das encomendas pendentes.
     */
    public Map<String, IEncomenda> getEncPendentes() {
        return new HashMap<>(this.encPendentes);
    }

    /**
     * Devolve o conjunto de encomendas que a loja tem prontas para serem entregues mas ainda não têm um entregador associado.
     *
     * @return o conjunto de encomendas prontas.
     */
    public Map<String, IEncomenda> getEncProntas() {
        return new HashMap<>(this.encProntas);
    }

    /**
     * Devolve o conjunto de encomendas que estão prontas para serem entregues e já têm entregador associado, mas ainda não foram entregues ao utilizador.
     *
     * @return o conjunto de encomendas aceites.
     */
    public Map<String, IEncomenda> getEncAceites() {
        return new HashMap<>(this.encAceites);
    }

    /**
     * Devolve o conjunto de encomendas que estão prontas para serem entregues e o entregador está a ser validado com o utilizador pelo que ainda não foram entregues ao utilizador.
     *
     * @return o conjunto de encomendas por aceitar.
     */
    public Map<String, IEncomenda> getEncPorAceitar() {
        return new HashMap<>(this.encPorAceitar);
     }

    /**
     * Devolve o conjunto de encomendas que já foram entregues ao utilizador.
     *
     * @return o conjunto de encomendas finalizadas.
     */
    public Map<String, IEncomenda> getEncFinalizadas() {
        return new HashMap<>(this.encFinalizadas);
    }

    //sets

    /**
     * Atualiza o conjunto de encomendas que foram feitas mas que as lojas ainda não têm disponiveis para serem entregues.
     *
     * @param novaEncPendentes novo conjuntos de encomendas pendentes.
     */
    public void setEncPendentes(Map<String, IEncomenda> novaEncPendentes) {
        this.encPendentes =  new HashMap<>(novaEncPendentes);
    }

    /**
     * Atualiza o conjunto de encomendas que a loja tem prontas para serem entregues mas ainda não têm um entregador associado.
     *
     * @param novaEncProntas novo conjuntos de encomendas prontas.
     */
    public void setEncProntas(Map<String, IEncomenda> novaEncProntas) {
       this.encProntas = new HashMap<>(novaEncProntas);
    }

    /**
     * Atualiza o conjunto de encomendas que estão prontas para serem entregues e já têm entregador associado, mas ainda não foram entregues ao utilizador.
     *
     * @param novaEncAceites novo conjuntos de encomendas aceites.
     */
    public void setEncAceites(Map<String, IEncomenda> novaEncAceites){
        this.encAceites = new HashMap<>(novaEncAceites);
    }

    /**
     * Atualiza o conjunto de encomendas que estão prontas para serem entregues e o entregador está a ser validado com o utilizador pelo que ainda não foram entregues ao utilizador.
     *
     * @param novaEncNAceites novo conjuntos de encomendas aceites.
     */
    public void setEncPorAceitar(Map<String, IEncomenda> novaEncNAceites){
        this.encPorAceitar = new HashMap<>(novaEncNAceites);
    }

    /**
     * Atualiza o conjunto de encomendas  que já foram entregues ao utilizador.
     *
     * @param novaEncFinalizadas novo conjuntos de encomendas finalizadas.
     */
    public void setEncFinalizadas(Map<String, IEncomenda> novaEncFinalizadas) {
        this.encFinalizadas = new HashMap<>(novaEncFinalizadas);
    }

    //outros metodos obrigatorios

    /**
     * Metodo que devolve a representacao em String do Registo de Encomendas.
     * @return String com os varios conjuntos de encomendas que constituem o Registo de Encomendas.
     */
    public String toString() {
        StringBuilder s=new StringBuilder();
        s.append("\nConjunto de Encomendas Pendentes: ");
        s.append(this.encPendentes.toString());
        s.append("\nConjunto de Encomendas Prontas: ");
        s.append(this.encProntas.toString());
        s.append("\nConjunto de Encomendas Aceites: ");
        s.append(this.encAceites.toString());
        s.append("\nConjunto de Encomendas Finalizadas: ");
        s.append(this.encFinalizadas.toString());
        s.append("\n");
        return s.toString();
    }

    /**
     * Metodo que verifica se o Objeto o e igual ao Voluntario para o qual a funçao e chamada
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        RegistoEncomendas p = (RegistoEncomendas) o;
        return (   this.encPendentes.equals(p.getEncPendentes())
                && this.encProntas.equals(p.getEncProntas())
                && this.encAceites.equals(p.getEncAceites())
                && this.encPorAceitar.equals(p.getEncPorAceitar())
                && this.encFinalizadas.equals(p.getEncFinalizadas()));
    }

    public RegistoEncomendas clone() {
        return new RegistoEncomendas(this);
    }

    //metodos especificos

    /**
     * Metodo que adiciona uma Encomenda ao conjunto de encomendas Pendentes.
     */
    public void adicionaPendentes(IEncomenda novaEncomenda){
        this.encPendentes.putIfAbsent(novaEncomenda.getCodigo(), novaEncomenda);
    }


    /**
     * Metodo que adiciona uma Encomenda ao conjunto de encomendas Aceites.
     */
    public void adicionaAceites(String cod){
        IEncomenda e = this.encPendentes.get(cod);
        if(e != null){
            this.encAceites.putIfAbsent(cod, e);
            this.encPendentes.remove(cod);
        } 
    }

    //Provavelmente para apagar (este estado não aparece no ficheiro log dos professores, logo, não devo precisar desta função)
    /**
     * Metodo que adiciona uma Encomenda ao conjunto de encomendas Finalizadas.
     */
    /*public void adicionaFinalizadas(String cod){
        boolean found= false;
        Iterator<Encomenda> i= this.encPendentes.iterator();
        while(!found && i.hasNext()){
            Encomenda aux = i.next();
            if(aux.getCodigo().equals(cod)){
                found=true;
                this.encFinalizadas.add(aux);
                i.remove();
            }
        }
    }


    /**
     * Metodo que passa uma encomenda do conjunto das Encomendas pendentes para o conjunto das encomendas prontas.
     *
     * @param codEnc codigo da encomenda a ser movida
     */
    public void pendentesParaProntas(String codEnc) throws NoEntityException{
        IEncomenda e = this.encPendentes.get(codEnc);
        if(e != null){
            this.encProntas.putIfAbsent(codEnc, e);
            this.encPendentes.remove(codEnc);
        } 
        else
            throw new NoEntityException("encomenda \'" + codEnc + "\' inexistente.");
    }

    /**
     * Metodo que passa uma encomenda do conjunto das Encomendas prontas para o conjunto das encomendas aceites.
     *
     * @param codEnc codigo da encomenda a ser movida
     */
    public void prontasParaAceites(String codEnc) throws NoEntityException{
        IEncomenda e = this.encProntas.get(codEnc);
        if(e != null){
            this.encAceites.putIfAbsent(codEnc, e);
            this.encProntas.remove(codEnc);
        } 
        else
            throw new NoEntityException("encomenda \'" + codEnc + "\' inexistente.");
    }

    /**
     * Metodo que passa uma encomenda do conjunto das Encomendas aceites para o conjunto das encomendas finalizadas.
     *
     * @param codEnc codigo da encomenda a ser movida
     */
    public void aceitesParaFinalizadas(String codEnc) throws NoEntityException{
        IEncomenda e = this.encAceites.get(codEnc);
        if(e != null){
            this.encFinalizadas.putIfAbsent(codEnc, e);
            this.encAceites.remove(codEnc);
        } 
        else
            throw new NoEntityException("encomenda \'" + codEnc + "\' inexistente.");
    }


    /**
     * Metodo que diz o estado de uma determinada encomenda.
     *
     * @param codEnc codigo da encomenda cujo estado queremos saber
     */
    public String estadoEnc(String codEnc) throws NoEntityException{
        boolean found = false;
        String res = "n/a";

        if(found = (this.encPendentes.get(codEnc) != null))
            res = "Pendente";
        if(!found && (found = (this.encProntas.get(codEnc) != null)))
            res = "Pronta";
        if(!found && (found = (this.encAceites.get(codEnc) != null)))
            res = "Aceite";
        if(!found && (found = (this.encFinalizadas.get(codEnc) != null)))   
            res = "Finalizada";
        if(!found && (found = this.encPorAceitar.values().stream().filter(x -> x.getCodigo().equals(codEnc)).count() > 0))
            res = "Por aceitar pedido de entrega";
        if(!found)
            throw new NoEntityException("encomenda \'" + codEnc +"\' inexistente.");

        return res;
    }

    /**
     * Metodo que devolve a encomenda pronta de codigo enc.
     */
    public IEncomenda getEncPronta(String enc){
        return  this.encProntas.get(enc);
    }

    /**
     * Metodo que devolve a encomenda aceite de codigo enc.
     */
    public IEncomenda getEncAceite(String enc){
        return  this.encAceites.get(enc);
    }

    /**
     * Metodo que devolve a encomenda por aceitar de codigo enc.
     */
    public IEncomenda getEncPorAceitar(String dist){
        return  this.encPorAceitar.get(dist);
    }

    /**
     * Metodo que determina se dois pontos sao iguais.
     * @return booleano que e verdadeiro se os valores das duas coordenadas forem iguais
     */
   /* public boolean iguais(RegistoEncomendas umRegisto) {
        return (   this.encPendentes.equals(umRegisto.getEncPendentes())
                && this.encProntas.equals(umRegisto.getEncProntas())
                && this.encAceites.equals(umRegisto.getEncAceites())
                && this.encFinalizadas.equals(umRegisto.getEncFinalizadas()) );
    }*/

    /**
     * Metodo que devolve a encomenda de codigo enc.
     */
    public IEncomenda getEncomenda(String codEnc){
        IEncomenda e;
        if((e = this.encAceites.get(codEnc)) != null) return e;
        else return this.encFinalizadas.get(codEnc);
    }

    /**
     * Metodo que devolve o conjunto de encomendas feitas por um dado utilizador
     */
    public Set<IEncomenda> getEncUt(String cod){
        Set<IEncomenda> res = new HashSet<>();
        for(IEncomenda e: this.encPendentes.values()){
            if(e.getCodUtil().equals(cod)) res.add(e);
        }
        for(IEncomenda e: this.encAceites.values()){
            if(e.getCodUtil().equals(cod)) res.add(e);
        }
        for(IEncomenda e: this.encProntas.values()){
            if(e.getCodUtil().equals(cod)) res.add(e);
        }
        for(IEncomenda e: this.encFinalizadas.values()){
            if(e.getCodUtil().equals(cod)) res.add(e);
        }
        for(IEncomenda e: this.encPorAceitar.values()){
            if(e.getCodUtil().equals(cod)) res.add(e);
        }
        return res;
    }

    /**
     * Metodo que devolve o conjunto de encomendas de produtos de uma dada loja
     * @param cod codigo da loja cujas encomendas queremos
     */
    public Set<IEncomenda> getEncLoja(String cod){
        Set<IEncomenda> res = new HashSet<>();
        for(IEncomenda e: this.encPendentes.values()){
            if(e.getCodLoja().equals(cod)) res.add(e);
        }
        for(IEncomenda e: this.encProntas.values()){
            if(e.getCodLoja().equals(cod)) res.add(e);
        }
        for(IEncomenda e: this.encAceites.values()){
            if(e.getCodLoja().equals(cod)) res.add(e);
        }
        for(IEncomenda e: this.encFinalizadas.values()){
            if(e.getCodLoja().equals(cod)) res.add(e);
        }
        return res;
    }

    /**
     * Metodo que passa uma encomenda de pronta para aceite
     */
    public void aceitada(String enc, LocalDateTime ldt) throws NoEntityException {
        IEncomenda e;
        if((e = this.encProntas.remove(enc)) == null) throw new NoEntityException("encomenda \'" + enc + "\' inexistente");
        e.setDataAceitacao(ldt);
        this.encAceites.put(enc, e);
    }

    /**
     * Metodo que retira a encomenda de prontas para por aceitar e a associa ao custo dos portes da transportadora que está a ser "aconcelhada" ao utilizador
     */
    public void transportadoraPorAceitar(String transportadora, String enc, BigDecimal portes) throws NoEntityException{
        IEncomenda e;
        if((e = this.encProntas.remove(enc)) == null) throw new NoEntityException("encomenda \'" + enc + "\' inexistente");
        e.setPortes(portes);
        this.encPorAceitar.put(transportadora, e);
    }

    /**
     * Metodo que passa a enc de por aceitar para aceite no caso que o utilizador ter aceite os custos dos portes da transportadora que lhe estava a ser aconcelhada
     */
    public void transportadoraAceitada(String transportadora, LocalDateTime ldt)throws NoEntityException{
        IEncomenda e;
        if((e = this.encPorAceitar.remove(transportadora)) == null)  throw new NoEntityException("a transportadora \'" + transportadora + "\' não tem a encomenda por aceitar.");
        e.setDataAceitacao(ldt);
        this.encAceites.put(e.getCodigo(), e);
    }

    /**
     * Metodo que retira as informações da transportadora e zera os portes no caso que o utilizador não ter aceite os custos dos portes da transportadora que lhe estava a ser aconcelhada
     */
    public void transportadoraRejeitada(String transportadora)throws NoEntityException{
        IEncomenda e;
        if((e = this.encPorAceitar.remove(transportadora)) == null) throw new NoEntityException("a transportadora \'" + transportadora + "\' não tem a encomenda por aceitar.");
        e.setPortes(BigDecimal.ZERO);
        this.encProntas.put(e.getCodigo(), e);
    }

    /**
     * Metodo que devolve a lista de encomendas de uma determinada loja que se encontram como pendentes
     */
    public List<IEncomenda> encomendasPendentesALoja(String loja){
        return this.encPendentes.values().stream().filter(x -> x.getCodLoja().equals(loja)).collect(Collectors.toList());
    }

    /**
     * Metodo que devolve a lista de map entry de Strings e encomendas que contem as encomendas feitas por um utilizador que se encontram por aceitar (ordenada em função do valor)
     */
    public List<Map.Entry<String, IEncomenda>> encomendasPorAceitarDoUtilizador(String utilizador){
        Comparator<Map.Entry<String, IEncomenda>> c = (m1,m2) -> m2.getValue().getData().compareTo(m1.getValue().getData());

        return this.getEncPorAceitar().entrySet().stream().filter(x -> x.getValue().getCodUtil().equals(utilizador)).sorted(c).collect(Collectors.toList());
    }

    /**
     * Metodo que devolve a encomenda por aceitar de codigo cod
     */
    public IEncomenda encsPorAceitar(String cod){
        return getEncPorAceitar(cod);

    }

    public void pendenteAtualizadaParaProntas(IEncomenda atualizada) throws NoEntityException{
        this.encPendentes.put(atualizada.getCodigo(), atualizada);
        this.pendentesParaProntas(atualizada.getCodigo());
    }

}