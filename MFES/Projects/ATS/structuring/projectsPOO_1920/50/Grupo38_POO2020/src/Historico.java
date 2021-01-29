import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class Historico implements Serializable {
    private Map<String,Registos> accepted;
    private Map<String,Registos> pending;
    private Map<String,Registos> finalized;
    private Map<String,Registos> delivered;
    private Map<String,Registos> terminated;
    private Map<String,List<StringDistAux>> fila;

    public Historico (){
        this.accepted = new HashMap<>();
        this.pending = new HashMap<>();
        this.finalized = new HashMap<>();
        this.delivered = new HashMap<>();
        this.terminated = new HashMap<>();
        this.fila = new HashMap<>();
    }

    /**Método que adiciona um novo registo à respectiva fila
     *
     * @param i indíce de modo
     * @param a Registo
     */
    public void adicionaReg (int i, Registos a ){
        if (i==1) {this.accepted.replace(a.getEnc().getCod(),a); this.accepted.putIfAbsent(a.getEnc().getCod(),a);}
        else if (i==2) {this.finalized.replace(a.getEnc().getCod(),a); this.finalized.putIfAbsent(a.getEnc().getCod(),a);}
        else if (i==3) {this.pending.replace(a.getEnc().getCod(),a); this.pending.putIfAbsent(a.getEnc().getCod(),a);}
        else if (i==4) {this.terminated.replace(a.getEnc().getCod(),a); this.terminated.putIfAbsent(a.getEnc().getCod(),a);}
        else {this.delivered.replace(a.getEnc().getCod(),a); this.delivered.putIfAbsent(a.getEnc().getCod(),a);}
    }

    /**Método que remove um registo da respectiva fila
     *
     * @param i indíce de modo
     * @param a Registo
     */
    public Registos removeReg (int i, Registos a ){
        if (i==1) return this.accepted.remove(a.getEnc().getCod());
        else if (i==2) return this.finalized.remove(a.getEnc().getCod());
        else if (i==3) return this.pending.remove(a.getEnc().getCod());
        else if (i==4) return this.terminated.remove(a.getEnc().getCod());
        else if (i==5) return this.delivered.remove(a.getEnc().getCod());
        else return null;
    }

    /**Método que adiciona um par cod , List<StringDistAux> ao map fila
     *
     * @param cod Código
     * @param a Lista StringDistAux
     */
    public void adicionaFila (String cod,List<StringDistAux> a){
        this.fila.replace(cod,a);
        this.fila.putIfAbsent(cod,a);
    }

    /**Método que remove um par cod , List<StringDistAux> do map fila
     *
     * @param cod Código
     */
    public void removeFila (String cod){
        this.fila.remove(cod);
    }

    /**Método que retorna uma List<registos> dos voluntários dependendo do modo de fila desejado
     *
     * @param codVol Código voluntário
     * @param modo Modo
     */
    public List<Registos> volHist(String codVol,int modo){
        List<Registos> regV;
        if(modo==1)
            regV = this.accepted.values().stream().filter(x->(x.getDriver().equals(codVol))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==2)
            regV = this.terminated.values().stream().filter(x->(x.getDriver().equals(codVol))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==3)
            regV = this.delivered.values().stream().filter(x->(x.getDriver().equals(codVol))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==4)
            regV =this.pending.values().stream().filter(x->(x.getDriver().equals(codVol))).map(x->x.clone()).collect(Collectors.toList());
        else  regV =this.finalized.values().stream().filter(x->(x.getDriver().equals(codVol))).map(x->x.clone()).collect(Collectors.toList());
        return regV;
    }

    /**Método que retorna uma List<registos> das empresas dependendo do modo de fila desejado
     *
     * @param codEmp Código Empresa
     * @param modo Modo
     */
    public List<Registos> empHist(String codEmp,int modo){
        List<Registos> regE;
        if(modo==1)
            regE = this.accepted.values().stream().filter(x->(x.getDriver().equals(codEmp))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==2)
            regE =this.terminated.values().stream().filter(x->(x.getDriver().equals(codEmp))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==3)
            regE =this.delivered.values().stream().filter(x->(x.getDriver().equals(codEmp))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==4)
            regE =this.pending.values().stream().filter(x->(x.getDriver().equals(codEmp))).map(x->x.clone()).collect(Collectors.toList());
        else regE =this.finalized.values().stream().filter(x->(x.getDriver().equals(codEmp))).map(x->x.clone()).collect(Collectors.toList());
        return regE;
    }

    /**Método que retorna uma List<registos> dos utilizadores dependendo do modo de fila desejado
     *
     * @param codUser Código utilizador
     * @param modo Modo
     */
    public List<Registos> userHist(String codUser,int modo){
        List<Registos> regU;
        if(modo==1)
            regU = this.accepted.values().stream().filter(x->(x.getUser().equals(codUser))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==2)
            regU =this.terminated.values().stream().filter(x->(x.getUser().equals(codUser))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==3)
            regU =this.delivered.values().stream().filter(x->(x.getUser().equals(codUser))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==4)
            regU =this.pending.values().stream().filter(x->(x.getUser().equals(codUser))).map(x->x.clone()).collect(Collectors.toList());
        else regU =this.finalized.values().stream().filter(x->(x.getUser().equals(codUser))).map(x->x.clone()).collect(Collectors.toList());
        return regU;
    }

    /**Método que retorna uma List<registos> das lojas dependendo do modo de fila desejado
     *
     * @param codLoja Código Loja
     * @param modo Modo
     */
    public List<Registos> lojaHist(String codLoja,int modo){
        List<Registos> regU;
        if(modo==1)
            regU = this.accepted.values().stream().filter(x->(x.getLoja().equals(codLoja))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==2)
            regU =this.terminated.values().stream().filter(x->(x.getLoja().equals(codLoja))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==3)
            regU =this.delivered.values().stream().filter(x->(x.getLoja().equals(codLoja))).map(x->x.clone()).collect(Collectors.toList());
        else if (modo==4)
            regU =this.pending.values().stream().filter(x->(x.getLoja().equals(codLoja))).map(x->x.clone()).collect(Collectors.toList());
        else regU =this.finalized.values().stream().filter(x->(x.getLoja().equals(codLoja))).map(x->x.clone()).collect(Collectors.toList());
        return regU;
    }

    /**Método que faz a gestaoEncomenda para os registos declined
     *
     * @param a Registo
     */
    public void declinedT (Registos a){
        gestaoEncomenda(a.getEnc().clone(),a.getUser(),this.fila.get(a.getEnc().getCod()));
    }

    /**Método que adiciona um novo registo à fila de terminated se a List<StringDistAux> estiver vazia e pending se não. Retorna 0 se for para a terminated e 1 se for para pending
     *
     * @param enc Encomenda
     * @param cod Código
     * @param a List StringDistAux
     *
     */
    public int gestaoEncomenda(Encomenda enc, String cod, List<StringDistAux> a){
        int res=1;
        if (a.isEmpty()){
            adicionaReg(4,new Registos(enc, LocalDateTime.now(), cod, "N/A", -1.0,-1.0,-1.0));
            res=0;
        }
        else {
            adicionaReg(3, new Registos(enc, LocalDateTime.now(), cod, a.remove(0).getCod(), enc.custo(),0.0,(double) 0.0));
            adicionaFila(enc.getCod(), a);
        }
        return res;
    }

    /**Método que remove um registo da fila accepted e finaliza o seu tempo retornando o registo
     *
     * @param enc Encomenda
     * @param temp Tempo
     */
    public Registos done (String enc, double temp){
        Registos f = this.accepted.remove(enc);
        f.setTmp(temp);
        return f.clone();
    }

    /**Método que retorna uma Lista<topUsers> com o top dez de utilizadores que mais usaram o sistema */
    public List<topUsers> topUser(){
        Map <String,topUsers> a  = new HashMap();

        for(Registos reg : this.finalized.values()){
            a.putIfAbsent(reg.getUser(),new topUsers(reg.getUser()));
            a.get(reg.getUser()).addQnt();
        }

        return a.values().stream().sorted(Comparator.comparing(topUsers::getQnt).reversed()).limit(10).collect(Collectors.toList());
    }

    /** Método que retorna uma List<Registos> respectiva às transportadoras na fila finalized*/
    public List<Registos> finList (){
        return this.finalized.values().stream().filter(x->(x.getDriver().charAt(0)=='t')).map(x->x.clone()).collect(Collectors.toList());
    }
}
