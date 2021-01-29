package Model;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

public class TrazAqui implements Serializable {
    private Map<Integer,Login> loginInfo; // Map with all the necessary login information
    private Map<String,User> userCatalog; // Map with all existing users
    private Map<String,Voluntario> voluntarioCatalog; // Map with all existing volunteers
    private Map<String,Loja> lojas; // Map with all existing stores
    private Map<String,EmpresaTransportadora> empresas; // Map with all existing transporters
    private List<String> aceites;  // List of all accepted packages
    private GestaoEncomenda gestor; // Class with all pending packages

    /* Class constructors*/
    public TrazAqui(){
        this.loginInfo = new HashMap<>(5);
        this.loginInfo.put(1,new Login());
        this.loginInfo.put(2,new Login());
        this.loginInfo.put(3,new Login());
        this.loginInfo.put(4,new Login());
        this.loginInfo.put(5,new Login());
        this.userCatalog = new HashMap<>();
        this.voluntarioCatalog = new HashMap<>();
        this.lojas = new HashMap<>();
        this.empresas = new HashMap<>();
        this.aceites = new ArrayList<>();
        this.gestor = new GestaoEncomenda();
    }
    public TrazAqui (Map<Integer,Login> lo, Map<String,User> u, Map<String,Voluntario> v, Map<String,Loja> l, Map<String,EmpresaTransportadora> e, List<String> aceites, GestaoEncomenda g){
        setLoginInfo(lo);
        setUserCatalog(u);
        setVoluntarioCatalog(v);
        setLojas(l);
        setEmpresas(e);
        setAceites(aceites);
        setGestor(g);
    }
    public TrazAqui(TrazAqui s){
        setLoginInfo(s.getLoginInfo());
        setUserCatalog(s.getUserCatalog());
        setVoluntarioCatalog(s.getVoluntarioCatalog());
        setLojas(s.getLojas());
        setEmpresas(s.getEmpresas());
        setAceites(s.getAceites());
        setGestor(s.getGestor());
    }

    /* Getters and setters*/
    public Login getLoginInfo(int i) {
        return loginInfo.get(i);
    }
    public Map<Integer, Login> getLoginInfo() {
        return  loginInfo;
    }
    public void setLoginInfo(Map<Integer, Login> loginInfo) {
        this.loginInfo = loginInfo;
    }

    public Map<String,User> getUserCatalog(){
        Map<String,User> res = new HashMap<>();
        for(String s: this.userCatalog.keySet()){
            res.put(s,this.userCatalog.get(s).clone());
        }

        return res;
    }
    public void setUserCatalog(Map<String,User> u){
        this.userCatalog = new HashMap<>();
        for(String user: u.keySet()){
            this.userCatalog.put(user,u.get(user));
        }
    }

    /** Given a user, add that user to the System's user catalog.
     *
     * @param u - User that will be added.
     */
    public void addUser(User u){
        this.userCatalog.put(u.getCodUser(),u);
    }

    public Map<String,Voluntario> getVoluntarioCatalog(){
        Map<String,Voluntario> res = new HashMap<>();
        for(String v: this.voluntarioCatalog.keySet()){
            res.put(v,this.voluntarioCatalog.get(v).clone());
        }

        return res;
    }
    public void setVoluntarioCatalog(Map<String,Voluntario> voluntario){
        this.voluntarioCatalog = new HashMap<>();
        for(String v: voluntario.keySet()){
            this.voluntarioCatalog.put(v,voluntario.get(v));
        }
    }

    /** Given a volunteer, add that volunteer to the System's volunteer catalog.
     *
     * @param v - User that will be added.
     */
    public void addVoluntario(Voluntario v){
        this.voluntarioCatalog.put(v.getCodVoluntario(),v);
    }

    public Map<String,Loja> getLojas(){
        Map<String,Loja> res = new HashMap<>();
        for(String l: this.lojas.keySet()){
            res.put(l,this.lojas.get(l).clone());
        }

        return res;
    }
    public void setLojas(Map<String,Loja> loja){
        this.lojas = new HashMap<>();
        for(String l: loja.keySet()){
            this.lojas.put(l,loja.get(l));
        }
    }

    /** Given a store, add that store to the System's store catalog.
     *
     * @param l - User that will be added.
     */
    public void addLoja(Loja l){
        this.lojas.put(l.getStoreCode(),l);
    }

    public Map<String,EmpresaTransportadora> getEmpresas(){
        Map<String,EmpresaTransportadora> res = new HashMap<>();
        for(String e: this.empresas.keySet()){
            res.put(e,this.empresas.get(e));
        }

        return res;
    }
    public void setEmpresas(Map<String,EmpresaTransportadora> empresa){
        this.empresas = new HashMap<>();
        for(String e: empresa.keySet()){
            this.empresas.put(e,empresa.get(e));
        }
    }

    /** Given a company, add that company to the System's campanies catalog.
     *
     * @param et - User that will be added.
     */
    public void addEmpresaTransportadora(EmpresaTransportadora et){
        this.empresas.put(et.getCodTrans(),et);
    }

    public List<String> getAceites() {
        List<String> l = new ArrayList<>(this.aceites.size());
        int size = this.aceites.size();
        for(int i = 0; i < size; i++){
            l.add(i,this.aceites.get(i));
        }
        return l;
    }
    public void setAceites(List<String> aceites) {
        this.aceites = new ArrayList<>(aceites.size());
        int size = aceites.size();
        for(int i = 0; i < size; i++){
            this.aceites.add(i,aceites.get(i));
        }
    }

    /** Given a code of a package that has been accepted by a store, adds that package to the System's accepted packages register.
     *
     * @param aceite - Code that will be added.
     */
    public void addAceite(String aceite){
        int size = this.aceites.size();
        List<String> r = new ArrayList<>(size+1);
        int i;
        for(i = 0; i < size; i++){
            r.add(i,this.aceites.get(i));
        }
        r.add(i,aceite);
        setAceites(r);
        this.getGestor().getGestor().get(aceite).lojaAceita();

        Loja loja = this.lojas.get(this.getGestor().getGestor().get(aceite).getCodigoLoja());
        loja.addEncomendasHist(aceite);
    }

    public GestaoEncomenda getGestor(){
        return new GestaoEncomenda(this.gestor);
    }
    public void setGestor(GestaoEncomenda g){
        this.gestor = new GestaoEncomenda(g);
    }

    /** Adds a package to the System's package manager.
     *
     * @param e - Package that will be added.
     */
    public void addEncomenda(Encomenda e){
        this.gestor.addEncomenda(e);
        this.userCatalog.get(e.getCodigoUser()).addEnc(e.getCodigo());
        this.lojas.get(e.getCodigoLoja()).addEncomendasHist(e.getCodigo());
    }

    public void userEscolheTransportadora(Encomenda e, EmpresaTransportadora et){
        //Coloca na encomenda o codigo da transportadora responsável pela entrega
        this.getGestor().getGestor().get(e.getCodigo()).setCodigoTrans(et.getCodTrans());

        //Calcula o tempo de entrega usando um fator aleatório relativo à metereologia.
        double metFat;
        int metereologia = ThreadLocalRandom.current().nextInt(1,3+1);
        switch (metereologia){
            case 2: //Chuva
                metFat = 1.5;
                break;
            case 3: //Neve/Tempestade
                metFat = 2;
                break;
            case 1: //Bom tempo
            default:
                metFat = 1;
                break;
        }
        double t = et.getLocalizacao().distanceTo(this.userCatalog.get(e.getCodigoUser()).getLocation()) * 60 * metFat;
        e.setTempoEntrega(t);

        //Dá a empresa escolhida como ocupada para o sistema
        this.empresas.get(et.getCodTrans()).setStatus(false);

        //Adiciona a encomenda à lista de encomendas da transportadora
        int size = this.empresas.get(et.getCodTrans()).getEncomendas().size();
        List<String> list = new ArrayList<>(size + 1);
        list.addAll(this.empresas.get(et.getCodTrans()).getEncomendas());
        list.add(size,e.getCodigo());
        this.empresas.get(et.getCodTrans()).setEncomendas(list);

        //Adiciona a encomenda à lista de encomendas do user
        size = this.userCatalog.get(e.getCodigoUser()).getEncomendas().size();
        List<String> l = new ArrayList<>(size+1);
        l.addAll(this.userCatalog.get(e.getCodigoUser()).getEncomendas());
        l.add(size,e.getCodigo());
        this.userCatalog.get(e.getCodigoUser()).setEncomendas(l);
    }

    public void VoluntarioAssumeEntrega(Encomenda e, Voluntario v){
        //Coloca na encomenda o codigo do voluntario responsável pela entrega
        this.getGestor().getGestor().get(e.getCodigo()).setCodigoTrans(v.getCodVoluntario());

        //Calcula o tempo de entrega usando um fator aleatório relativo à metereologia.
        double metFat;
        int metereologia = ThreadLocalRandom.current().nextInt(1,3+1);
        switch (metereologia){
            case 2: //Chuva
                metFat = 1.5;
                break;
            case 3: //Neve/Tempestade
                metFat = 2;
                break;
            case 1: //Bom tempo
            default:
                metFat = 1;
                break;
        }
        double t = v.getLocalizacao().distanceTo(this.userCatalog.get(e.getCodigoUser()).getLocation()) * 45 * metFat;
        e.setTempoEntrega(t);

        //Dá o voluntario escolhido como ocupado para o sistema
        this.empresas.get(v.getCodVoluntario()).setStatus(false);

        //Adiciona a encomenda à lista de encomendas do voluntario
        int size = this.voluntarioCatalog.get(v.getCodVoluntario()).getEncomendas().size();
        List<String> list = new ArrayList<>(size + 1);
        list.addAll(this.voluntarioCatalog.get(v.getCodVoluntario()).getEncomendas());
        list.add(size,e.getCodigo());
        this.voluntarioCatalog.get(v.getCodVoluntario()).setEncomendas(list);

        //Adiciona a encomenda à lista de encomendas do user
        size = this.userCatalog.get(e.getCodigoUser()).getEncomendas().size();
        List<String> l = new ArrayList<>(size+1);
        l.addAll(this.userCatalog.get(e.getCodigoUser()).getEncomendas());
        l.add(size,e.getCodigo());
        this.userCatalog.get(e.getCodigoUser()).setEncomendas(l);
    }

    /** Once a package is delivered by a delivery company, the user get's the chance to rate the company. This calculates and sets the rate to the given company.
     *
     * @param rate - Rate given by the user.
     * @param codeTrans - Delivery Company that is being rated.
     */
    public void avaliarTransportadora(int rate, String codeTrans){
        double r = ((this.empresas.get(codeTrans).getRate() * this.empresas.get(codeTrans).getEncomendas().size()) + rate) / (this.empresas.get(codeTrans).getEncomendas().size() + 1);
        this.empresas.get(codeTrans).setRate(r);
    }

    /** Once a package is delivered by a volunteer, the user get's the chance to rate the volunteer. This calculates and sets the rate to the given volunteer.
     *
     * @param rate - Rate given by the user.
     * @param code - Delivery Company that is being rated.
     */
    public void avaliarVoluntario(int rate, String code){
        double r = ((this.voluntarioCatalog.get(code).getRate() * this.voluntarioCatalog.get(code).getEncomendas().size()) + rate) / (this.voluntarioCatalog.get(code).getEncomendas().size() + 1);
        this.voluntarioCatalog.get(code).setRate(r);
    }

    /** Given a store and a location, changes that store's location to the new one.
     *
     * @param store - store whose location is being changed.
     * @param l - new location of the store.
     */
    public void changeStoreLocation(Loja store, Location l){
        store.setLocation(l);
        this.getLojas().replace(store.getStoreCode(),store);
    }

    /** Once a store confirms a package, adds the package to the store's history and declares the package as accpeted by the store.
     *
     * @param e - package that has been accepted
     */
    public void lojaAceitaEncomenda(Encomenda e){
        this.getLojas().get(e.getCodigoLoja()).addEncomendasHist(e.getCodigo());
        this.getGestor().getGestor().get(e.getCodigo()).lojaAceita();
    }

    /** Builds a list with all available volunteers to deliver a certain package.
     *
     * @param e - package
     * @return list with all available volunteers to deliver the package.
     */
    public List<Voluntario> voluntariosDisponiveis (Encomenda e){
        Set<Voluntario> vSet = new TreeSet<>();
        int size = 0;
        Location l = new Location(this.getLojas().get(e.getCodigoLoja()).getLocation());
        for(String s: this.getVoluntarioCatalog().keySet()){
            Voluntario vol = new Voluntario(this.getVoluntarioCatalog().get(s));
            if(vol.isAvailable(l)){
                vSet.add(vol);
                size++;
            }
        }
        List<Voluntario> v = new ArrayList<>(size);
        int i = 0;
        for(Voluntario vs: vSet){
            v.add(i,vs);
            i++;
        }

        return v;
    }

    /** Builds a list with all available companies to deliver a certain package.
     *
     * @param e - package
     * @return list with all available companies to deliver the package.
     */
    public List<EmpresaTransportadora> empresasDisponiveis (Encomenda e){
        Set<EmpresaTransportadora> eSet = new TreeSet<>();
        int size = 0;
        for(String s: this.getEmpresas().keySet()){
            EmpresaTransportadora et = new EmpresaTransportadora(this.getEmpresas().get(s));
            if(et.isAvailable(this.getLojas().get(e.getCodigoLoja()).getLocation())){
                eSet.add(et);
                size++;
            }
        }
        List<EmpresaTransportadora> etl = new ArrayList<>(size);
        int i = 0;
        for(EmpresaTransportadora ets: eSet){
            etl.add(i,ets);
            i++;
        }

        return etl;
    }

    /** Gets the top tem users with the most purchases.
     *
     * @return LinkedMap which the keys represent the codes of the top users and the values the number os packages.
     */
    public Map<String,Integer> topTenUser(){
        Map<String,Integer> reorder = new HashMap<>();
        for (Map.Entry<String, User> entry : this.userCatalog.entrySet()) {
            reorder.put(entry.getKey(),entry.getValue().getEncomendas().size());
        }
        Map<String,Integer> result = new LinkedHashMap<>(10);
        for(int i = 0; i<10 ; i++) {
            int top = -1;
            String codUser = "";
            for (Map.Entry<String, Integer> entry : reorder.entrySet()) {
                if(entry.getValue() > top){
                    top = entry.getValue();
                    codUser = entry.getKey();
                }
            }
            result.put(codUser,top);
            reorder.remove(codUser);
        }
        return result;
    }

    /** Gets the top tem Transporting companies with most distance traveled.
     *
     * @return LinkedMap which the keys represent the codes of the top companies and the values the distance.
     */
    public Map<String,Double> topTenTrans(){
        Map<String,Double> reorder = new HashMap<>();
        for (Map.Entry<String, EmpresaTransportadora> entry : this.empresas.entrySet()) {
            if(reorder.containsKey(entry.getKey())){
                reorder.replace(entry.getKey(),reorder.get(entry.getKey())+entry.getValue().getDistanciaPercorrida());
            }else{
                reorder.put(entry.getKey(),entry.getValue().getDistanciaPercorrida());
            }
        }
        Map<String,Double> result = new LinkedHashMap<>(10);
        for(int i = 0; i<10; i++) {
            double top = -1;
            String codUser = "";
            for (Map.Entry<String, Double> entry : reorder.entrySet()) {
                if(entry.getValue() > top){
                    top = entry.getValue();
                    codUser = entry.getKey();
                }
            }
            result.put(codUser,top);
            reorder.remove(codUser);
        }
        return result;
    }

    /** Builds a list with all packages made by the user.
     *
     * @param codeUser - user
     * @return list with all packages made by the user.
     */
    public List<Encomenda> encByUser(String codeUser){
        Set<Encomenda> set = new TreeSet<>();
        Map<String,Encomenda> newM = this.getGestor().getGestor();
        for(String s: newM.keySet()){
            if(newM.get(s).getCodigoUser().equals(codeUser)){
                set.add(newM.get(s));
            }
        }
        int size = set.size();
        List<Encomenda> l = new ArrayList<>(size);
        int i = 0;
        for(Encomenda s: set){
            l.add(i,s);
        }
        return l;
    }
}
