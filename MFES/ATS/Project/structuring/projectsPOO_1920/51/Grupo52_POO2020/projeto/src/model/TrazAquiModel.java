package model;

import exceptions.EmailJaExisteException;

import java.io.*;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class TrazAquiModel implements Serializable{
    private Utilizador admin;
    private Map<String, Utilizador> utilizadores; // email -> utilizador
    private Map<String, Voluntario> voluntarios; // email -> voluntario
    private Map<String, Empresa> empresas; // email -> empresa
    private Map<String, Loja> lojas; // email -> loja
    private Map<String, Encomenda> encomendas; //// codEncomenda -> encomenda

    private Set<Entidade> disponiveis;

    private int nUtilizadores; //inteiro usado para atribuir código de utilizador
    private int nVoluntarios;
    private int nEmpresas;
    private int nLojas;
    private int nEncomendas;

    public TrazAquiModel(){
        this.admin = new Utilizador("admin@uminho.pt", "admin", "Admin", null, 0);
        this.utilizadores = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.empresas = new HashMap<>();
        this.lojas = new HashMap<>();
        this.encomendas = new HashMap<>();
        this.nUtilizadores = 0;
        this.nVoluntarios = 0;
        this.nEmpresas = 0;
        this.nLojas = 0;
    }

    public int getnUtilizadores() {
        return this.nUtilizadores;
    }

    public void setnUtilizadores(int nUtilizadores) {
        this.nUtilizadores = nUtilizadores;
    }

    public int getnVoluntarios() {
        return this.nVoluntarios;
    }

    public void setnVoluntarios(int nVoluntarios) {
        this.nVoluntarios = nVoluntarios;
    }

    public int getnEmpresas() {
        return this.nEmpresas;
    }

    public void setnEmpresas(int nEmpresas) {
        this.nEmpresas = nEmpresas;
    }

    public int getnLojas() {
        return this.nLojas;
    }

    public void setnLojas(int nLojas) {
        this.nLojas = nLojas;
    }

    public int getnEncomendas() {
        return this.nEncomendas;
    }

    public void setnEncomendas(int nEncomendas) {
        this.nEncomendas = nEncomendas;
    }

    public void guardaEstado() throws FileNotFoundException, IOException {
        FileOutputStream fos = new FileOutputStream("estado.obj");
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }


    public static TrazAquiModel carregaEstado() throws FileNotFoundException, IOException, ClassNotFoundException {
        FileInputStream fis = new FileInputStream("estado.obj");
        ObjectInputStream ois = new ObjectInputStream(fis);
        TrazAquiModel new_model = (TrazAquiModel) ois.readObject();
        ois.close();
        return new_model;
    }


    public boolean checkLogin(Integer tipo, String email, String password){
        boolean valid = false;
        switch (tipo) {
            case 1:
                if(this.admin.getEmail().equals(email) && this.admin.getPassword().equals(password)) valid = true;
                break;
            case 2:
                if(this.utilizadores.containsKey(email) && this.utilizadores.get(email).getPassword().equals(password)) valid = true;
                break;
            case 3:
                if(this.voluntarios.containsKey(email) && this.voluntarios.get(email).getPassword().equals(password)) valid = true;
                break;
            case 4:
                if(this.lojas.containsKey(email) && this.lojas.get(email).getPassword().equals(password)) valid = true;
                break;
            case 5:
                if(this.empresas.containsKey(email) && this.empresas.get(email).getPassword().equals(password)) valid = true;
                break;
            default:
                break;
        }
        return valid;
    }



    public void addUtilizador(Utilizador utilizador) throws EmailJaExisteException {
        if(! utilizadores.containsKey(utilizador.getEmail())) utilizadores.put(utilizador.getEmail(), utilizador.clone());
        else throw new EmailJaExisteException();
    }

    public void addVoluntario(Voluntario voluntario) throws EmailJaExisteException {
        if(! voluntarios.containsKey(voluntario.getEmail())) voluntarios.put(voluntario.getEmail(), voluntario.clone());
        else throw new EmailJaExisteException();
    }

    public void addEmpresa(Empresa empresa) throws EmailJaExisteException {
        if(! empresas.containsKey(empresa.getEmail())) empresas.put(empresa.getEmail(), empresa.clone());
        else throw new EmailJaExisteException();
    }

    public void addLoja(Loja loja) throws EmailJaExisteException {
        if(! lojas.containsKey(loja.getEmail())) lojas.put(loja.getEmail(), loja.clone());
        else throw new EmailJaExisteException();
    }

    public void addEncomenda(Encomenda encomenda)  {
        if(!encomendas.containsKey(encomenda.getCodEncomenda()))
            encomendas.put(encomenda.getCodEncomenda(), encomenda.clone());
    }


    public void addEncomendaAceite(String codEncomenda) {
        Encomenda encomenda = getEncomendaC(codEncomenda);

        if(encomenda == null) return;

        Utilizador u = getUtilizadorC(encomenda.getCodUtilizador());
        Loja l = getLojaC(encomenda.getCodLoja());

        if(u == null || l == null) return;

        Voluntario v= temVoluntario(encomenda);

        if(v == null){
            u.addToStandby(encomenda);
            l.addToQueue(encomenda);
        }else{
            encomenda.setData(LocalDateTime.now());
            encomenda.setCodEntidade_transportadora(v.getCodVoluntario());
            v.addEncomendaPorSinalizar(encomenda);
            u.addEncomendaEntregue(encomenda);
            l.addToAceites(encomenda);
        }
    }

    public Utilizador getUtilizador(String email){ return this.utilizadores.get(email); }

    public Utilizador getUtilizadorC(String codUtilizador){
        for(Utilizador u : this.utilizadores.values()){
            if(u.getCodUtilizador().equals(codUtilizador)) return u;
        }
        return null;
    }
    public Voluntario getVoluntario(String email){ return this.voluntarios.get(email); }

    public Voluntario getVoluntarioC(String codVoluntario){
        for(Voluntario v : this.voluntarios.values()){
            if(v.getCodVoluntario().equals(codVoluntario)) return v;
        }
        return null;
    }

    public Loja getLoja(String email){ return this.lojas.get(email); }

    public Loja getLojaC(String codLoja){
        for(Loja l : this.lojas.values()){
            if(l.getCodLoja().equals(codLoja)) return l;
        }
        return null;
    }

    public Loja getLojaNome(String nome){
        for(Loja l : this.lojas.values()){
            if(l.getNome().equals(nome)) return l;
        }
        return null;
    }

    public Empresa getEmpresa(String email){ return this.empresas.get(email); }

    public Empresa getEmpresaC(String codEmpresa){
        for(Empresa e : this.empresas.values()){
            if(e.getCodEmpresa().equals(codEmpresa)) return e;
        }
        return null;
    }

    public Encomenda getEncomenda(String cod) {return this.encomendas.get(cod);}

    public Encomenda getEncomendaC(String codEncomenda){
        for(Encomenda e : this.encomendas.values()){
            if(e.getCodEncomenda().equals(codEncomenda)) return e;
        }
        return null;
    }

    public List<Encomenda> getAllEncomendas(String email, int op){
        List<Encomenda> res = null;
        switch(op){
            case 2:
                res = getUtilizador(email).getAllEncomendas();
                break;
            case 3:
                res = getVoluntario(email).allEncomendas();
                break;
            case 4:
                res = getLoja(email).allEncomendas();
                break;
            case 5:
                res = getEmpresa(email).allEncomendas();
                break;
            default:
                break;
        }
        return res;
    }


    public void available(int tipo, String email, boolean state){
        if(tipo == 3) {
            Voluntario v =this.voluntarios.get(email);
            List<Encomenda> le = getEncomendas_Espera_Voluntario();
            v.setDisponivel(state);
            for(int i=0; i<le.size(); i++){
                Encomenda e =le.get(i);
                if (e instanceof EncomendaMedica)
                    if(dentroDoRaio(v,e, true)!=null) v.addEncomendaPorSinalizar(e);
                else if(dentroDoRaio(v,e, false)!=null) v.addEncomendaPorSinalizar(e);
            }
        }
        else this.empresas.get(email).setDisponivel(state);
    }

    public void availableToMed(int tipo, String email, boolean state){
        if(tipo == 3) this.voluntarios.get(email).aceitaMedicamentos(state);
        else this.empresas.get(email).aceitaMedicamentos(state);
    }

    /** Dado um voluntario vê se a encomenda dada se encontra no seu raio
     * Auxiliar da available
     */
    public Encomenda dentroDoRaio(Voluntario v ,Encomenda e,Boolean medica){
        GPS localizacao_voluntario= v.getGps();
        GPS localizacao_loja = getLojaC(e.getCodLoja()).getGps();
        GPS localizacao_u = getUtilizadorC(e.getCodUtilizador()).getGps();
        double dist_lu = localizacao_loja.distancia(localizacao_u);
        double dist_total = dist_lu+ localizacao_voluntario.distancia(localizacao_loja);
        if(v.isDisponivel() && dist_total<=v.getRaio()){
            if(medica && v.aceitoTransporteMedicamentos()) return e;
            else return e;
        }
        return null;
    }
    /** Dada uma encomenda vê se há voluntarios que a possam entregar
     Seleciona o primeiro que encontrar
     */
    public Voluntario temVoluntario(Encomenda e){
        GPS gpsLoja = getLojaC(e.getCodLoja()).getGps();
        GPS gpsU = getUtilizadorC(e.getCodUtilizador()).getGps();

        double dist_lu = gpsLoja.distancia(gpsU);

        for (Voluntario v : voluntarios.values()){
            double dist_total = dist_lu + v.getGps().distancia(gpsLoja);
            if(e instanceof EncomendaMedica && v.isDisponivel() && v.aceitoTransporteMedicamentos() && dist_total<=v.getRaio()){
                e.setDist_total(dist_total);
                return v;
            }
            else if(v.isDisponivel() && dist_total<=v.getRaio()) {
                e.setDist_total(dist_total);
                return v;
            }
        }
        return null;
    }

    /**
     *  Dada uma encomenda vê se há empresas que a possam entregar e associa o custo do transporte
     *  Auxiliar da fazerEncomenda
     */
    public  Map<Empresa, Double> empresasDisponiveis(Encomenda enc){
        Map<Empresa, Double> res= new HashMap<>();

        GPS gpsLoja = getLojaC(enc.getCodLoja()).getGps();
        GPS gpsU = getUtilizadorC(enc.getCodUtilizador()).getGps();
        double dist_lu = gpsLoja.distancia(gpsU);

        for (Empresa e : this.empresas.values()){
            double dist_total = dist_lu+ e.getGps().distancia(gpsLoja);
            Double custo = dist_total * e.getPrecokm();
            if(e.isDisponivel() && dist_total <= e.getRaio()){
                enc.setDist_total(dist_total);
                if(enc instanceof EncomendaMedica && e.isLicenca()) res.put(e, custo);
                else res.put(e, custo);
            }
        }
        return res;
    }

    public List<String> encomendas_por_sinalizar(int tipo, String email){

        if(tipo == 3) return this.voluntarios.get(email).getEncomendas_por_sinalizar().stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList());
        else return this.empresas.get(email).getEncomendas_por_sinalizar().stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList());
    }

    public List<String> toEntrega(String lEmail){
       return this.lojas.get(lEmail).getQueue().stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList());
    }


    /*CLONE*/
    public Map<String, Encomenda> getMEncomenda() {
        Map<String,Encomenda> ne = new HashMap<>();
        encomendas.forEach((key,value)->ne.put(key,value.clone()));
        return ne;
    }

    public Map<String, Loja> getMLoja() {
        Map<String,Loja> nl = new HashMap<>();
        lojas.forEach((key,value)->nl.put(key,value.clone()));
        return nl;
    }

    public Map<String, Utilizador> getMUtilizador() {
        Map<String,Utilizador> nu = new HashMap<>();
        utilizadores.forEach((key,value)->nu.put(key,value.clone()));
        return nu;
    }

    public List<Encomenda> getEncomendas_Espera_Voluntario(){
        List<Encomenda> encomendas_espera_voluntario = new ArrayList<>();
        for(Utilizador u: utilizadores.values()){
           encomendas_espera_voluntario.addAll(u.getEncomendas_Standy());
        }
        return encomendas_espera_voluntario;
    }


    /*para debug */
    public int nUsers() { return this.utilizadores.size(); }

    public int nVolu() {
        return voluntarios.size();
    }

    public int nLojas() {
        return lojas.size();
    }


    public int nEncomendas() {
        return encomendas.size();
    }



    public Set<Utilizador> top10Users(){
        Set<Utilizador> user = new TreeSet<>((o1, o2) -> {
            if(o1.nrEncomendas() > o2.nrEncomendas()) return -1;
            if(o1.nrEncomendas() < o2.nrEncomendas()) return 1;
            return o1.getNome().compareTo(o2.getNome());
        });

        if(this.utilizadores !=null){
            this.utilizadores.values().stream().limit(10).forEach(e->user.add(e.clone()));
        }
        return user;
    }

    public Set<Empresa> top10Empresas(){
        Set<Empresa> top = new TreeSet<>((o1, o2) -> {
            if(o1.totalKms() > o2.totalKms()) return -1;
            if(o1.totalKms() < o2.totalKms()) return 1;
            return o1.getNome().compareTo(o2.getNome());
        });

        if(this.empresas !=null){
            this.empresas.values().stream().limit(10).forEach(e->top.add(e.clone()));
        }
        return top;
    }
}