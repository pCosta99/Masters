import jdk.jshell.execution.Util;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

public class Model implements Serializable{
    /** variaveis de instancia */
    private Map<String,Utilizador> utlizadores;
    private Map<String,Voluntario> voluntarios;
    private Map<String,Transportadora> transportadoras;
    private Map<String,Loja> lojas;
    private Map<String,Encomenda> encomendas;
    private List<String> aceites;
    private Map<String,DadosEntrega> pendentes;
    private Map<Encomenda, Double> entregues;

    /** constructores de classe */
        /** vazio */
        public Model(){
            this.utlizadores = new HashMap<>();
            this.voluntarios = new HashMap<>();
            this.transportadoras = new HashMap<>();
            this.lojas = new HashMap<>();
            this.encomendas = new HashMap<>();
            this.aceites = new ArrayList<>();
            this.pendentes = new HashMap<>();
            this.entregues = new HashMap<>();
        }

        /** parametrico */

        /** copia */

    /** gets/sets das variaveis de instancia */
    public Set<Utilizador> getUtlizadores() {
        return this.utlizadores.values().stream().map(Utilizador::clone).collect(Collectors.toSet());
    }
    public void setUtlizadores(Set<Utilizador> utilizadores) {
        for(Utilizador u : utilizadores)
            this.utlizadores.put(u.getCodUtilizador(),u.clone());
    }

    public Set<Voluntario> getVoluntarios() {
        return this.voluntarios.values().stream().map(Voluntario::clone).collect(Collectors.toSet());
    }
    public void setVoluntarios(Set<Voluntario> voluntarios) {
        for(Voluntario v : voluntarios)
            this.voluntarios.put(v.getCodVoluntario(),v.clone());
    }

    public Set<Transportadora> getTransportadoras() {
        return this.transportadoras.values().stream().map(Transportadora::clone).collect(Collectors.toSet());
    }
    public void setTransportadoras(Set<Transportadora> transportadoras) {
        for(Transportadora t : transportadoras)
            this.transportadoras.put(t.getCodTransportadora(),t.clone());
    }

    public Set<Loja> getLojas() {
        return this.lojas.values().stream().map(Loja::clone).collect(Collectors.toSet());
    }
    public void setLojas(Set<Loja> lojas) {
        for(Loja l : lojas)
            this.lojas.put(l.getCodLoja(),l.clone());
    }

    public Set<Encomenda> getEncomendas() {
        return this.encomendas.values().stream().map(Encomenda::clone).collect(Collectors.toSet());
    }
    public void setEncomendas(Set<Encomenda> encomendas) {
        for(Encomenda e : encomendas)
            this.encomendas.put(e.getCodEncomenda(),e.clone());
    }

    public List<String> getAceites() { return new ArrayList<>(this.aceites); }
    public void setAceites(List<String> aceites) { this.aceites = new ArrayList<>(aceites); }

    public Map<String,DadosEntrega> getPendentes() {
        Map<String,DadosEntrega> res = new HashMap<>();
        for(Map.Entry<String,DadosEntrega> x : this.pendentes.entrySet()){
            res.put(x.getKey(),x.getValue().clone());
        }
        return res;
    }
    public void setPendentes(Map<String,DadosEntrega> pendentes) {
        for(Map.Entry<String,DadosEntrega> x : pendentes.entrySet()){
            this.pendentes.put(x.getKey(),x.getValue().clone());
        }
    }

    public Map<Encomenda, Double> getEntregues() {
        Map<Encomenda, Double> res = new HashMap<>();
        for(Map.Entry<Encomenda, Double> x : this.entregues.entrySet()){
            res.put(x.getKey(),x.getValue());
        }
        return res;
    }
    public void setEntregues(Map<Encomenda, Double> entregues) {
        for(Map.Entry<Encomenda, Double> x : entregues.entrySet()){
            this.entregues.put(x.getKey(),x.getValue());
        }
    }

    /** metodos override */
    @Override
    public String toString() {
        return  this.utlizadores.toString() + '\n' +
                this.voluntarios.toString() + '\n' +
                this.transportadoras.toString() + '\n' +
                this.lojas.toString() + '\n' +
                this.encomendas.toString() + '\n' +
                this.aceites.toString();
    }

    /** metodos especificos */
    /**
     * insere um Utilizador na lista de utilizadores
     */
    public void insereUtilizador(Utilizador newUtilizador){
        this.utlizadores.put(newUtilizador.getCodUtilizador(),newUtilizador.clone());
    }

    /**
     * devolve um Utilizador da lista de utilizadores pelo seu codigo
     */
    public Utilizador devolveUtilizador(String codUtilizador){
        return this.utlizadores.get(codUtilizador).clone();
    }

    /**
     * insere um Voluntario na lista de voluntario
     */
    public void insereVoluntario(Voluntario newVoluntario){
        this.voluntarios.put(newVoluntario.getCodVoluntario(),newVoluntario.clone());
    }

    /**
     * devolve um Voluntario da lista de Voluntarios pelo seu codigo
     */
    public Voluntario devolveVoluntario(String codVoluntario){
        return this.voluntarios.get(codVoluntario).clone();
    }

    /**
     * insere uma Transportadora na lista de Transportadoras
     */
    public void insereTransportadora(Transportadora newTransportadora){
        this.transportadoras.put(newTransportadora.getCodTransportadora(),newTransportadora.clone());
    }

    /**
     * devolve um Transportadora da lista de Transportadoras pelo seu codigo
     */
    public Transportadora devolveTransportadora(String codTransportadora){
        return this.transportadoras.get(codTransportadora).clone();
    }

    /**
     * insere uma Loja na lista de Lojas
     */
    public void insereLoja(Loja newLoja){
        this.lojas.put(newLoja.getCodLoja(),new Loja(newLoja));
    }

    /**
     * devolve uma Loja da lista de Lojas pelo seu codigo
     */
    public Loja devolveLoja(String codLoja){
        return this.lojas.get(codLoja).clone();
    }

    /**
     * insere uma Encomenda na lista de Encomendas
     */
    public void insereEncomenda(Encomenda newEncomenda){
        this.encomendas.put(newEncomenda.getCodEncomenda(),new Encomenda(newEncomenda));
    }

    /**
     * insere uma Encomenda na lista de Encomendas Aceites
     */
    public void insereAceite(String newEncomendaRef){
        this.aceites.add(newEncomendaRef);
    }


    /**
     * aceder à informação das entregas efectuadas num determinado período e por voluntário
     * ou transportador
     */
    public List<Encomenda> verEstadoEncomendas(String codUtilizador){
        return encomendas.values().stream()
                .filter(e -> e.getUtilizador()
                        .equals(codUtilizador)).collect(Collectors.toList());
    }

    /**
     * gerar a lista de encomendas pendentes de um utilizador
     * @param utilizador que se pretende ver
     */
    public List<DadosEntrega> verEncomendasPendentes(Utilizador utilizador){
        List<DadosEntrega> res = new ArrayList<>();
        for(Map.Entry<String,DadosEntrega> x : this.pendentes.entrySet()){
            if(this.encomendas.get(x.getKey()).getUtilizador().equals(utilizador))
                res.add(x.getValue());
        }
        return res;
    }


    /**
     * gerar a lista de encomendas que podem ser efetuadas por uma transportadora
     */

    public List<Encomenda> verEncomendasValidas(String codEmpresa){
        List<Encomenda> res = new ArrayList<>();
        Transportadora t = this.transportadoras.get(codEmpresa).clone();

        for(Encomenda e : this.encomendas.values()){
            if(t.podeEntregar(e)){
                res.add(e.clone());
            }
        }

        return res;
    }

    /**
     * valida login de usuario
     * @param email do usuario
     * @param pass do usuario
     * @param tipoUsuario sendo 1 - Utilizador, 2 - Voluntário, 3 - Transportadora, 4 - Loja
     * @return codigo do usuario ou vazio
     */
    public String validaLogin(String email, String pass, char tipoUsuario){
        String res = "";
        switch (tipoUsuario){
            case '1':
                for(Utilizador u : this.utlizadores.values()){
                    if(u.getEmail().equals(email) && u.getPassword().equals(pass)){
                        res = u.getCodUtilizador();
                        break;
                    }
                }
                break;
            case '2':
                for(Voluntario v : this.voluntarios.values()){
                    if(v.getEmail().equals(email) && v.getPassword().equals(pass)){
                        res = v.getCodVoluntario();
                        break;
                    }
                }
                break;
            case '3':
                for(Transportadora t : this.transportadoras.values()){
                    if(t.getEmail().equals(email) && t.getPassword().equals(pass)){
                        res = t.getCodTransportadora();
                        break;
                    }
                }
                break;
            case '4':
                for(Loja l : this.lojas.values()){
                    if(l.getEmail().equals(email) && l.getPassword().equals(pass)){
                        res = l.getCodLoja();
                        break;
                    }
                }
                break;
            default:
                break;
        }
        return res;
    }

    /**
     * valida registo de usuario
     */
    public String validaRegisto(String email, String pass, char tipoUsuario){
        String res = "";
        switch (tipoUsuario){
            case '1':
                if(this.utlizadores.values().stream()
                .map(Utilizador::getEmail)
                .anyMatch(e->e.equals(email))){
                    break;
                }else{
                    Utilizador u = new Utilizador(email, pass);
                    this.insereUtilizador(u);
                    res = u.getCodUtilizador();
                }
                break;
            case '2':
                if(this.voluntarios.values().stream()
                        .map(Voluntario::getEmail)
                        .anyMatch(e->e.equals(email))){
                    break;
                }else{
                    Voluntario v = new Voluntario(email, pass);
                    this.insereVoluntario(v);
                    res = v.getCodVoluntario();
                }
                break;
            case '3':
                if(this.transportadoras.values().stream()
                        .map(Transportadora::getEmail)
                        .anyMatch(e->e.equals(email))){
                    break;
                }else{
                    Transportadora t = new Transportadora(email, pass);
                    this.insereTransportadora(t);
                    res = t.getCodTransportadora();
                }
                break;
            case '4':
                if(this.lojas.values().stream()
                        .map(Loja::getEmail)
                        .anyMatch(e->e.equals(email))){
                    break;
                }else{
                    Loja l = new Loja(email, pass);
                    this.insereLoja(l);
                    res = l.getCodLoja();
                }
                break;
            default:
                break;
        }
        return res;
    }

    /**
     * valida existencia de loja
     */
    public boolean lojaExiste(String nomeLoja){
        for(Loja l : this.lojas.values()){
            if(l.getNome().toUpperCase().equals(nomeLoja.toUpperCase()))
                return true;
        }
        return false;
    }


    /**
     * propor uma entrega de uma encomenda
     */
    public void proporEntregaT(String codEncomenda,Transportadora t){
        DadosEntrega de = t.proporEntrega(this.encomendas.get(codEncomenda));
        this.pendentes.put(codEncomenda,de.clone());
    }

    /**
     * remover uma entrega da lista de pendentes
     */
    public void removeEntregaPendente(String codEncomenda){
        this.pendentes.remove(codEncomenda);
    }

    public void voluntarioPodeEntregar(String codVol){
        this.voluntarios.get(codVol).setDisponibilidade(true);
    }

    public Encomenda voluntarioVaiALoja(String codVol){
        Voluntario vol = this.voluntarios.get(codVol);
        Encomenda enc = this.encomendas.values().stream().filter(e -> !this.aceites.contains(e.getCodEncomenda())
                && vol.getGps().distancia(e.getUtilizador().getGps()) <= vol.getRaio()
                && vol.getGps().distancia(e.getLoja().getGps()) <= vol.getRaio()
                && e.getProntaASerLevantada()).findAny().orElse(null);

        if (enc != null) {
            this.aceites.add(enc.getCodEncomenda());
        }

        return enc;
    }

    public double entregarEncomenda(String codEnc, String codVol){
        Voluntario vol = this.voluntarios.get(codVol);
        Encomenda enc = this.encomendas.get(codEnc);
        double temp = vol.entregarEncomenda(enc);
        this.encomendas.remove(codEnc);
        this.entregues.put(enc, temp);

        return temp;
    }

    /**
     * Gravar o ficheiro de logs
     */
    public void gravarLog() throws IOException {
        ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream("logs.dat"));

        o.writeObject(this);
        o.flush();
        o.close();
    }

    /**
     * Ler o ficheiro de logs
     */
    public static Model lerLog() throws IOException, ClassNotFoundException {
        ObjectInputStream o = new ObjectInputStream(new FileInputStream("logs.dat"));

        Model model = (Model) o.readObject();
        o.close();
        return model;
    }

    /**
     * Loja sinaliza que encomenda esta pronta a ser levantada
     */

    public void sinalizaPodeSerLevantada(String codEnc){
        this.encomendas.get(codEnc).setProntaASerLevantada(true);
    }
}
