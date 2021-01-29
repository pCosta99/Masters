package Model;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que junta os módulos de dados da aplicação
 */
public class Sistema implements Serializable {

    private Map <String,Login> logins; //Key -> email
    private Map<String,Transporte> transportes;
    private Map<String,Utilizador> utilizadores;
    private Map<String,Loja> lojas;
    private int ENC_NUM = 1;

    /**
     * Construtor da classe
     */
    public Sistema() {
        this.logins = new HashMap<>();
        this.transportes = new HashMap<>();
        this.utilizadores = new HashMap<>();
        this.lojas = new HashMap<>();
    }

    /**
     * Construtor da classe
     * @param s Um sistema do qual se pretende extrair a informação
     */
    public Sistema(Sistema s){
        this.logins = s.getLogins();
        this.transportes = s.getTransportes();
        this.utilizadores = s.getUtilizadores();
        this.lojas = s.getLoja();
        this.ENC_NUM = s.getENC_NUM();
    }

    /**
     * Indica o número da próxima encomenda a ser registada
     */
    public int getENC_NUM(){
        return this.ENC_NUM;
    }

    /**
     * Indica o mapeamento dos emails dos usuários com os seus respetivos logins
     * @return Uma cópia do mapeamento
     */
    public Map<String,Login> getLogins(){
        return this.logins.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, p->p.getValue().clone()));
    }

    /**
     * Define o mapeamento dos emails dos usuários com os seus respetivos logins
     * @param l O mapeamento que se pretende inserir
     */
    public void setLogin(Map<String,Login> l) {
        this.logins = l.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p->p.getValue().clone()));
    }

    /**
     * Indica o mapeamento dos códigos dos transportes com as suas respetivas informações
     * @return Uma cópia do mapeamento
     */
    public Map<String,Transporte> getTransportes(){
        return this.transportes.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p->p.getValue().clone()));
    }

    /**
     * Define o mapeamento dos códigos dos transportes com as suas respetivas informações
     * @param u O mapeamento que se pretende inserir
     */
    public void setTransportes(Map<String,Transporte> u) {
        this.transportes = u.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p->p.getValue().clone()));
    }

    /**
     * Indica o mapeamento dos códigos dos utilizadores com as suas respetivas informações
     * @return Uma cópia do mapeamento
     */
    public Map<String,Utilizador> getUtilizadores(){
        return this.utilizadores.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p->p.getValue().clone()));
    }

    /**
     * Define o mapeamento dos códigos dos utilizadores com as suas respetivas informações
     * @param u O mapeamento que se pretende inserir
     */
    public void setUtilizadores(Map<String,Utilizador> u) {
        this.utilizadores = u.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p->p.getValue().clone()));
    }

    /**
     * Indica o mapeamento dos códigos das lojas com as suas respetivas informações
     * @return Uma cópia do mapeamento
     */
    public Map<String,Loja> getLoja(){
        return this.lojas.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p->p.getValue().clone()));
    }

    /**
     * Define o mapeamento dos códigos das lojas com as suas respetivas informações
     * @param u O mapeamento que se pretende inserir
     */
    public void setLoja(Map<String,Loja> u) {
        this.lojas = u.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p->p.getValue().clone()));
    }

    /**
     * Cria um clone de um sistema
     * @return O clone do sistema
     */
    public Sistema clone(){return new Sistema(this);}

    /**
     * Adiciona um login ao sistema
     * @param l O login a ser adicionado
     */
    public void addLogin(Login l){
        this.logins.put(l.getEmail(), l);
    }

    /**
     *Adiciona um usuário ao sistema
     * @param x O usuário a ser adicionado
     */
    public void addUser(LocalCodeName x){
        switch (x.getClass().getSimpleName()) {

            case "Utilizador":
                Utilizador u = (Utilizador) x;
                this.utilizadores.put(u.getCode(), u);
                break;

            case "Loja":
                Loja l = (Loja) x;
                this.lojas.put(l.getCode(), l);
                break;

            default:
                Transporte t = (Transporte) x;
                this.transportes.put(t.getCode(), t);

        }
    }

    /**
     * Verifica se as credenciais inseridas correspondem às registadas no sistema
     * @param email O email inserido
     * @param password A password inserida
     * @return True caso afirmativo e false caso contrário
     */
    public boolean checksCredentials(String email, String password){
        if(logins.containsKey(email)) return this.logins.get(email).checksLoginEmail(email, password);
        return false;
    }

    /**
     * Indica o user associado a um dado email
     * @param email O email do user
     * @return Um clone do objeto associado a esse user
     */
    public LocalCodeName whatIs(String email){

        String cod;
        LocalCodeName x;

        if(logins.containsKey(email)){

            cod = logins.get(email).getCodigo();

            switch(cod.charAt(0)){
                case 'u':
                    x = utilizadores.get(cod).clone();
                    break;

                case 'l':
                    x = lojas.get(cod).clone();
                    break;

                default:
                    x = transportes.get(cod).clone();

            }

            return x;
        }

        else return null;

    }

       /**
     * Indica os 10 utilizadores que mais utilizaram o sistema tendo em conta o número de encomendas transportadas
     *
     * @return Um Set com os 10 utilizadores que mais utilizaram o sistema
     */
    public List<Utilizador> top10U() {
        Comparator<Utilizador> temp = (Utilizador a , Utilizador b) -> b.nEncsBought() - a.nEncsBought();
        Comparator<Utilizador> temp2 = temp.thenComparing(LocalCodeName::getCode);
        Set<Utilizador> u = new TreeSet<>(temp2);
        for (Map.Entry<String, Utilizador> aux : this.utilizadores.entrySet()) {
            u.add(aux.getValue().clone());
        }

        List<Utilizador> res = new ArrayList<>(u);

        if (res.size() > 10) {
            res = res.subList(0, 10);
        }

        return res;
    }

    /**
     * Indica as 10 empresas transportadoras que mais utilizaram o sistema tendo em conta o número de kms percorridos
     *
     * @return Um Set com as 10 empresas transportadoras que mais utilizaram o sistema
     */
    public List<Transportadora> top10T() {
        Comparator<Transportadora> temp = (Transportadora a , Transportadora b) ->  Double.compare(b.getTotal_km(),a.getTotal_km()) ;
        Comparator<Transportadora> temp2 = temp.thenComparing(LocalCodeName::getCode);
        TreeSet<Transportadora> r = new TreeSet<>(temp2);
        for (Map.Entry<String, Transporte> aux : this.transportes.entrySet()) {

            if (aux.getValue() instanceof Transportadora) {
                Transportadora t = (Transportadora) aux.getValue();
                r.add(t.clone());
            }

        }

        List<Transportadora> res = new ArrayList<>(r);

        if (res.size() > 10) {
            res = res.subList(0, 10);
        }

        return res;
    }

    /**
     * Indica se um email está registado no sistema
     * @param email O email que se pretende procurar
     * @return True caso afirmativo e false caso contrário
     */
    public boolean existsEmail(String email){return this.logins.containsKey(email);}

    /**
     * Indica o cliente associado a um dado código
     * @param codCliente O código do cliente
     * @return O cliente correspondente ao código
     */
    public Utilizador getUtilizador(String codCliente){
        return this.utilizadores.get(codCliente).clone();
    }

    /**
     * Indica o voluntário associado a um dado código
     * @param codVoluntario O código do voluntário
     * @return O voluntário correspondente ao código
     */
    public Voluntario getVoluntario(String codVoluntario){
        return (Voluntario) this.transportes.get(codVoluntario).clone();
    }

    /**
     * Indica a transportadora associada a um dado código
     * @param codTransportadora O código da transportadora
     * @return A transportadora correspondente ao código
     */
    public Transportadora getTransportadora(String codTransportadora){
        return (Transportadora) this.transportes.get(codTransportadora).clone();
    }

    /**
     * Indica a loja associada a um dado código
     * @param codLoja O código da loja
     * @return A loja correspondente ao código
     */
    public Loja getLoja(String codLoja){
        return this.lojas.get(codLoja).clone();
    }

    /**
     * Recomenda transportes tendo por base a sua localização e disponibilidade
     * @param gps As coordenadas do utilizador
     * @return Os transportes recomendados ou null caso não tenha transportes disponíveis
     */
    public Set<Transporte> recomendaTrans(GPS gps, boolean encMedica){

        Transporte taux;
        Set<Transporte> res = new HashSet<>();

        if(!encMedica) {
            for (Map.Entry<String, Transporte> aux : this.transportes.entrySet()) {
                taux = aux.getValue();
                if (gps.distGPSKms(taux.getGps()) < taux.getRaio()
                        && taux.isDisp()) {
                    res.add(taux.clone());
                }
            }
        }
        else{
            for (Map.Entry<String, Transporte> aux : this.transportes.entrySet()) {
                taux = aux.getValue();
                if (gps.distGPSKms(taux.getGps()) < taux.getRaio()
                        && taux.isDisp() && taux.aceitoTransporteMedicamentos()) {
                    res.add(taux.clone());
                }
            }
        }

        return res;

    }

    /**
     * Indica a loja mais perto
     * @param gps As coordenadas do utilizador
     * @return A loja mais perto
     */
    public Loja lojaMaisPerto(GPS gps){

        double min = Double.MAX_VALUE;
        Loja l = null, lAux;

        for(Map.Entry<String,Loja> aux : this.lojas.entrySet()){
            lAux = aux.getValue();
            if(gps.distGPSKms(lAux.getGps()) < min){
                l = aux.getValue();
            }
        }

        return l.clone();
    }

    /**
     * Adiciona uma encomenda a uma loja
     * @param l A loja à qual se pretende adicionar a encomenda
     * @param e A encomenda que se pretende adicionar
     */
    public void addEncLoja(Loja l, Encomenda e){
        this.lojas.get(l.getCode()).addEncomenda(e.clone());
    }

    /**
     * Adiciona um cliente à fila de uma loja
     * @param l A loja à qual se pretende adicionar o transporte
     * @param t O transporte que se pretende adicionar
     * @return True caso o transporte tenha sido adicionado à fila e false caso contrário
     */
    public boolean addTrans(String l, Transporte t){
        return this.lojas.get(l).adicionaCliente(t.clone());
    }

    /**
     * Retira uma encomenda a uma loja
     * @param l A loja à qual se pretende retirar a encomenda
     * @param e A encomenda que se pretende retirar
     */
    public void retiraEncLoja(Loja l, String e){this.lojas.get(l.getCode()).retiraEncomenda(e);}

    /**
     * Adiciona uma encomenda a um utilizador
     * @param u O utilizador ao qual se pretende adicionar a encomenda
     * @param e A encomenda que se pretende adicionar
     */
    public void addEncUtilizador(Utilizador u, Encomenda e){
        this.utilizadores.get(u.getCode()).addEnc(e.clone());
    }

    /**
     * Indica as encomendas já recebidas por um dado utilizador
     * @param codUtilizador O código do utilizador
     * @return Uma lista das encomendas já recebidas
     */
    public List<Encomenda> encsUtilizador (String codUtilizador) {

        List<Encomenda> res = new ArrayList<>();
        Map<String,Encomenda> encs = this.utilizadores.get(codUtilizador).getEncomendaMap();

        for(Map.Entry<String,Encomenda> aux : encs.entrySet()){
            res.add(aux.getValue());
        }

        return res;

    }

    /**
     * Adiciona uma classificação a um transporte
     * @param t O transporte ao qual se pretende adicionar a classificação
     * @param classificacao A classificação a ser adicionada
     */
    public void classificaTrans(Transporte t, double classificacao){
        this.transportes.get(t.getCode()).addRate(classificacao);
    }

    /**
     * Passa uma encomenda por preparar para as encomendas prontas
     *
     * @param cod O código da loja na qual se pretende fazer a alteração
     * @return Encomenda pronta
     */
    public Encomenda transfereEncPronta(String cod) {
        return this.lojas.get(cod).adicionaEncPronta();
    }
    /**
     * Indica as encomendas por preparar de uma loja
     * @param cod O código da loja
     * @return As encomendas por preparar
     */
    public Queue<Encomenda> encsNotReady(String cod){
        return this.lojas.get(cod).getEncs();
    }

    /**
     * Indica as encomendas prontas de uma loja
     * @param cod O código da loja
     * @return As encomendas prontas
     */
    public List <Encomenda> encsReady(String cod){

        List<Encomenda> res = new ArrayList<>();

        for(Map.Entry<String, Encomenda> aux : this.lojas.get(cod).getEncsProntas().entrySet()){
            res.add(aux.getValue());
        }

        return res;
    }

    /**
     * Atribui um código a uma encomenda e adiciona-a à loja mais próxima do utilizador
     * @param e A encomenda a ser direcionada
     */
    public Encomenda novaEncomenda(Encomenda e){

        //Atribui código
        e.setNumEnc("e" + ENC_NUM);
        ENC_NUM++;

        //Calcula a loja mais próxima
        double min = Double.MAX_VALUE;
        double temp;
        Loja l = null;
        Utilizador u = this.utilizadores.get(e.getNome());

        for(Loja aux : this.lojas.values()){
            temp = u.getGps().distGPSKms(aux.getGps()) + aux.tempoDeEspera();
            if(temp < min){
                l = aux;
                min = temp;
            }
        }

        e.setLoja(l.getCode());

        Transporte t = this.transportes.get(e.getTransporte().getCode());

        if(t instanceof Transportadora){
            Transportadora trans = (Transportadora) t;
            double distTotal = t.getGps().distGPSKms(l.getGps()) + t.getGps().distGPSKms(u.getGps());
            double precoTotal = e.geraPreco() + (distTotal * trans.getPrec_km()) + l.tempoDeEspera()*0.1;
            e.setPreco(precoTotal);
        }else{
            e.geraPreco();
        }

        //Adiciona encomenda à loja
        this.lojas.get(l.getCode()).adicionaEncomenda(e);

        //Adiciona encomenda ao utilizador
        this.utilizadores.get(e.getNome()).addEnc(e);

        return e;

    }

    /**
     * Altera as coordenadas de um transporte
     * @param x O transporte do qual se pretende alterar as coordenadas
     * @param gps As novas coordenadas
     */
    public void alteraGPS(Transporte x, GPS gps) {
        x.setGps(gps);
    }

    /**
     * Adiciona uma encomenda a uma transportadora
     * @param t A transportadora à qual se pretende adicionar a encomenda
     * @param e A lista das encomendas a serem adicionadas
     * @return True em caso de sucesso e false caso contrário
     */
    public boolean addEncTrans(Transportadora t, List<Encomenda> e) {
        if(t.isDisp()) {
            t.addEncomendas(e);
            return true;
        }
        return false;
    }

    /**
     * Adiciona uma encomenda a um voluntário
     * @param v O voluntário ao qual se pretende adicionar a encomenda
     * @param e A encomenda que se pretende adicionar
     * @return True em caso de sucesso e false caso contrário
     */
    public boolean addEncTrans(Voluntario v, Encomenda e) {
        if(v.isDisp()) {
            v.setParaEntregar(e.clone());
            return true;
        }
        return false;
    }

    /**
     * Altera a disponibilidade de um transporte
     * @param t O transporte que se pretende alterar
     */
    public void changeDisp(Transporte t) {
        t.trocaDisp();
    }

    /**
     * Indica as encomendas por entregar
     * @param e A transportadora da qual se pretende recolher a informação
     * @return As encomendas por entregar
     */
    public Map<String, Set<Encomenda>> porEntregar(Transportadora e) {
        Map<String,Encomenda> temp = e.getFuturasEnc();

        Map<String,Set<Encomenda>> ret = new HashMap<>();

        for(Encomenda enc : temp.values()) {
            if(ret.containsKey(enc.getNome())) ret.get(enc.getNome()).add(enc);
            else {
                Set<Encomenda> set = new HashSet<>();
                set.add(enc);
                ret.put(enc.getNome(),set);
            }
        }

        return ret;
    }

    /**
     * Regista data de entrega de uma dada encomenda
     * @param e A encomenda à qual se vai definir a data de entrega
     * @param data A data de entrega a ser definida
     */
    public void registaDataEntrega(Encomenda e, LocalDateTime data){
        e.setDataEntrega(data);
    }

    /**
     * Altera o estado de aceitação de medicamentos
     * @param t O transporte que se pretende alterar
     */
    public void alteraAceitacao(Transporte t){
        Transporte trans = this.transportes.get(t.getCode());
        trans.aceitaMedicamentos(!trans.aceitoTransporteMedicamentos());
    }

    /**
     * Indica se um transporte aceita ou não encomendas especiais
     * @param t O transporte que se pretende alterar
     * @return True caso afirmativo e false caso contrário.
     */
    public boolean fazEncomendasEspecias(Transporte t){
        return this.transportes.get(t.getCode()).aceitoTransporteMedicamentos();
    }

    /**
     * Adiciona uma encomenda para entregar a uma transportadora
     * @param t A transportadora à qual se pretende adicionar a encomenda
     * @param e A encomenda que se pretende adicionar
     */
    public void addEncTrans(Transportadora t, Encomenda e){
        Transportadora trans = (Transportadora) this.transportes.get(t.getCode());
        trans.addEncomenda(e);
    }

    /**
     * Indica o tempo de espera numa dada loja
     * @param l A loja da qual se pretende obter a informação
     * @return O tempo de espera na loja dada
     */
    public double esperaNaLoja(String l){
        return this.lojas.get(l).tempoDeEspera();
    }

}
