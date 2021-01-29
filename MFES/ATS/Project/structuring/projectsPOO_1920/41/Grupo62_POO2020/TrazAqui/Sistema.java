import java.util.*;
import java.io.*;
import java.time.*;
import java.util.stream.*;
import java.lang.Math;

public class Sistema implements Serializable
{
    private Map<String, Set<Encomenda>> sistema;
    private Map<String, Entidade> info;
    private Administrador admin;
    
    public Sistema(){
        sistema = new HashMap<>();
        info = new HashMap<>();
        admin = new Administrador();
    }
    
    public Sistema(Map<String, Set<Encomenda>> s, Map<String, Entidade> info, Administrador a){
        this.sistema = new HashMap<>();
        this.sistema.putAll(s);
        this.info = new HashMap<>();
        this.info.putAll(info);
        this.admin = new Administrador(a);
    }
    
    public Sistema(Sistema s){
        sistema = s.getSistema();
        info = s.getInfo();
        admin = s.getAdministrador();
    }
    
    /**
     * Metodo que retorna o sistema das encomendas
     * 
     * @return as Encomendas do sistema
     */
    public Map<String, Set<Encomenda>> getSistema(){
        Map<String, Set<Encomenda>> m = new HashMap<>();
        for(String st: sistema.keySet()){
            Set<Encomenda> s = new HashSet<>();
            for(Encomenda e: sistema.get(st)){
                s.add(e.clone());
            }
            m.put(st, s);
        }
        return m;
    }
    
    /**
     * Metodo que altera o Set de encomendas
     * 
     * @param m O set das encomendas serao alterados para estes valores 
     */
    public void SetSistema(Map<String, Set<Encomenda>> m){
        sistema = new HashMap<>();
        for(String st: m.keySet()){
            Set<Encomenda> s = new HashSet<>();
            for(Encomenda e: m.get(st)){
                s.add(e.clone());
            }
            sistema.put(st, s);
        }
    }
    
    /**
     * Metodo que retorna um Map sobre a informação das entidades do sistema
     * 
     * @return as informações sobre as Entidades
     */
    public Map<String, Entidade> getInfo(){
        Map<String, Entidade> m = new HashMap<>();
        for(String st: info.keySet()){
            Entidade e = info.get(st);
            m.put(st, e);
        }
        return m;
    }
    
    /**
     * Metodo que altera as entidades do sistema
     * 
     * @param e Novos entidades do sistema
     */
    public void setInfo(Map<String, Entidade> e){
        info = new HashMap<>();
        for(String st: e.keySet())
            info.put(st,e.get(st));
    }
    
    /**
     * Metodo que retorna o administrador atual do sistema
     * 
     * @return o administrador atual do sistema
     */
    public Administrador getAdministrador(){
        return admin.clone();
    }
    
    /**
     * Metodo que altera o administrador
     * 
     * @param a Novos valores do administrador
     */
    public void setAdministrador(Administrador a){
        admin = a.clone();
    }
    
    /**
     * Metodo que faz uma copia do sistema
     * 
     * @return uma copia do Sistema
     */
    public Sistema clone(){
        return new Sistema(this);
    }
    
    /**
     * Metodo que retorna todo o sistema numa String
     * 
     *  @return String do sistema
     */
    public String toString(){
        String s = "Administrador: " + admin.toString();
        for(String st: sistema.keySet()){
            s+= "NIF: " + info.get(st) + " Dados:" + info.get(st) + "  Faturas: ";
            for(Encomenda e : sistema.get(st))
                s += " " + e;
        }
        return s;
    }
    
    /**
     * Metodo que compara se dois sistemas sao iguais
     * 
     * @param o Object que ira ser comparado com o sistema
     * 
     * @return um boolean que corresponde a igualdade entre os dois sistemas
     */
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;

        Sistema s = (Sistema) o;
        if(s.getSistema().equals(sistema) && s.getInfo().equals(info) && admin.equals(s.getAdministrador()))
            return true;
        return false;
    }
    
    /**
     * Metodo que verifica se um utilizador consegue ter acesso aos dados
     * 
     * @param cod Código da entidade que ira tentar ter acesso aos dados
     * 
     * @param passe Palavra chave do utilizador
     * 
     * @return um boolean que corresponde se a entidade conseguiu entrar no sistema
     */
    public boolean validaAcesso(String cod, String passe) throws NaoExisteCodException, PasseErradaException{
        if(!info.containsKey(cod))
            throw new NaoExisteCodException("Código " + cod + " inexistente");
        else{
            if(!info.get(cod).getPassword().equals(passe))
                throw new PasseErradaException("palavra-passe incorreta");
            else return true;
        }
    }
    
    /**
     * Metodo que verifica se existe algum Utilizador no sistema com um determinado identificador
     * 
     * @param conta Identificador do Utilizador que ira ser procurada no sistema
     * 
     * @return um boolean que corresponde a existencia do Utilizador no sistema
     */
    public boolean existeUtilizador(String conta){
        if(info.containsKey(conta) && info.get(conta) instanceof Utilizador)
            return true;
        return false;
    }
    
    /**
     * Metodo que verifica se existe algum Voluntario no sistema com um determinado identificador
     * 
     * @param conta Identificador do Voluntario que ira ser procurado no sistema
     * 
     * @return um boolean que corresponde a existencia do Voluntario no sistema
     */
    public boolean existeVoluntario(String conta){
        if(info.containsKey(conta) && info.get(conta) instanceof Voluntario)
            return true;
        return false;
    }
    
    /**
     * Metodo que verifica se existe alguma Transportadora no sistema com um determinado identificador
     * 
     * @param conta Identificador da Transportadora que ira ser procurada no sistema
     * 
     * @return um boolean que corresponde a existencia da Transportadora no sistema
     */
    public boolean existeTransportadora(String conta){
        if(info.containsKey(conta) && info.get(conta) instanceof Transportadora)
            return true;
        return false;
    }
    
    /**
     * Metodo que verifica se existe alguma Loja no sistema com um determinado identificador
     * 
     * @param conta Identificador da Loja que ira ser procurada no sistema
     * 
     * @return um boolean que corresponde a existencia da Loja no sistema
     */
    public boolean existeLoja(String conta){
        if(info.containsKey(conta) && info.get(conta) instanceof Loja)
            return true;
        return false;
    }
    
    /**
     * Metodo que verifica se existe alguma entidade com um certo código
     * 
     * @param conta Identificador da entidade que irá ser procurada
     * 
     * @return um boolean que corresponde a existencia de um codigo no sistema
     */
    public boolean existeCodigo(String conta){
        return info.containsKey(conta);
    }
    
    /**
     * Metodo que verifica se existe alguma Encomenda no sistema com um determinado codigo
     * 
     * @param cod Codigo da encomenda que ira ser procurada no sistema
     * 
     * @return um boolean que corresponde a existencia da encomenda no sistema
     */
    public boolean existeEncomenda(String cod){
        if(sistema.containsValue(cod) && sistema.get(cod) instanceof Encomenda)
            return true;
        return false;
    }
    
    /**
     * Metodo que retorna uma Encomenda
     * 
     * @param cod Código da encomenda que se pretende retornar
     * 
     * @return uma Encomenda
     */    
    public Encomenda getEncomenda(String cod) throws NaoExisteEncomendaException, IndexOutOfBoundsException{
        try{
            Set<Encomenda> list = sistema.get(cod);
            if(list != null){
                for(Encomenda e: list){
                    if(e.getCodEnc().equals(cod)) return e;
                }
            }
        }
        catch(IndexOutOfBoundsException exc){
            throw new IndexOutOfBoundsException(exc.getMessage());
        }
        throw new NaoExisteEncomendaException("Nao existe nenhuma encomenda com esse código.");
    }
    
    public List<Encomenda> getEncEntrega(){
        List<Encomenda> lista = new ArrayList<>();
        for(Encomenda e: sistema.get("Procura Transporte")){
            lista.add(e);
        }
        return lista;
    }
    
    /**
     * Metodo que adiciona um Utilizador ao Sistema
     * 
     * @param u Utilizador que ira ser adicionado ao sistema
     */
    public void adicionaUtilizador(Utilizador u) throws ExisteCodSistemaException{
        if(existeUtilizador(u.getCod()))
            throw new ExisteCodSistemaException("Código" + u.getCod() + " e invalido, porque ja existe");
        sistema.put(u.getCod(), new HashSet<>());
        info.put(u.getCod(), u.clone());
    }
    
    /**
     * Metodo que adiciona um Voluntario ao Sistema
     * 
     * @param v Voluntario que ira ser adicionado ao sistema
     */
    public void adicionaVoluntario(Voluntario v) throws ExisteCodSistemaException{
        if(existeVoluntario(v.getCod()))
            throw new ExisteCodSistemaException("Código" + v.getCod() + " e invalido, porque ja existe");
        sistema.put(v.getCod(), new HashSet<>());
        info.put(v.getCod(), v.clone());
    }
    
    /**
     * Metodo que adiciona uma Transportadora ao Sistema
     * 
     * @param t Transportadora que ira ser adicionada ao sistema
     */
    public void adicionaTransportadora(Transportadora t) throws ExisteCodSistemaException{
        if(existeTransportadora(t.getCod()))
            throw new ExisteCodSistemaException("Código" + t.getCod() + " e invalido, porque ja existe");
        sistema.put(t.getCod(), new HashSet<>());
        info.put(t.getCod(), t.clone());
    }
    
    /**
     * Metodo que adiciona uma Loja ao Sistema
     * 
     * @param l Loja que ira ser adicionada ao sistema
     */
    public void adicionaLoja(Loja l) throws ExisteCodSistemaException{
        if(existeLoja(l.getCod()))
            throw new ExisteCodSistemaException("Código" + l.getCod() + " e invalido, porque ja existe");
        sistema.put(l.getCod(), new HashSet<>());
        info.put(l.getCod(), l.clone());
    }
    
    /**
    * Metodo que adiciona uma Encomenda ao Sistema
    * 
    * @param e encomenda que sera adicionada ao sistema
    */
    public void adicionaEncomenda(Encomenda e) throws NaoExisteUtilizadorException, ExisteEncomendaException{
        if(!existeUtilizador(e.getCodUser()))
            throw new NaoExisteUtilizadorException("O utilizador " + e.getCodUser() + " não existe");
        else if(existeEncomenda(e.getCodEnc())){
            throw new ExisteEncomendaException("A encomenda com o código " + e.getCodEnc() + " já se encontra no sistema");
        }            
        else{
            sistema.get(e.getCodEnc()).add(e.clone());
        }
    }
    
    /**
     * Metodo que calcula a distância entre duas entidades
     * 
     * @param e1 Entidade
     * 
     * @param e2 Outra entidade
     * 
     * @return um double com a distância entre as duas entidades
     */
    public double distanciaEntidade(Entidade e1, Entidade e2){
        double d = 0.0;
        d = Math.sqrt(Math.pow(e1.getX() - e2.getX(), 2) + Math.pow(e1.getY() - e2.getY(), 2));
        return d;
    }
    
    /**
     * Metodo que calcula a distância entre duas entidades através dos códigos
     * 
     * @param e1 Código de uma das entidades
     * 
     * @param e2 Código da outra entidade
     * 
     * @return um double com a distância entre as duas entidades
     */
    public double distanciaEntidadeCod(String code1, String code2){
        double d = 0.0;
        Entidade e1 = info.get(code1);
        Entidade e2 = info.get(code2);
        d = distanciaEntidade(e1,e2);
        return d;
    }
    
    /**
     * Metodo que dá a distancia percorrida por uma empresa
     * 
     * @param conta Código da empresa que se pretende calcular quanto percorreu
     * 
     * @return um double com o valor total
     */         
    public double distanciaTotalTransportadora(String conta){
        double t = 0.0;
        for(Encomenda e: sistema.get(conta)){
            if(e.getCodEnt() == conta)
                t += distanciaEntidadeCod(e.getCodEnt(), e.getCodLoja());
                t += distanciaEntidadeCod(e.getCodLoja(), e.getCodUser());
        }
        return t;
    } 
    
    /**
     * Metodo que calcula o top 10 das Empresas que mais distância percorreram
     * 
     * @param x Quantas empresas vao estar no top
     * 
     * @return Set com o top 10 empresas
     */   
    public Set<String> top10Empresas() throws NaoExisteTransportadoraException{
        Set <String> s = sistema.keySet();
        TreeSet <String> tree = new TreeSet<String>((i1, i2) -> Double.compare((double)distanciaTotalTransportadora(i1), (double)distanciaTotalTransportadora(i2)));
        TreeSet<String> rt = new TreeSet<String>();
        for(String i: s)
            tree.add(i);
        rt = (TreeSet)tree.descendingSet();
        rt.stream().limit(10);
        return rt;
    }
    
    /**
     * Metodo que calcula o nº total de encomendas de um utilizador
     * 
     * @param conta Conta do utilizador que se pretende calcular quantas encomendas fez
     * 
     * @return um int com o valor total
     */   
    public int totalEncUser(String conta){
        int t = 0;
        for(Encomenda e : sistema.get(conta))
            if (e.getCodUser() == conta) t++;
        return t;
    }
    
    /**
     * Metodo que calcula os 10 utilizadores que mais encomendaram em todo o sistema
     * 
     * @return um ArrayList com a identificacao dos 10 utilizadores que mais encomendaram
     */
    public Set<String> top10EncUser() throws NaoExisteUtilizadorException{
        Set<String> s = sistema.keySet();
        TreeSet<String> tree = new TreeSet<String>((i1, i2) -> Integer.compare((int)totalEncUser(i1), (int)totalEncUser(i2)));
        TreeSet<String> rt = new TreeSet<String>();
        for(String i: s)
            tree.add(i);
        rt = (TreeSet)tree.descendingSet();
        rt.stream().limit(10);
        return rt;
    }
    
    
    
    /**
     * Metodo que guarda num ficheiro um Sistema que contem todas as informações relevantes
     * 
     * @param nomeFicheiro Ficheiro que serao guardados os dados do sistema
     */
    public void guardaEstado(String nomeFicheiro) throws FileNotFoundException, IOException{
        FileOutputStream fos = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }
    
    /**
     * Metodo que carrega de um ficheiro um Sistema com todas as informações relevantes
     * 
     * @param nomeFicheiro Ficheiro em que estao guardadas as informações sobre o sistema
     * 
     * @return um Sistema
     */
    public static Sistema carregaEstado(String nomeFicheiro) throws FileNotFoundException, IOException, ClassNotFoundException{
        FileInputStream fis = new FileInputStream(nomeFicheiro);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Sistema s = (Sistema) ois.readObject();
        ois.close();
        return s;
    }
}
