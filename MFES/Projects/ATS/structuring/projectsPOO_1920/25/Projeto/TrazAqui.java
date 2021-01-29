
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.io.Serializable;
public class TrazAqui implements Serializable
{
    //variáveis de instância
    private Map<String,Transporte> transporte;
    private Map<String,Encomenda> encomenda;
    private Map<String,Utilizadores> utilizador;
    private Map<String,Lojas> loja;
    private List<EncomendasAceites> aceites;
    
    /**
     * Construtor por omissão da classe TrazAqui
     */
    public TrazAqui()
    {
        this.transporte = new HashMap<>();
        this.encomenda = new HashMap<>();
        this.utilizador = new HashMap<>();
        this.loja = new HashMap<>();
        this.aceites = new ArrayList<>();
    }
    
    /**
     * Construtor parametrizado da classe EmpresasTransportadoras
     */
    public TrazAqui(Map<String,Transporte> transporte, Map<String,Utilizadores> utilizador, Map<String,Encomenda> encomenda, Map<String,Lojas> loja, List<EncomendasAceites> ac)
    {
        setTransporte(transporte);
        setUtilizador(utilizador);
        setEncomenda(encomenda);
        setLoja(loja);
        setAceites(ac);
    }
    
    /**
     * Construtor de cópia da classe EmpresasTransportadoras
     */
    public TrazAqui(TrazAqui t)
    {
        setTransporte(t.getTransportes());
        setUtilizador(t.getUtilizadores());
        setEncomenda(t.getEncomendas());
        setLoja(t.getLojas());
        setAceites(t.getAceites());
    }
    
    
    //getters
    public Map<String,Transporte> getTransportes(){
        Map<String,Transporte> ret = new HashMap<>();
        for (Map.Entry<String,Transporte> t : this.transporte.entrySet()){
            ret.put(t.getKey(), t.getValue().clone());
        }
        return ret;
    }
    
    public Map<String,Utilizadores> getUtilizadores(){
        Map<String,Utilizadores> ret = new HashMap<>();
        for (Map.Entry<String,Utilizadores> u : this.utilizador.entrySet()){
            ret.put(u.getKey(), u.getValue().clone());
        }
        return ret;
    }
    
    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> ret = new HashMap<>();
        for (Map.Entry<String,Encomenda> e : this.encomenda.entrySet()){
            ret.put(e.getKey(), e.getValue().clone());
        }
        return ret;
    }
    
    public Map<String,Lojas> getLojas(){
        Map<String,Lojas> ret = new HashMap<>();
        for (Map.Entry<String,Lojas> l : this.loja.entrySet()){
            ret.put(l.getKey(), l.getValue().clone());
        }
        return ret;
    }
    
    public List<EncomendasAceites> getAceites(){
        List<EncomendasAceites> novo = new ArrayList<>();
        Iterator<EncomendasAceites> iter = this.aceites.iterator(); 
        while (iter.hasNext()){
                EncomendasAceites elem = iter.next();
                novo.add(elem.clone());
        }
        return novo;
    }
    
    
    //setters
    public void setTransporte(Map<String,Transporte> transporte){
        this.transporte = new HashMap<>();
        transporte.entrySet().forEach(t -> this.transporte.put(t.getKey(), t.getValue().clone()));
    }
    
    public void setUtilizador(Map<String,Utilizadores> nUtilizador){
        this.utilizador = new HashMap<>();
        nUtilizador.entrySet().forEach(u -> this.utilizador.put(u.getKey(), u.getValue().clone()));
    }
    
    public void setEncomenda(Map<String,Encomenda> enc){
        this.encomenda = new HashMap<>();
        enc.entrySet().forEach(t -> this.encomenda.put(t.getKey(), t.getValue().clone()));
    }
    
    public void setLoja(Map<String,Lojas> loja){
        this.loja = new HashMap<>();
        loja.entrySet().forEach(l -> this.loja.put(l.getKey(), l.getValue().clone()));
    }
    
    public void setAceites(List<EncomendasAceites> aceites){
        List<EncomendasAceites> novo = new ArrayList<>();
        for (EncomendasAceites codigo : aceites){
            novo.add(codigo.clone());
        }
        this.aceites = novo;
    }
    
    /**
     * Metodo que faz uma copia do objecto receptor da mensagem.
     * Para tal invoca o construtor de copia.
     */
    public TrazAqui clone() {
        return new TrazAqui(this);
    }
    
    /**
     *  Metodo que devolve a representaçao em String da EmpresasTransportadora.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Transportes:  ").append(this.transporte).append("\n");
        sb.append("Utilizadores:  ").append(this.utilizador).append("\n");
        sb.append("Encomendas:  ").append(this.encomenda).append("\n");
        sb.append("Lojas:  ").append(this.loja).append("\n");
        sb.append("Encomendas aceites:  ").append(this.aceites).append("\n");
        return sb.toString();
    }
    
    /**
     * Metodo que determina se duas empresas transporadoras sao iguais.
     */
    public boolean equals(Object obj) {
        if(this == obj) return true;
        if(obj == null && this.getClass() != obj.getClass()) return false;
        TrazAqui t = (TrazAqui) obj;     
        return this.transporte.equals(t.getTransportes()) && this.utilizador.equals(t.getUtilizadores())
            && this.encomenda.equals(t.getEncomendas()) && this.loja.equals(t.getLojas()) && this.aceites.equals(t.getAceites());
    }

    //ENCOMENDASACEITES

    /**
     * Método que adiciona encomenda aceite
     */
    public void addAceite(EncomendasAceites e){
        this.aceites.add(e.clone());
    }
    
    /**
     * Método que dá a informação das encomendas aceites em lista de string (necessário no controlador)
     */
    public List<String> getAceite(){
        List<String> novo = new ArrayList<>();
        for(EncomendasAceites e : this.aceites){
            String aceit = e.toString();
            novo.add(aceit);
        }
        return novo;
    }

    //ENCOMENDAS

    /**
    * Método que verifica se uma encomneda já existe a partir do seu codigo
    **/
    public boolean existeEncomenda(String cod){
       if(!this.encomenda.containsKey(cod)){
           return false;
       }
       return true;
    }
    
    /**
     * Devolver a quantidade de encomendas existentes na aplicaçao.
     */    
    public int quantasEncomendas(){
        return this.encomenda.size();               
    }
    
    /**
     * Devolver a informação de uma encomenda em específico, dado o seu código.
     */
    public Encomenda getEncomenda(String cod){
        return this.encomenda.get(cod).clone();
    }
    
    /**
     * Adicionar uma nova encomenda.
     */
    public void registaEncomenda(Encomenda e) throws MensagemException {
        if(!(this.encomenda.containsKey(e.getCodEncomenda()))){
            this.encomenda.put(e.getCodEncomenda(),e);
        }   
        else throw new MensagemException("Encomenda já está adicionada. \n");
    }
    
    //UTILIZADORES
    
    /**
     * Devolve lista que contém as encomendas pedidas por um utilizador a qualquer loja.
     */
    public List<String> utilizadorEncomendasPedidas(Utilizadores u){
        List <String> encomendas = new ArrayList <String>();
        for(Encomenda e : this.encomenda.values()){
            if(e.getDestinatario().equals(u.getCodigo()) ){
                encomendas.add(e.getCodEncomenda());
            }
        }
        return encomendas;
    }

    /**
     * Insere pedidos de encomendas a uma loja, por parte de um utilizador.
     */
    public void solicitaEncomenda(Encomenda e, Utilizadores u, Lojas l){
        u.pedeEncomenda(l,e);
    }
    
    /**
     * Atualiza informação acerca de um utilizador
     */    
    public void atualizaUtilizador(Utilizadores u){
        for(Map.Entry<String,Utilizadores> entry : this.utilizador.entrySet()){
            if(u.getCodigo().equals(entry.getKey())){
                if(!u.getRegistos().equals(entry.getValue().getRegistos())){
                    entry.getValue().setRegistos(u.getRegistos());
                }
            }
        }
    }

    /**
     * Devolver a informação de um utilizador, dado o seu código.
     */
    public Utilizadores getUtilizador(String cod) throws MensagemException{
        if(!(this.utilizador.containsKey(cod))){
            throw new MensagemException("Utilizador não está registado. \n");    
        }   
        else return this.utilizador.get(cod).clone();
    }
    
    /**
     * Adicionar um novo utilizador.
     */
    public void registaUtilizador(Utilizadores u) throws MensagemException {
        if(!(this.utilizador.containsKey(u.getCodigo()))){
            this.utilizador.put(u.getCodigo(),u.clone());
        }   
        else throw new MensagemException("Utilizador já esta registado. \n");
    }
    
    /**
     * Devolve utilizador tendo em conta o seu email e password.
     */
    public Utilizadores devolveUtilizadorCredenciais(String email,String pass) throws MensagemException{
        for (Utilizadores u : this.utilizador.values()){
            if(u.verificaCredenciais(email,pass)){return u.clone();}   
        }
        throw new MensagemException("Utilizador não está registado");
    }

    /**
     * Verifica se as credenciais com que um utilizador esta a tentar entrar na aplicaçao estão corretas. 
     */
    public boolean existeCredenciaisUtilizador(String email,String pass){
        for (Utilizadores u : this.utilizador.values()){
            if(u.verificaCredenciais(email,pass)){return true;}   
        }
        return false;   
    }
    
    /**
     * Determina a listagem dos 10 utilizadores que mais utilizam o sistema(em número de encomendas transportadas).
     */
    public List<Utilizadores> utilizadoresTop10(){
        return (List<Utilizadores>) this.utilizador.values().stream().limit((long) 10).sorted(new UtilizadoresTop10Comparator())
                                .map(Utilizadores::clone).collect(Collectors.toList());
    }
    
    
    //TRANSPORTES
    
    /**
     * Atualiza informação acerca de uma transportadora.
     */

    public void atualizaTransportadora(EmpresasTransportadoras e){
        for(Map.Entry<String,Transporte> entry : this.transporte.entrySet()){
            if(e.getCodigo().equals(entry.getKey())){
                EmpresasTransportadoras analise = (EmpresasTransportadoras) entry.getValue();
                if(!e.getAvaliacoes().equals(entry.getValue().getAvaliacoes())){
                    entry.getValue().setAvaliacoes(e.getAvaliacoes());
                }
                if(!e.getLucro().equals(analise.getLucro())){
                    analise.setLucro(e.getLucro());
                }
                if(!e.getRegistos().equals(entry.getValue().getRegistos())){
                    entry.getValue().setRegistos(e.getRegistos());
                    
                }
                if(e.getNumKms() != analise.getNumKms()){
                    analise.setNumKms(e.getNumKms());
                }
            }
        }
    }
    
    /**
     * Atualiza informação acerca de um voluntario
     */
    public void atualizaVoluntario(Voluntarios v){
        for(Map.Entry<String,Transporte> entry : this.transporte.entrySet()){
            if(v.getCodigo().equals(entry.getKey())){
                Voluntarios analise = (Voluntarios) entry.getValue();
                if(!v.getAvaliacoes().equals(entry.getValue().getAvaliacoes())){
                    entry.getValue().setAvaliacoes(v.getAvaliacoes());
                   
                }
                
                if(!v.getRegistos().equals(entry.getValue().getRegistos())){
                    entry.getValue().setRegistos(v.getRegistos());
                    
                }

                if(v.getDisponivel() != analise.getDisponivel()){
                    analise.setDisponivel(v.getDisponivel());

                }
            }
        }
    }
    
    /**
     * Devolve voluntario tendo em conta o seu email e password.
     */
    public Voluntarios devolveVoluntarioCredenciais(String email,String pass) throws MensagemException{
        for (Transporte t : this.transporte.values()){
            if(t.tipoTransporte() == "Voluntarios"){
                Voluntarios v = (Voluntarios) t;
                if(v.verificaCredenciais(email,pass)){return v;}
            }
        }
        throw new MensagemException("Utilizador não está registado");
    }

    /**
     * Devolve transportadora tendo em conta o seu email e password.
     */
    public EmpresasTransportadoras devolveTransportadoraCredenciais(String email,String pass) throws MensagemException{
        for (Transporte t : this.transporte.values()){
            if(t.tipoTransporte() == "EmpresasTransportadoras"){
                EmpresasTransportadoras e = (EmpresasTransportadoras) t;
                if(e.verificaCredenciais(email,pass)){return e.clone();}
            }
        }
        throw new MensagemException("Utilizador não está registado");
    }

    /**
     * Regista uma transportadora na aplicação.
     */
    public void RegistaTransportadora(EmpresasTransportadoras trans) throws MensagemException {
        if(!(this.transporte.containsKey(trans.getCodigo()))){
            transporte.put(trans.getCodigo(),trans.clone());
        }
        else{throw new MensagemException("Transportadora já está registada. \n");}
            
    }
    
    /**
     * Regista um voluntario na aplicação.
     */
    public void RegistaVoluntario(Voluntarios vol) throws MensagemException {
        if(!(this.transporte.containsKey(vol.getCodigo()))){
            transporte.put(vol.getCodigo(),vol); //se colocar vol.clone() mete sempre a disponibilidade a false
        }
        else{throw new MensagemException("Voluntario já está registado. \n");}
    }
    
    /**
     * Atualiza classificação de um transporte.
     */
    public void classificarTransporte(String cod, int classificacao){
        for (Transporte v : this.transporte.values()){
            if(cod.equals(v.getCodigo())){
                List<Integer> novo = new ArrayList<Integer>();
                for(int num : v.getAvaliacoes()){
                    novo.add(num);
                }
                novo.add(classificacao);
                v.setAvaliacoes(novo);
            }
        }
    }
    
    /**
     * Método que vẽ se há voluntarios disponiveis na zona da loja
     */
    public List<Voluntarios> voluntariosDisponiveis(){
        List<Voluntarios> vol = new ArrayList<Voluntarios>();
        for (Transporte t : this.transporte.values()){
            if(t.tipoTransporte().equals("Voluntarios")){
                Voluntarios v = (Voluntarios) t;
                if(v.verDisp() == true){
                    vol.add(v);
                }    
            }   
        }
        return vol;
    }
    
    /**
     * Devolver a quantidade de transportes existentes na aplicaçao.
     */    
    public int quantosTransportes(){
        return this.transporte.size();               
    }
    
    /**
     * Devolver a informação de um transporte, dado o seu código.
     */
    public Transporte getTransporte(String cod) throws MensagemException{
        if(!(this.transporte.containsKey(cod))){
            throw new MensagemException("Transporte não está registado. \n");    
        }   
        else return this.transporte.get(cod).clone();
    }
    
    /**
     * Método que vê se voluntário pode entregar a encomenda.
     */
    public boolean entregaEncomendaVoluntarios(Encomenda e, Lojas l, Utilizadores u){
        List <Voluntarios> vol = new ArrayList <Voluntarios>();
        vol = this.voluntariosDisponiveis();
        for(Voluntarios v : vol){
            if(v.recolheEncomenda(e,l,u)){
                v.fazTransporteEncomenda(e);                                          
                return true;
            }
        }
        return false;
    }
    
    /**
     * Determina a listagem das 10 empresas transportadoras que mais utilizam o sistema (em número de kms percorridos).
     */
    public List<Transporte> transportadorasTop10(){
        return (List<Transporte>) this.transporte.values().stream().limit((long) 10).filter(t -> t.tipoTransporte().equals("EmpresasTransportadoras"))
                                               .sorted(new TransportadorasTop10Comparator())
                                               .map(Transporte::clone).collect(Collectors.toList());
    }

    /**
     * Verifica se as credenciais com que um voluntario esta a tentar entrar na aplicaçao existem. 
     */
    public boolean existeCredenciaisVoluntario(String cod,String pass){
        for (Transporte t : this.transporte.values()){
            if (t.tipoTransporte().equals("Voluntarios")){ 
                Voluntarios v = (Voluntarios) t;
                if(v.verificaCredenciais(cod,pass)){return true;}   
            }
        }
        return false;   
    }
    
    /**
     * Verifica se as credenciais com que uma trasnportadora esta a tentar entrar na aplicaçao existem.
     */
    public boolean existeCredenciaisTransportadora(String cod,String pass){
        for (Transporte t : this.transporte.values()){
            if (t.tipoTransporte().equals("EmpresasTransportadoras")){ 
                EmpresasTransportadoras ep = (EmpresasTransportadoras) t;
                if(ep.verificaCredenciais(cod,pass)){return true;}   
            }
        }
        return false;   
    }
    
    //LOJAS

    /**
     * Atualiza informação acerca de uma loja
     */    
    public void atualizaLoja(Lojas l){
        for(Map.Entry<String,Lojas> entry : this.loja.entrySet()){
            if(l.getCodigo().equals(entry.getKey())){
                if(!l.getFilaEspera().equals(entry.getValue().getFilaEspera())){
                    entry.getValue().setFilaEspera(l.getFilaEspera());
                }
            }
        }
    }

    /**
     * Adiciona encomendas às filas de espera das lojas tendo em conta a informação dos logs iniciais.
     */
    public void adicionaFilaEsperaLojas(){
        for(Encomenda enc : this.encomenda.values()){
            String codLoja = enc.getVendedor();
            for(Lojas l : this.loja.values()){
                if(l. getCodigo().equals(codLoja)){
                    l.adicionaFilaEspera(enc);
                }
            }
        }
    }

    /**
     * Devolve loja tendo em conta o seu email e password.
     */
    public Lojas devolveLojaCredenciais(String email,String pass) throws MensagemException{
        for (Lojas l : this.loja.values()){
            if(l.verificaCredenciais(email,pass)){return l.clone();}   
        }
        throw new MensagemException("Loja não está registada");
    }

    /**
     * Vê se uma loja está registada na aplicação.
     */
    public boolean existeLoja(String codLoja){
        for ( String key : this.loja.keySet() ) {
            if(key.equals(codLoja)){
                return true;
            }
        }
        return false;
    }
    
    /**
     * Devolve a informação de uma loja, dado o seu código.
     */
    public Lojas getLojaInfo(String cod){
        return this.loja.get(cod).clone();
    }
    
    /**
     * Método que retorna o tamanho da fila de espera.
     */
    public int getTamanhoFila(Lojas l) {
        return l.numeroPessoasFila();
    }
    
    /**
     * Adiciona uma nova loja.
     */
    public void registaLoja(Lojas l) throws MensagemException {
        if(!(this.loja.containsKey(l.getCodigo()))){
            this.loja.put(l.getCodigo(),l.clone());
        }   
        else throw new MensagemException("Loja ja existe. \n");
    }
    
    /**
     * Verifica se as credenciais com que uma loja esta a tentar entrar na aplicaçao estão corretas. 
     */
    public boolean existeCredenciaisLoja(String cod,String pass){
        for (Lojas l : this.loja.values()){
            if(l.verificaCredenciais(cod,pass)){return true;}   
        }
        return false;   
    }

    /**
     * Verifica se um utilizador está na fila de espera da loja.
     */
    public boolean existeUtilizadorFila(String u, String codLoja){
        return (this.loja.get(codLoja).getFilaEspera().containsKey(u));
    }
    
    
    //TrazAqui

    /**
     * Método que exporta a informação de trazAqui como um ficheiro CSV.
     */   
    public void gravaCSV(String nomeFicheiro) throws FileNotFoundException {
        PrintWriter pw = new PrintWriter(nomeFicheiro);
        
        for (Utilizadores u: this.utilizador.values()){
            pw.println(u.toStringCSV());
        }
    
        for (Transporte t: this.transporte.values()){
            if (t instanceof Voluntarios){
                pw.println(t.toStringCSV());
            }
        }
    
        for (Transporte t: this.transporte.values()){
            if (t instanceof EmpresasTransportadoras){
                pw.println(t.toStringCSV());
            }
        }
    
        for (Lojas l: this.loja.values()){
            pw.println(l.toStringCSV());
        }
    
        for (Encomenda e: this.encomenda.values()){
            pw.println(e.toStringCSV());
        }
        
        for (EncomendasAceites ea: this.aceites){
            pw.println(ea.toStringCSV());
        }
    
        pw.flush();
        pw.close();
      }

   
    /**
     * Método que grava a instância de TrazAqui num ficheiro de objeto em disco.
     */
    public void gravaEmFicheiro(String nomeFicheiro) throws FileNotFoundException, IOException{
        FileOutputStream fos = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }    
    
    /**
     * Método que carrega a instância de TrazAqui num ficheiro de objeto em disco.
     */
    public static TrazAqui leFicheiro(String nomeFicheiro) throws FileNotFoundException, IOException, ClassNotFoundException{
        FileInputStream fis = new FileInputStream(nomeFicheiro);
        ObjectInputStream ois = new ObjectInputStream(fis);
        TrazAqui t = (TrazAqui) ois.readObject();
        ois.close();
        return t;
    }
}