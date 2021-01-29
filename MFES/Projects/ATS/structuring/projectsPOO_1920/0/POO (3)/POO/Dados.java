import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.Set;
import java.util.TreeSet;
import java.util.ArrayList;
import java.time.LocalDate;


public class Dados implements Serializable {
    //Chave:codvoluntario,valor:voluntario
    private Map<String, Voluntario>         voluntarios;
    //Chave:codutilizador,valor:utilizador
    private Map<String, Utilizador>         users;
    //Chave:codvtransportadora,valor:transportadora
    private Map<String, Transportadora>     trans;
    //Chave:codloja,valor:loja
    private Map<String, Loja>               lojas;
    //Chave:codcliente,valor:cliente
    private Map<String, Cliente>            clientes;
    //chate:codEncomenda, valor:Encomenda
    private Map<String, Encomenda>          encomendas;
    //Chave:codCliente,valor:Encomenda
    private Map<String, List<Encomenda>>    encomendasProntas;
    //Chave:codEncomenda,valor:Encomenda
    private Map<String, Encomenda>          encomendasAceites; // cada encomenda aceite pela loja é adicionado
    // chate:codCliente, valor:Lista de Viagem
    private Map<String, List<Viagem>>       viagensCliente;
  
    //chave:codEmpresa, valor:Lista de Viagem
    private Map<String, List<Viagem>>       viagensTransportadora; //viagens que empresa realizou
    //chave:codVoluntario, valor:Lista de Viagem
    private Map<String, List<Viagem>>       viagensVoluntario;
    //chave:codEncomenda, valor:Lista de Viagem
    private Map<String, List<Viagem>>       viagensEncomendas; // indicar as encomendas das viagens aceites
   
    
    public Dados(){
        this.voluntarios = new HashMap<>();
        this.users = new HashMap<>();
        this.trans = new HashMap<>();
        this.lojas = new HashMap<>();
        this.clientes = new HashMap<>();
        this.encomendas = new HashMap<>();
        this.encomendasProntas = new HashMap<>();
        this.encomendasAceites = new HashMap<>();
        this.viagensCliente = new HashMap<>();
        this.viagensTransportadora = new HashMap<>();
        this.viagensVoluntario = new HashMap<>();
        this.viagensEncomendas = new HashMap<>();
    }
    public Dados(Dados d){
        this.voluntarios=d.getVoluntarios();
        this.users=d.getUsers();
        this.trans=d.getTransportadoras();
        this.lojas=d.getLojas();
        this.clientes=d.getClientes();
        this.encomendas=d.getEncomendas();
        this.encomendasProntas=d.getEncomendasProntas();
        
    }
    public Cliente getCliente(String codCliente) {

        if (this.clientes.containsKey(codCliente)) {
            return this.getClientes().get(codCliente).clone();
        }
        return null;
    }

    public Map<String, Voluntario> getVoluntarios() {

        Map<String, Voluntario> u = new HashMap<String, Voluntario>();

        Iterator<Map.Entry<String, Voluntario>> itt = voluntarios.entrySet().iterator();
        while (itt.hasNext()) {
            Map.Entry<String, Voluntario> entry = itt.next();
            u.put(entry.getKey(), entry.getValue());
        }
        return u;
    }

    public Map<String, Utilizador> getUsers() {
        Map<String, Utilizador> u = new HashMap<String, Utilizador>();

        Iterator<Map.Entry<String, Utilizador>> itt = users.entrySet().iterator();
        while (itt.hasNext()) {
            Map.Entry<String, Utilizador> entry = itt.next();
            u.put(entry.getKey(), entry.getValue());
        }
        return u;
    }

    public Map<String, Transportadora> getTransportadoras() {
        Map<String, Transportadora> u = new HashMap<String, Transportadora>();

        Iterator<Map.Entry<String, Transportadora>> itt = trans.entrySet().iterator();
        while (itt.hasNext()) {
            Map.Entry<String, Transportadora> entry = itt.next();
            u.put(entry.getKey(), entry.getValue());
        }
        return u;
    }

    public Map<String, Loja> getLojas() {
        Map<String, Loja> u = new HashMap<String, Loja>();

        Iterator<Map.Entry<String, Loja>> itt = lojas.entrySet().iterator();
        while (itt.hasNext()) {
            Map.Entry<String, Loja> entry = itt.next();
            u.put(entry.getKey(), entry.getValue());
        }
        return u;
    }

    public Map<String, Cliente> getClientes() {
        Map<String, Cliente> u = new HashMap<String, Cliente>();

        Iterator<Map.Entry<String, Cliente>> itt = clientes.entrySet().iterator();
        while (itt.hasNext()) {
            Map.Entry<String, Cliente> entry = itt.next();
            u.put(entry.getKey(), entry.getValue());
        }
        return u;
    }
    
     public Map<String, Encomenda> getEncomendas() {
        Map<String, Encomenda> u = new HashMap<String, Encomenda>();

        Iterator<Map.Entry<String, Encomenda>> itt = encomendas.entrySet().iterator();
        while (itt.hasNext()) {
            Map.Entry<String, Encomenda> entry = itt.next();
            u.put(entry.getKey(), entry.getValue());
        }
        return u;
    }
    public Map<String, List<Viagem>> getViagensEncomendas(){
        Map<String, List<Viagem>> u = new HashMap<>();

        Iterator<Map.Entry<String, List<Viagem>>> itt = viagensEncomendas.entrySet().iterator();
        while (itt.hasNext()) {
            Map.Entry<String, List<Viagem>> entry = itt.next();
            u.put(entry.getKey(), entry.getValue());
        }
        return u;
    }
    
    

    public boolean registarVoluntario(Voluntario novoVoluntario) {
        if (!this.voluntarios.containsKey(novoVoluntario.getCodVoluntario()))
            this.voluntarios.put(novoVoluntario.getCodVoluntario(), novoVoluntario);
        else
            return false;
        return true;
    }

    public Map<String, List<Encomenda>> getEncomendasProntas() {
        Map<String,  List<Encomenda>> u = new HashMap<>();

        Iterator<Map.Entry<String,  List<Encomenda>>> itt = this.encomendasProntas.entrySet().iterator();
        while (itt.hasNext()) {
            Map.Entry<String,  List<Encomenda>> entry = itt.next();
            u.put(entry.getKey(), entry.getValue());
        }
        return u;
    }

    public Map<String, Encomenda> getEncomendasAceites() {
        Map<String, Encomenda> u = new HashMap<>();

        Iterator<Map.Entry<String, Encomenda>> itt = encomendasAceites.entrySet().iterator();
        while (itt.hasNext()) {
            Map.Entry<String, Encomenda> entry = itt.next();
            u.put(entry.getKey(), entry.getValue());
        }
        return u;
    }
    
    public List<Viagem> getViagensCliente(String codCliente){
        List <Viagem> nova = new ArrayList<>();
        if(this.viagensCliente.containsKey(codCliente)){
            return this.viagensCliente.get(codCliente);
        }
        return nova;
    }
    public boolean registarUser(Utilizador novoUser) {

        if (!this.users.containsKey(novoUser.getEmail()))
            this.users.put(novoUser.getEmail(), novoUser);
        else
            return false;
        return true;
    }

    public boolean registarTransportadora(Transportadora novaTrans) {

        if (!this.trans.containsKey(novaTrans.getCodEmpresa()))
            this.trans.put(novaTrans.getCodEmpresa(), novaTrans.clone());
        else
            return false;
        return true;
    }

    public boolean registarLoja(Loja novaLoja) {
        if (!this.lojas.containsKey(novaLoja.getCodLoja()))
            this.lojas.put(novaLoja.getCodLoja(), novaLoja.clone());
        else
            return false;
        return true;
    }

    public boolean registarUtilizador(Cliente novoUtilizador) {
        if (!this.clientes.containsKey(novoUtilizador.getCodCliente()))
            this.clientes.put(novoUtilizador.getCodCliente(), novoUtilizador.clone());
        else
            return false;
        return true;
    }
    
    public void registarProduto(LinhaEncomenda le,String loja) {
        
        if (this.lojas.containsKey(loja)){
           
            this.lojas.get(loja).registarProduto(le);
        }
    }
    public boolean containsVoluntario(String email) {
        if (this.voluntarios.containsKey(email)) {
            return true;
        }
        return false;
    }

    public boolean containsTransportadora(String email) {
        if (this.trans.containsKey(email)) {
            return true;
        }
        return false;
    }

    public boolean containsLoja(String codLoja) {
        if (this.lojas.containsKey(codLoja)) {
            return true;
        }
        return false;
    }

    public boolean containsUtilizador(String email) {
        if (this.clientes.containsKey(email)) {
            return true;
        }
        return false;
    }

    public boolean containsUser(String email) {
        if (this.users.containsKey(email)) {
            return true;
        }
        return false;
    }

    public Voluntario getVoluntario(String codVoluntario) {
        return this.voluntarios.get(codVoluntario);
    }

    public Utilizador getUser(String email) {
        return this.users.get(email);
    }

    public Transportadora getTrans(String codEmpresa) {
        return this.trans.get(codEmpresa);
    }

    public Loja getLoja(String email) {
        return this.lojas.get(email);
    }

    public Cliente getUtilizador(String email) {
        return this.clientes.get(email);
    }
    
    /*
    public LinhaEncomenda getProduto(String codigo) {
        return this.produtos.get(codigo);
    } */

    public Voluntario LogInVoluntario(String codVoluntario, String password) {
        if (this.voluntarios.containsKey(codVoluntario))
            if (this.voluntarios.get(codVoluntario).getPassword().equals(password))
                return this.getVoluntario(codVoluntario);
        return null;
    }

    public Utilizador LogInUser(String email, String password) {
        if (this.users.containsKey(email))
            if (this.users.get(email).getPassword().equals(password))
                return this.getVoluntario(email);
        return null;
    }

    public Transportadora LogInTransportadora(String codEmpresa, String password) {
        if (this.trans.containsKey(codEmpresa))
            if (this.trans.get(codEmpresa).getPassword().equals(password))
                return this.getTrans(codEmpresa);
        return null;
    }

    public Loja LogInLoja(String codLoja, String password) {
        if (this.lojas.containsKey(codLoja))
            if (this.lojas.get(codLoja).getPassword().equals(password))
                return this.getLoja(codLoja);
        return null;
    }

    public Cliente LogInUtilizador(String codUtilizador, String password) {
        if (this.clientes.containsKey(codUtilizador))
            if (this.clientes.get(codUtilizador).getPassword().equals(password))
                return this.getUtilizador(codUtilizador);
        return null;
    }

    public boolean removerVoluntario(String codVoluntario) {
        return null != this.voluntarios.remove(codVoluntario);
    }


    public void registarEncomenda(Encomenda encomenda) throws NaoExisteException, QuantidadeExcedidaException {
        String codloja = encomenda.getCodLoja();
     
        if (this.lojas.containsKey(codloja)) {
            Loja loja = this.lojas.get(codloja);
            loja.adicionaListaEspera(encomenda);
        }
        else{
            throw new NaoExisteException(Constantes.LOJA_NAO_EXISTE);
        }
    }
    
    public List<Transportadora> top10Transportadoras(){
        
        Set<Transportadora> transpOrd = new TreeSet<>(new ComparatorDistTransportadora());
        
        for(Transportadora t : this.trans.values()){
            transpOrd.add(t);
        }
        
        List<Transportadora> transpList = new ArrayList<>(10);
        
        int c = 0;
        Iterator<Transportadora> it = transpOrd.iterator();
        while (it.hasNext() && c < 10) {
            Transportadora elem = it.next();
            transpList.add(elem);
            c++;
        }
        return transpList;
    }
    
    public List<ClienteViagem> top10Cliente(){
        
        Set<ClienteViagem> clienteOrd = new TreeSet<>(new ComparatorNrViagens());
        
        for(Map.Entry<String, List<Viagem>> entry : this.viagensCliente.entrySet()){
            Cliente c = this.clientes.get(entry.getKey());
            int size = entry.getValue().size();
            ClienteViagem cv = new ClienteViagem(c, size);
            clienteOrd.add(cv);
        }
        
        List<ClienteViagem> clienteList = new ArrayList<>(10);
        
        int c = 0;
        Iterator<ClienteViagem> it = clienteOrd.iterator();
        while (it.hasNext() && c < 10) {
            ClienteViagem elem = it.next();
            clienteList.add(elem);
            c++;
        }
        return clienteList;
    }
    public void atualizarLoja(Loja loja){
        if(this.lojas.containsKey(loja.getCodLoja())){
            this.lojas.put(loja.getCodLoja(),loja.clone());
        }
    }
    
    public void adicionaEncomendaCliente(Encomenda e){
        if(this.clientes.containsKey(e.getCodCliente())){
            Cliente c = this.clientes.get(e.getCodCliente());
            c.adicionaEncomenda(e);
        }
    }
    
    public Encomenda validarEncomendaLoja(String codLoja) throws NaoExisteException {
        if(this.lojas.containsKey(codLoja)){
            Loja loja = this.lojas.get(codLoja);
            Encomenda e = loja.validarEncomenda();
            this.encomendasAceites.put(e.getCodEncomenda(),e.clone());
            this.encomendas.put(e.getCodEncomenda(),e.clone());
            return e;
        }
        return null;
    }
    
    public void adicionarNotificacaoCliente(Encomenda enc){
        if(this.clientes.containsKey(enc.getCodCliente())){
            Cliente cliente = this.clientes.get(enc.getCodCliente());
            cliente.adicionarNotificacao(enc.getCodEncomenda());
        }
    }
    
    public void removerTodasNotificacoesCliente(String codCliente){
        if(this.clientes.containsKey(codCliente)){
            this.clientes.get(codCliente).removerTodasNotificacoes();
        }
    }
    
        /**
     * Método em que um Voluntario aceita o transporte de uma encomenda
     */
    public void escolheEncomenda(String codEncomenda, Voluntario vol) throws ForaDeAlcanceException {
        if(this.encomendasAceites.containsKey(codEncomenda)){
            Encomenda e = this.encomendasAceites.get(codEncomenda);
            Loja l = this.lojas.get(e.getCodLoja());
            Cliente c = this.clientes.get(e.getCodCliente());
            Viagem v = vol.fazerTransporte(c,l);
            mapearViagens(e,v);
        }
    }
    
    
    /**
     * Método em que uma transportadora aceita o transporte de uma encomenda
     */
    public void escolheEncomenda(String codEncomenda, Transportadora t) throws ForaDeAlcanceException {
        if(this.encomendasAceites.containsKey(codEncomenda)){
            Encomenda e = this.encomendasAceites.get(codEncomenda);
            Loja l = this.lojas.get(e.getCodLoja());
            Cliente c = this.clientes.get(e.getCodCliente());
            Viagem v = t.fazerTransporte(c,l,e);
            mapearViagens(e,v);
        }
    }
    
    /**
     * Método que trás a listagem das encomendas prontas para transporte para um certo cliente.
     */
    public Set<String> getCodEncomendasProntasCliente(String codCliente){
        Set<String> encomendasProntas = new TreeSet<>();
        if(this.encomendasProntas.containsKey(codCliente)){
            List<Encomenda> encs = this.encomendasProntas.get(codCliente);
            encs.forEach(e -> encomendasProntas.add(e.getCodEncomenda()));
            return encomendasProntas;
        } 
        else {
            return encomendasProntas;
        }
    }
    
    /**
     * Método que trás as listagem das viagens de uma certa encomenda.
     */
    public List<Viagem> getViagensEncomenda(String codEncomenda){
        List<Viagem> viagensProntas = new ArrayList<>();
        if(this.viagensEncomendas.containsKey(codEncomenda)){
            List<Viagem> viagens = this.viagensEncomendas.get(codEncomenda);
            viagens.forEach(e -> viagensProntas.add(e.clone()));
            return viagensProntas;
        } 
        else {
            return viagensProntas;
        }
    }
    
    
    public List<Viagem> getViagens(String codEncomenda){
        if(this.encomendasAceites.containsKey(codEncomenda)){
            Encomenda e = this.encomendasAceites.get(codEncomenda);
            List<Viagem> viagens = this.viagensEncomendas.get(codEncomenda);
            return viagens;
        }
        return null;
    }
    
    public void realizaTransporte(int i, String codCliente, String codEncomenda) throws NaoExisteException {
        
        if(this.encomendasProntas.containsKey(codCliente)){
            Encomenda e = this.encomendasAceites.get(codEncomenda);
            
            Viagem v = this.viagensEncomendas.get(codEncomenda).get(i-1);
            
            String codTransporte = v.getCodTransportadora();
            if(this.trans.containsKey(codTransporte)){
                Transportadora t = this.trans.get(codTransporte);
                
                t = t.atualizaKms(v.getDistancia());
                this.trans.put(codTransporte, t.clone());
                if(this.viagensTransportadora.containsKey(codTransporte)){
                   
                    List<Viagem> viagensT = this.viagensTransportadora.get(codTransporte);
                    viagensT.add(v.clone());
                    this.viagensTransportadora.put(codTransporte,viagensT);
                }
                else{
                    
                    List<Viagem> viagensT = new ArrayList<>();
                    viagensT.add(v.clone());
                    this.viagensTransportadora.put(codTransporte,viagensT);
                }
                
            }
            else{
                if(this.voluntarios.containsKey(codTransporte)){
                    Voluntario vol = this.voluntarios.get(codTransporte);
                    
                    vol = vol.atualizaKms(v.getDistancia());
                    if(this.viagensVoluntario.containsKey(codTransporte)){
                        List<Viagem> viagensV = this.viagensVoluntario.get(codTransporte);
                        viagensV.add(v.clone());
                        this.viagensTransportadora.put(codTransporte,viagensV);
                    }
                    else{
                        List<Viagem> viagensV = new ArrayList<>();
                        viagensV.add(v.clone());
                        this.viagensVoluntario.put(codTransporte,viagensV);
                    }
                }
                else{
                    throw new NaoExisteException(Constantes.TRANSPORTADORA_NAO_EXISTE);
                }
            }
            
            if(this.viagensCliente.containsKey(codCliente)){
                this.viagensCliente.get(codCliente).add(v.clone());
            }
            else{
                List<Viagem> viagensC = new ArrayList<>();
                viagensC.add(v.clone());
                this.viagensCliente.put(codCliente,viagensC);
            }
            
            this.encomendasProntas.remove(codCliente);
            this.encomendasAceites.remove(codEncomenda);
        }
    }
    
    private void mapearViagens(Encomenda e, Viagem v){
        if(this.encomendasProntas.containsKey(e.getCodCliente())){
            List<Encomenda> encomendas = this.encomendasProntas.get(e.getCodCliente());
            encomendas.add(e.clone());
            this.encomendasProntas.put(e.getCodCliente(), encomendas);
            if(this.viagensEncomendas.containsKey(e.getCodEncomenda())){
                List<Viagem> viagens = this.viagensEncomendas.get(e.getCodEncomenda());
                viagens.add(v.clone());
                this.viagensEncomendas.put(e.getCodEncomenda(), viagens);
            }
            else{
                List<Viagem> viagens = new ArrayList<>();
                viagens.add(v.clone());
                this.viagensEncomendas.put(e.getCodEncomenda(),viagens);
            }
        }
        else{
            List<Encomenda> encomendas = new ArrayList<>();
            encomendas.add(e.clone());
            this.encomendasProntas.put(e.getCodCliente(), encomendas);
            if(this.viagensEncomendas.containsKey(e.getCodEncomenda())){
                List<Viagem> viagens = this.viagensEncomendas.get(e.getCodEncomenda());
                viagens.add(v.clone());
                this.viagensEncomendas.put(e.getCodEncomenda(), viagens);
            }
            else{
                List<Viagem> viagens = new ArrayList<>();
                viagens.add(v.clone());
                this.viagensEncomendas.put(e.getCodEncomenda(),viagens);
            }
        }
    }
    
    public void classificar(String codigo, int classificacao) throws NaoExisteException {
        if(this.voluntarios.containsKey(codigo)){
            Voluntario v = this.voluntarios.get(codigo);
            v = v.registarAvaliacao(classificacao);
            this.voluntarios.put(codigo, v.clone());
        }else{
            if(this.trans.containsKey(codigo)){
                Transportadora t= this.trans.get(codigo);
                t = t.registarAvaliacao(classificacao);
                this.trans.put(codigo,t.clone());
            }
            else { 
                 throw new NaoExisteException(Constantes.TRANSPORTADORA_NAO_EXISTE);
            }
        }
       
    }
    
    public double faturadoEntreDatas(LocalDate inicio, LocalDate fim,String codTransportadora) throws NaoExisteException {
        double total =0;
        Calculos calculos = new Calculos();
        if(this.viagensTransportadora.containsKey(codTransportadora)){
            List<Viagem> viagens = this.viagensTransportadora.get(codTransportadora);
            for(Viagem v: viagens){
                if((calculos.isBetween(v.getData(),inicio,fim))){
                    total += v.getCusto();
                    
                }
            }
            
                
        }else{
            throw new NaoExisteException(Constantes.TRANSPORTADORA_NAO_TEM_VIAGENS);
        }
        return total;
    }
    
    public List<Viagem> informacaoViagem(LocalDate inicio, LocalDate fim,String codCliente) throws NaoExisteException {
        List<Viagem> nova = new ArrayList<>();
        Calculos calculos = new Calculos();
        if(this.viagensCliente.containsKey(codCliente)){
            List<Viagem> viagens = this.viagensCliente.get(codCliente);
            for(Viagem v: viagens){
                if((calculos.isBetween(v.getData(),inicio,fim))){
                    nova.add(v.clone());
                    
                }
            }
            
        }else{
            throw new NaoExisteException(Constantes.CLIENTE_NAO_TEM_VIAGENS);
        }
        return nova;
    }
    
    public double getAvaliacaoT(String codEmpresa){
        double total=0;
        Transportadora t = this.trans.get(codEmpresa);
        total = t.getAvaliacao();
        return total;
    }
}