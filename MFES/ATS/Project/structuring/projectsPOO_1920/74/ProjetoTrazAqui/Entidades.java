import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.function.Consumer;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.time.LocalDateTime;
import java.time.Duration;
import java.io.Serializable;
import java.util.stream.Stream;
public class Entidades implements Serializable{
    private Map<String,Entidade> entidades;
    
     /**
     * Construtor por omissao da classe
     */
    public Entidades(){
        this.entidades = new HashMap<>();
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros Map<String,Entidade>
     */ 
    public Entidades(Map<String,Entidade> m){
        this.entidades = m.entrySet().stream().collect(Collectors.toMap(v->v.getKey(),v->v.getValue().clone()));
    }
    /**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */ 
    public Entidades(Entidades m){
        this.entidades = m.getEntidades();
    }
     /**
     * Faz clone da classe
     * @return o clone da classe
     */
    public Entidades clone(){
        return new Entidades(this);
    }
     /**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;
        Entidades p = (Entidades) o;
        return this.entidades.equals(p.getEntidades());
    }
      /**
     * Metodo que coloca numa String algumas variaveis de instancia da Encomenda
     */ 
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.entidades);
        return sb.toString();
    }
    /**
     * Devolve o Map<String,Entidade> entidades da classe
     * @return Map<String,Entidade> entidades
     */
    public Map<String,Entidade> getEntidades(){
        return this.entidades.entrySet().stream().collect(Collectors.toMap(v->v.getKey(), v-> v.getValue().clone()));
    }
    /**
     * Atualiza a Map<String,Entidade> entidades da classe
     * @param m novo entidades da classe
     */
    public void setEntidades(Map<String,Entidade> m){
        this.entidades = m.entrySet().stream().collect(Collectors.toMap(v->v.getKey(), v-> v.getValue().clone()));
    }
    /**
     * Metodo que verifica se uma dada Entidade existe atraves do seu codigo
     * @param String cod
     * @return boolean resultante da verificaçao
     */
    public boolean existe(String cod){
        return this.entidades.containsKey(cod);
    }
    /** 
     * Metodo que adiciona uma nova Entidade ao Sistema
     * @param Entidade cod
     * @return boolean resultante da  inserçao
     */  
    public boolean adicionaEntidade(Entidade cod) throws AddEntidadeRepetidaException{
        if(this.entidades.containsKey(cod)) throw new AddEntidadeRepetidaException(cod + " já existe!");
        boolean ret = this.entidades.values().stream().anyMatch(v-> v.getEmail().equals(cod.getEmail()));
        if(!ret) this.entidades.put(cod.getCodigo(), cod.clone());
        return !ret;
    }
    /** 
     * Metodo que devolve uma entidade a partir do seu codigo
     * @param String cod
     * @return Entidade caso exista
     */
    public Entidade getEntidade(String cod) throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException(cod + " não existe!");
        return this.entidades.get(cod).clone();
    }
    /**
     * Metodo que verifica se umas dadas credencias estao corretas
     * @param String String email, String pass
     * @return String do codigo da Entidade caso as credenciais estejam certas
     */
    public String logIn(String email, String pass) throws CredenciaisErradasException{
        Iterator<Entidade> e = this.entidades.values().iterator();
        boolean found = false;
        Entidade temp = null;
        while(e.hasNext() && !found){
            temp = e.next();
            found = temp.getEmail().equals(email) && temp.getPassword().equals(pass);
        }
        if(!found)
            throw new CredenciaisErradasException("As credencias introduzidas são inválidas!");
        return temp.getCodigo();
    }

    /**
     * Metodo que dado um codigo de uma Entidade e uma funçao atualiza uma dada variavel da Entidade
     * @param String cod, Consumer<Entidade> c
     */  
    public void atualizaEntidade(String cod, Consumer<Entidade> c) throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Entidade não existe!");
        Entidade e = this.entidades.get(cod);
        c.accept(e);
    }
    /**
     * Metodo que dado um codigo de uma Loja e uma funçao se se tratar de uma Loja atualiza uma dada variavel da Loja
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaLoja(String cod, Consumer<Loja> c) throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Loja não existe!");
        Entidade e = this.entidades.get(cod);
        if(e instanceof Loja){
            Loja p = (Loja) e;
            c.accept(p);
        }
    }
    /**
     * Metodo que dado um codigo de um Utilizador e uma funçao se se tratar de um Utilizador atualiza uma dada variavel do Utilizador
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaUtilizador(String cod, Consumer<Utilizador> c) throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Utilizador não existe!");
        Entidade e = this.entidades.get(cod);
        if(e instanceof Utilizador){
            Utilizador p = (Utilizador) e;
            c.accept(p);
        }
    }
    /**
     * Metodo que dado um codigo de uma Entidade  e uma funçao se se tratar de um Voluntario atualiza uma dada variavel da Voluntario
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaVoluntario(String cod, Consumer<Voluntario> c) throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Voluntario não existe!");
        Entidade e = this.entidades.get(cod);
        if(e instanceof Voluntario){
            Voluntario p = (Voluntario) e;
            c.accept(p);
        }
    }
    /**
     * Metodo que dado um codigo de uma Transportadora e uma funçao atualiza uma dada variavel da Transportadora
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaTransportadora(String cod, Consumer<Transportadora> c) throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Transportadora não existe!");
        Entidade e = this.entidades.get(cod);
        if(e instanceof Transportadora){
            Transportadora p = (Transportadora) e;
            c.accept(p);
        }
    }
    /**
     * Metodo que devolve um Map<String,Integer> com a string do utilizador na key e no value o numero de encomendas
     * ja recebidas
     * @return Map<String,Integer>
     */  
    public Map<String,Integer> nrEncomendasPorUtilizador(){
        HashMap<String,Integer> n = new HashMap<>();
        for(Entidade entity : this.entidades.values())
            if(entity instanceof Utilizador){
                Utilizador transp = (Utilizador) entity;
                n.put(transp.getCodigo(), transp.nrEntregas());
            }
        return n;
    }
    /**
     * Metodo que devolve um Map<String,Double> com a string da Transportadora na key e no value o numero de quilometros
     * percorridos
     * @return Map<String,Double>
     */
    public Map<String,Double> kmPercorridosPorEmpresa(){
        HashMap<String,Double> n = new HashMap<>();
        for(Entidade entity : this.entidades.values())
            if(entity instanceof Transportadora){
                Transportadora transp = (Transportadora) entity;
                n.put(transp.getCodigo(), transp.kmPercorridos());
            }
        return n;
    }
    /**
     * Metodo que dado um Produto o adiciona a todas as lojas 
     * @param Produto p
     */
    public void adicionaProdutoLojas(Produto p){
        for(Entidade x : this.entidades.values())
            if(x instanceof Loja){
                Loja l = (Loja) x;
                l.adicionaProduto(p);
            }
    }

    /**
     * Metodo que dado uma encomenda devolve qual o objeto que implementa a interface ITransporte que se encontra mais perto 
     * para fazer a entrega excluindo aqueles que ja a rejeitaram que vem na String  
     * @param Encomenda enc , String igonre
     * @return objecto que implmenta a interface ITransporte
     */
  public ITransporte getMaisProximo(Encomenda enc, List<String> ignore) throws TransporteNaoExistenteException{
        ITransporte ret = null;
        boolean dispEntregasMedicas = false;
        ITransporte aux = null;
        boolean comp = false;
        for(Entidade p : this.entidades.values()){
            if(p instanceof ITransporteMedico){
                ITransporteMedico c = (ITransporteMedico) p;
                dispEntregasMedicas = c.aceitoTransporteMedicamentos();

            }
            if(p instanceof ITransporte){
                ITransporte t = (ITransporte) p;
                comp = !ignore.contains(t.getCodigo()) && (enc.getMedicas() == (p instanceof ITransporteMedico && dispEntregasMedicas)) && t.getEncomendas().size() < t.getCapacidade()&& t.getLivre() && t.getGps().distancia(enc.getLocalizacaoLoja()) <= t.getRaio() && t.getGps().distancia(enc.getLocalizacaoUtilizador()) <= t.getRaio();
                if(comp){
                    if(ret == null) ret = t;
                    else if(t.tempoTotal(enc) < ret.tempoTotal(enc)) ret = t; 
                }
            }
        } 
        if(ret == null) throw new TransporteNaoExistenteException("De momento não há transporte disponível.");
        if(ret instanceof Voluntario)
            aux = ((Voluntario) ret).clone();
        else if(ret instanceof Transportadora)
            aux = ((Transportadora) ret).clone();
        return aux;
    }
    /**
     * Metodo que dado um codigo de uma Entidade devolve as encomendas dessa mesma 
     * @param String cod
     * @return List<String> 
        */
    public List<String> getEncomendas(String cod)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Entidade não existe!");
        List<String> ret = new ArrayList<>();
        Entidade e = this.entidades.get(cod);
        if(e instanceof Loja){
            Loja p = (Loja) e;
            ret = p.getFila();
        }else if(e instanceof Voluntario){
            Voluntario p = (Voluntario) e;
            ret = p.getEncomendas();
        }else if(e instanceof Transportadora){
            Transportadora p = (Transportadora) e;
            ret = p.getEncomendas();
        }else if(e instanceof Utilizador){
            Utilizador p = (Utilizador) e;
            ret = p.getEncomendasPorAceitar();
        }
        return ret;
    }
    /**
     * Metodo que devolve uma lista com os codigos das lojas
     * @return List<String>
     */
    public List<String> getListLojas(){
        return this.entidades.entrySet().stream().filter(v->v.getValue() instanceof Loja).map(v-> v.getValue().getNomeCodigo()).collect(Collectors.toList());
    }
    /**
     * Metodo que dado o codigo de uma Entidade se se tratar de uma Voluntario ou Transportadora
     * simula a entrega de uma encomenda por parte de um Voluntario ou Transportadora
     * @param String transp, Encomenda en, long tempoInLoja
     * @return Duration que representa a duraçao da entrega
     */
    public Duration entregaEncomenda(String transp, Encomenda en, long tempoInLoja)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(transp)) throw new EntidadeNaoExistenteException("Transportadora não existe!");
        Entidade e = this.entidades.get(transp);
        Duration d = Duration.ZERO;
        if(e instanceof Transportadora)
            d = ((Transportadora) e).entregaEncomenda(en, tempoInLoja);
        else if (e instanceof Voluntario)
            d = ((Voluntario) e).entregaEncomenda(en,tempoInLoja);
        return d;
    }
    /**
     * Metodo que dado um codigo de uma Entidade se se tratar de uma Loja processa a proxima encomenda na fila da loja
     * @param String cod, String codEnc
     */
    public void processarProximaEnc(String cod, String codEnc)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Loja não existe!");
        Entidade e = this.entidades.get(cod);
        if(e instanceof Loja)
            ((Loja) e).processarProximaEnc(codEnc);
    }

    /**
     * Metodo que dado um codigo de uma Entidade se se tratar de uma Transportadora calcula o custo de uma dada Encomenda la presente 
     * @param String codt, Encomenda enc
     * @return Double que representa o preço
     */
    public double getCustoEncomenda(String codt, Encomenda enc)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(codt)) throw new EntidadeNaoExistenteException("Transportadora não existe!");
        Entidade e = this.entidades.get(codt);
        double ret = 0.0;
        if(e instanceof Transportadora)
            ret = ((Transportadora) e).custoDeEncomenda(enc);
        
        return ret;
    }
    /**
     * Metodo que dado um codigo de uma Entidade se se tratar de um Utilizador devolve a lista de RegistoEntrega 
     * @param String cod
     * @return List<RegistoEntregas>
     */
    public List<RegistoEntregas> getRegistosEntrega(String cod)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Transportadora não existe!");
        Entidade e = this.entidades.get(cod);
        List<RegistoEntregas> ret = new ArrayList<>();
        if(e instanceof Utilizador)
            ret =((Utilizador) e).getRegistos();
        return ret;
    }
    /**
     * Metodo que dado a String de uma Entidade se se tratar de uma Transportadora ou Voluntario
     * devolve a lista de RegistosTransporte 
     * @param String cod
     * @return List<RegistosTransporte>
     */ 
    public List<RegistosTransporte> encomendasFeitas(String cod)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Entidade não existe!");
        Entidade e = this.entidades.get(cod);
        List<RegistosTransporte> ret = new ArrayList<>();
        if(e instanceof Transportadora)
            ret = ((Transportadora) e).encomendasFeitas();
        else if(e instanceof Voluntario)
            ret = ((Voluntario) e).encomendasFeitas();
        return ret;
    }
    /**
     * Metodo que dada uma classificaçao e uma codigo de uma Entidade se se tratar de uma Transportadora ou Voluntario
     * coloca na sua Lista de classificaçoes a nova classificaçao
     * @param String codigo, int classificacao
     */
    public void adicionaClassificacao(String codigo, int classificacao)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(codigo)) throw new EntidadeNaoExistenteException("Entidade não existe!");
        Entidade e = this.entidades.get(codigo);
        if(e instanceof Voluntario)
            ((Voluntario) e).adicionaClassificacao(classificacao);
        else if(e instanceof Transportadora)
            ((Transportadora) e).adicionaClassificacao(classificacao);
    }
    /**
     * Metodo que dado o codigo de uma Entidade se se tratar de uma Transportadora verifica o total faturado num intervalo de tempo
     * @param  String codEmpresa, LocalDateTime t1, LocalDateTime t2
     * @return double que representa a faturaçao
     */
    public double faturacaoDeEmpresa(String codEmpresa, LocalDateTime t1, LocalDateTime t2)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(codEmpresa)) throw new EntidadeNaoExistenteException("Entidade não existe!");
        Entidade e = this.entidades.get(codEmpresa);
        double ret = 0.0;
        if(e instanceof Transportadora)
            ret = ((Transportadora) e).totalFaturado(t1,t2);
        return ret;
    }
    /**
     * Metodo que dado um codigo de uma Entidade se se tratar de uma Loja devolve o tempo de espera
     * @param String cod
     * @return long que representa o tempo de espera
     */
    public long getTempoInLoja(String cod)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Loja não existe!");
        long ret = 0;
        Entidade e = this.entidades.get(cod);
        if(e instanceof Loja){
            Loja p = (Loja) e;
            ret = p.tempoDeEspera();
        }
        return ret;
    }
    /**
     * Metodo que dado um codigo de uma Entidade se esta se tratar de um voluntario ou transportadora devolve a
     * List<Integer> correspondente a sua classificaçao
     * @param String cod 
     * @return List<Integer>
     */    
    public List<Integer> classificacoes(String cod) throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Entidade não existe!");
        Entidade e = this.entidades.get(cod);
        List<Integer> classi = new ArrayList<>();
        if(e instanceof Voluntario){
            Voluntario v = (Voluntario) e;
            classi = v.getClassificacoes();
        }else if(e instanceof Transportadora){
            Transportadora v = (Transportadora) e;
            classi = v.getClassificacoes();
        }
        return classi;
    }
     /**
     * Metodo que que dado um codigo se se tratar de um objeto que implementa a interface  ITransportadora atualiza 
     * atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaTransporte(String cod, Consumer<ITransporte> c)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Entidade não existe!");
        Entidade e = this.entidades.get(cod);
        if(e instanceof ITransporte){
            ITransporte p = (ITransporte) e;
            c.accept(p);
        }
    }
    /**
     * Metodo que que dado um codigo se se tratar de um objeto que implementa a interface  ITransportadoraMedica atualiza 
     * atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaTransporteMedico(String cod, Consumer<ITransporteMedico> c)throws EntidadeNaoExistenteException{
        if(!this.entidades.containsKey(cod)) throw new EntidadeNaoExistenteException("Entidade não existe!");
        Entidade e = this.entidades.get(cod);
        if(e instanceof ITransporteMedico){
            ITransporteMedico p = (ITransporteMedico) e;
            c.accept(p);
        }
    }
    /**
     * Metodo que verifica se um email ja existe 
     * @param String que representa o email
     * @return boolean resultante da verificaçao
     */
    public boolean checkEmail(String email){
        return this.entidades.values().stream().anyMatch(v-> v.getEmail().equals(email));
    }
}
