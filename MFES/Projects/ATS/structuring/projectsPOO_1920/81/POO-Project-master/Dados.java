
import java.util.*;
import java.util.stream.*;
import java.io.*;
import java.time.LocalDate;
import java.text.*;
/**
 * Dados - Registo com todos os produtos, utilizadores, lojas,voluntarios e transportadoras.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Dados implements Serializable{
    private Map<String, Utilizador> utilizadores; //codigo utilizador
    private Map<String, Voluntarios> voluntarios; // codigo voluntario
    private Map<String, Transportadora>transportadoras; //codigo empresa
    private Map<String, Loja> lojas; //codigo produto, loja
    private Map<String, Produto> produtos;
    private Map<String, Encomenda> encomendas;
    /**
     * Construtor para objetos da classe Dados (por omissao)
     */
    public Dados(){
        this.utilizadores = new TreeMap<>();
        this.voluntarios = new TreeMap<>();
        this.transportadoras = new TreeMap<>();
        this.lojas = new TreeMap<>();
        this.produtos = new TreeMap<>();
        this.encomendas = new TreeMap<>();
    }
    
    /**
     * Construtor para objetos da classe Dados (parameterizado)
     * 
     * @param  utl   os utilizadores
     * @param  vol   os voluntarios
     * @param  tr   as transportadoras
     * @param  loj   as lojas
     * @param  p   os produtos
     * @param  en  encomendas
     */
    public Dados(Map<String, Utilizador> utl, Map<String, Voluntarios> vol,
                    Map<String, Transportadora> tr,Map<String, Loja> loj, Map<String, Produto> p, Map<String, Encomenda> en){
          this.setUtilizadores(utl);
          this.setVoluntarios(vol);
          this.setTransportadoras(tr);
          this.setLojas(loj);
          this.setAllProdutos(p);
          this.setAllEncomendas(en);
    }
        
    /**
     * Construtor para objetos da classe Dados (de copia)
     * 
     * @param  d   os dados
     */
    
    public Dados(Dados d){
        this.utilizadores = d.getUtilizadores();
        this.voluntarios = d.getVoluntarios();
        this.transportadoras = d.getTransportadoras();
        this.lojas = d.getLojas();
        this.produtos = d.getAllProdutos();
        this.encomendas = d.getAllEncomendas();
    }
    
    /**
     * Metodo que devolve os utilizadores de um conjunto de dados
     * 
     * @return     os utilizadores dos dados
     */
    public Map<String, Utilizador> getUtilizadores(){
        Map<String, Utilizador> aux = new TreeMap<>();
        for(String em : this.utilizadores.keySet()){
            Utilizador c = this.utilizadores.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }
    
    /**
     * Metodo que devolve os voluntarios de um conjunto de dados
     * 
     * @return     os voluntarios dores dos dados
     */
    public Map<String, Voluntarios> getVoluntarios(){
        Map<String, Voluntarios> aux = new TreeMap<>();
        for(String em : this.voluntarios.keySet()){
            Voluntarios c = this.voluntarios.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }
    
    /**
     * Metodo que devolve as transportadoras de um conjunto de dados
     * 
     * @return     as transportadoras dos dados
     */
    public Map<String, Transportadora> getTransportadoras(){
        Map<String, Transportadora> aux = new TreeMap<>();
        for(String em : this.transportadoras.keySet()){
            Transportadora c = this.transportadoras.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }
    
    /**
     * Metodo que devolve as lojas de um conjunto de dados
     * 
     * @return     as lojas dos dados
     */
    public Map<String, Loja> getLojas(){
        Map<String, Loja> aux = new TreeMap<>();
        for(String em : this.lojas.keySet()){
           Loja c = this.lojas.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }
    
    /**
     * Metodo que devolve os produtos de um conjunto de dados
     * 
     * @return     os produtos dos dados
     */
    public Map<String, Produto> getAllProdutos(){
        Map<String, Produto> aux = new TreeMap<>();
        for(String em : this.produtos.keySet()){
           Produto c = this.produtos.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }
    
    /**
     * Metodo que devolve as encomendas de um conjunto de dados
     * 
     * @return     as encomendass dos dados
     */
    public Map<String, Encomenda> getAllEncomendas(){
        Map<String, Encomenda> aux = new TreeMap<>();
        for(String em : this.encomendas.keySet()){
           Encomenda c = this.encomendas.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }
    
    /**
     * Metodo que altera os utilizadores de um conjunto de cados
     * 
     * @param  utl   os novos utilizadores
     */
    public void setUtilizadores(Map<String, Utilizador> utl){
        this.utilizadores = utl.values().stream()
                           .collect(Collectors
                           .toMap((utilizador -> utilizador.getCod()), (utilizador) -> utilizador.clone()));
    }
    
    /**
     * Metodo que altera os voluntarios de um conjunto de cados
     * 
     * @param  vol   os novos voluntarios
     */
    public void setVoluntarios(Map<String, Voluntarios> vol){
        this.voluntarios = vol.values().stream()
                           .collect(Collectors
                           .toMap((voluntarios -> voluntarios.getCod()), (voluntarios) -> voluntarios.clone()));
    }
    
    /**
     * Metodo que altera as transpoertadoras de um conjunto de cados
     * 
     * @param  tr   as novas transportadoras
     */
    public void setTransportadoras(Map<String, Transportadora> tr){
        this.transportadoras = tr.values().stream()
                           .collect(Collectors
                           .toMap((transportadora -> transportadora.getCod()), (transportadora) -> transportadora.clone()));
    }
    
    /**
     * Metodo que altera as lojas de um conjunto de cados
     * 
     * @param  loj   as novas lojas
     */
    public void setLojas(Map<String, Loja> loj){
        this.lojas = loj.values().stream()
                           .collect(Collectors
                           .toMap((loja -> loja.getCod()), (loja) -> loja.clone()));
    }
    
    /**
     * Metodo que altera os produtos de um conjunto de cados
     * 
     * @param  p   os novos produtos
     */
    public void setAllProdutos(Map<String, Produto> p){
        this.produtos = p.values().stream()
                           .collect(Collectors
                           .toMap((produto -> produto.getCodProduto()), (produto) -> produto.clone()));
    }
    
     /**
     * Metodo que altera as encomendas de um conjunto de cados
     * 
     * @param  en   as novas encomendas
     */
    public void setAllEncomendas(Map<String, Encomenda> en){
        this.encomendas = en.values().stream()
                           .collect(Collectors
                           .toMap((encomenda -> encomenda.getCodEncomenda()), (encomenda) -> encomenda.clone()));
    }
   
    /**
     * Metodo que duplica os dados
     * 
     * @return   o clone dos dados
     */
    public Dados clone(){
        return new Dados(this);
    }
    
    /**
     * Metodo que verifica se dois conjuntos de dados sao iguais
     * 
     * @param  o   o objeto a comparar
     * 
     * @return     o resultado da comparacao dos objetos
     */
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;
        else{
            Dados d = (Dados) o;
            return this.utilizadores.equals(d.getUtilizadores()) &&
                   this.voluntarios.equals(d.getVoluntarios()) && 
                   this.transportadoras.equals(d.getTransportadoras()) && 
                   this.lojas.equals(d.getLojas()) && 
                   this.produtos.equals(d.getAllProdutos());
        }
    }
    
    /**
     * Metodo que converte um conjunto de dados para uma string
     * 
     * @return     o conjunto de dados em string
     */
    public String toString(){
        String aux = new String();
        for(Utilizador u : this.utilizadores.values())
            aux += u.toString();
        for(Voluntarios v : this.voluntarios.values())
            aux += v.toString();
        for(Transportadora t : this.transportadoras.values())
            aux += t.toString();
        for(Loja l : this.lojas.values())
            aux += l.toString();
        for(Produto p : this.produtos.values())
            aux += p.toString();
            return aux;
    }
    
    /**
     * Metodo que devolve o codigo de hash para um loja
     * 
     * @return     o hashcode
     */
    public int hashCode(){
        int hash = 7; 
        for(Utilizador u : this.utilizadores.values())
            hash = 31 * hash + u.hashCode();
        for(Voluntarios v : this.voluntarios.values())
            hash = 31 * hash + v.hashCode();
        for(Transportadora t : this.transportadoras.values())
            hash = 31 * hash + t.hashCode();
        for(Loja l : this.lojas.values())
            hash = 31 * hash + l.hashCode();
        for(Produto p : this.produtos.values())
            hash = 31 * hash + p.hashCode();
            return hash;
    }
    
     /*
                *******METODOS DE REGISTO*******
     */  
    
    /**
     * Metodo que insere um Utilizador no conjunto de dados
     * 
     * @param  utl   o novo utilizador
     */
    
    public void registarUtilizador (Utilizador utl){
        this.utilizadores.put(utl.getCod(), utl.clone());
    }
    
    /**
     * Metodo que insere um Voluntario no conjunto de dados
     * 
     * @param  v   o novo voluntario
     */
    
    public void registarVoluntario (Voluntarios v){
        this.voluntarios.put(v.getCod(), v.clone());
    }
    
    /**
     * Metodo que insere uma Transportadora no conjunto de dados
     * 
     * @param  t   a nova transportadora
     */
    
    public void registarTransportadora (Transportadora t){
        this.transportadoras.put(t.getCod(), t.clone());
    }
    
    /**
     * Metodo que insere uma Loja no conjunto de dados
     * 
     * @param  l   a nova loja
     */
    
    public void registarLoja (Loja l){
        this.lojas.put(l.getCod(), l.clone());
    }
    /**
     * Metodo que insere uma Encomenda no conjunto de dados
     *
     * @param  e   a nova Encomenda
     */
    public void  registarEncomenda(Encomenda e){this.encomendas.put(e.getCodEncomenda(),e.clone());}

    /**
     * Metodo que insere um Produto num conjunto de dados
     * 
     * @param  p   o Produto a ser inserido
     * @param  loj   a loja que o vende
     */
    public void insereProduto(Produto p, Loja loj){
        Loja l = this.lojas.get(loj.getEmail());
        l.adicionaProduto(p.clone());
        this.lojas.put(l.getEmail(), l.clone());
        this.produtos.put(p.getCodProduto(), p.clone());
    }
    
    public void insereEncomendaLoj(String email, String codEncomenda, Encomenda e){
        Loja l = this.lojas.get(email);
        if(l != null){
            l.insereEncomenda(e.clone());
            for(Encomenda en : l.getEncomendas())
                if(en.getCodEncomenda().equals(codEncomenda)){
                    l.insereEncomenda(e.clone());
                    this.registarLoja(l.clone());
                    break;
                }
        }  
    }
    
    public void insereEncomendaUtilizador(String email, Encomenda e){
        Utilizador u = this.utilizadores.get(email);
        if(u != null){
            u.insereEncomenda(e.clone());
            this.registarUtilizador(u.clone());
        }
    }
    public void insereEntregaTransportadora(String email, Entregas e){
        Transportadora t = this.transportadoras.get(email);
        if(t != null){
            t.dispostoFazerEntrega();
            this.registarTransportadora(t.clone());
        }
    }
    public void insereEntregaVoluntario(String email, Entregas e){
        Voluntarios v = this.voluntarios.get(email);
        if(v != null){
            v.dispostoFazerEntrega();
            this.registarVoluntario(v.clone());
        }
    }
    
     /*
                *******METODOS DE VALIDACAO DE LOGIN*******
     */   
    /**
     * Metodo que valida o login de um utilizador
     * 
     * @param  email   o email do utilizador
     * @param  pass   a password do utilizador
     * 
     * @return  true se os dados forem validos, false em caso contrario
     */
    public boolean loginUtilizador(String cod,String email, String pass){
        if(this.utilizadores.get(cod).dadosValidosUtilizador(cod,email, pass))
            return true;
        return false;
    }
    /**
     * Metodo que valida o login de um voluntario
     * 
     * @param  email   o email do voluntario
     * @param  pass   a password do voluntario
     * 
     * @return  true se os dados forem validos, false em caso contrario
     */
    public boolean loginVoluntario(String email, String pass){
        if(this.voluntarios.get(email).dadosValidosVoluntario(email, pass) == true)
            return true;
        return false;
    }
    /**
     * Metodo que valida o login de uma Empresa
     * 
     * @param  email   o email da Empresa
     * @param  pass   a password da Empresa
     * 
     * @return  true se os dados forem validos, false em caso contrario
     */
    public boolean loginEmpresaTransportadora(String email, String pass){
        if(this.transportadoras.get(email).dadosValidosEmpresa(email, pass) == true)
            return true;
        return false;
    }
    /**
     * Metodo que valida o login de uma loja
     * 
     * @param  email   o email da loja
     * @param  pass   a password da loja
     * 
     * @return  true se os dados forem validos, false em caso contrario
     */
    public boolean loginLoja(String email, String pass){
        if(this.lojas.get(email).dadosValidosLoja(email, pass) == true)
            return true;
        return false;
    }
    
   
    /**
     * Metodo que verifica a existencia do email 
     * 
     * @param  email   o email 
     * 
     * @return  true se o email existir, false em caso contrario
     */
    public boolean existeEmail (String email) throws EmailInexistenteException {
        Utilizador u = this.utilizadores.get(email);
        
        if (u == null) {
            Loja l = this.lojas.get(email);
                if(l == null){
                    Transportadora t = this.transportadoras.get(email);
                    if(t == null){
                       Voluntarios v = this.voluntarios.get(email);
                        if(v == null)
                        throw new EmailInexistenteException(email);
                        else
                        return true;
                    }
                }
        }
        return false;
    }
    
    /**
     * Metodo que verifica a existencia do userCode 
     * 
     * @param  codigo   o codigo
     * 
     * @return  true se o codigo existir, false em caso contrario
     */
    public boolean existeCodigo (String codigo) throws CodigoInexistenteException {
        Utilizador u = this.utilizadores.get(codigo);
        
        if (u == null) {
            Loja l = this.lojas.get(codigo);
                if(l == null){
                    Transportadora t = this.transportadoras.get(codigo);
                    if(t == null){
                       Voluntarios v = this.voluntarios.get(codigo);
                        if(v == null)
                        throw new CodigoInexistenteException(codigo);
                        else
                        return true;
                    }
                }
        }
        return false;
    }
    
    /**
     * Metodo que verifica a existencia de um produto
     * 
     * @param  prod   o produto
     * 
     * @return  true se o produto existir, false em caso contrario
     */
    public boolean existeProduto (String prod) throws ProdutoInexistenteException {
        Produto p = this.produtos.get(prod);
        
        if (p == null){ 
            throw new ProdutoInexistenteException(prod);
        }
        else
            return true;
    }
    
    /**
     * Metodo que verifica a existencia de um voluntario
     * 
     * @param  vol   o voluntario
     * 
     * @return  true se o produto existir, false em caso contrario
     */
    public boolean existeVoluntarios(String vol) throws VoluntariosInexistentesException {
        Voluntarios v = this.voluntarios.get(vol);
        if(v == null){
            throw new VoluntariosInexistentesException(vol);
        }
        else
            return true;
    }

    /*
                *******METODOS AUXILIARES*******
     */   
    
    public List<Utilizador> top10_Utilizacao() {
        List<Utilizador> top10 = new ArrayList<Utilizador>();
        top10 = this.getUtilizadores().values().stream().collect(Collectors.toList());
        Collections.sort(top10, new ComparadorUtilizacao());
        Collections.reverse(top10);
        top10 = top10.stream().limit(10).collect(Collectors.toList());
        return top10;
    }
    
    public List<Transportadora> top10_Distancia() {
        List<Transportadora> top10 = new ArrayList<Transportadora>();
        top10 = this.getTransportadoras().values().stream().collect(Collectors.toList());
        Collections.sort(top10, new ComparadorDistancia());
        Collections.reverse(top10);
        top10 = top10.stream().limit(10).collect(Collectors.toList());
        return top10;
    }
    
    /**
     * Metodo que dado o email, diz-nos a que utilizador corresponde o email, caso corresponda
     * 
     * @param  email o email a testar
     * 
     * @return Utilizador caso corresponda o email, null caso contrario
     */
    public Utilizador mailToUser(String email){
        Map<String,Utilizador> aux = this.getUtilizadores();
        Utilizador u = aux.get(email);
        return u;
    }

    public Utilizador codeToUser( String code){
        Map<String,Utilizador> aux = this.getUtilizadores();
        Utilizador u = aux.get(code);
        return u;
    }
    
    /**
     * Metodo que dado o produto, diz-nos a que Loja corresponde, caso corresponda
     * 
     * @param  codProduto o produto a testar
     * 
     * @return Loja caso corresponda o produto, null caso contrario
     */
    public Loja produtoToloja(String codProduto){
        List<Produto> ps = new ArrayList<Produto>();
        Loja n = null;
        for(Loja l : this.lojas.values()){
            for(Produto prod : l.getProdutos()){
                if(codProduto.equals(prod.getCodProduto())){
                    n = l.clone();
                    break;
                }
            }
            if(n != null)
                break;
        }
        return n;    
    }
    
    /**
     * Metodo que dado o email, diz-nos a que Loja corresponde, caso corresponda.
     * 
     * @param  email  o email a testar
     * 
     * @return Loja caso corresponda o email, null caso contrario
     */
    public Loja mailToLoja(String email){
        Map<String, Loja> aux = this.getLojas();
        Loja l = aux.get(email);
        return l;
     }
    public Loja codeToLoja(String codigo){
        Map<String, Loja> aux = this.getLojas();
        Loja l = aux.get(codigo);
        return l;
    }


    /**
     * Metodo que dado o email, diz-nos a que Empresa corresponde, caso corresponda.
     * 
     * @param  email  o email a testar
     * 
     * @return empresa caso corresponda o email, null caso contrario
     */
    public Transportadora mailToEmpresa(String email){
        Map<String, Transportadora> aux = this.getTransportadoras();
        Transportadora t = aux.get(email);
        return t;
     }
    public Transportadora codeToEmpresa(String codigo){
        Map<String, Transportadora> aux = this.getTransportadoras();
        Transportadora t = aux.get(codigo);
        return t;
    }

    /**
     * Metodo que dado o email, diz-nos a que Voluntario corresponde, caso corresponda.
     * 
     * @param  email  o email a testar
     * 
     * @return Voluntarios caso corresponda o email, null caso contrario
     */
    public Voluntarios mailToVoluntario(String email){
        Map<String, Voluntarios> aux = this.getVoluntarios();
        Voluntarios v = aux.get(email);
        return v;
     }
    public Voluntarios codeToVoluntario(String codigo){
        Map<String, Voluntarios> aux = this.getVoluntarios();
        Voluntarios v = aux.get(codigo);
        return v;
    }
     
      /**
     * Metodo que dado o codigo, diz-nos a que Produto corresponde, caso corresponda.
     * 
     * @param  cod  o cod a testar
     * 
     * @return Produto caso corresponda o codigo, null caso contrario
     */
    public Produto codigoToProduto(String cod){
        Map<String, Produto> aux = this.getAllProdutos();
        Produto p = aux.get(cod);
        return p;
    }
    
      /**
     * Metodo que dado o codigo, diz-nos a que Encomenda corresponde, caso corresponda.
     * 
     * @param  cod  o cod a testar
     * 
     * @return Encomenda caso corresponda o codigo, null caso contrario
     */
    public Encomenda codigoToEncomenda(String cod){
        Map<String, Encomenda> aux = this.getAllEncomendas();
        Encomenda e = aux.get(cod);
        return e;
    }
     
    /**
     * Metodo que devolve a distancia entre dois pontos 
     * 
     * @param  x1 a coordenada x atual
     * @param  y1 a coordenada y atual
     * @param  x2 a coordenada x destino
     * @param  y2 a coordenada y destino
     * 
     * @return dist percorrida de um ponto ao outro
     */
    public double dist(double x1, double y1, double x2, double y2) {       
        return Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1));
    }
    
    public List<Produto> produtosDisponiveis() throws ProdutoInexistenteException{
        List<Produto> disponiveis = new ArrayList<Produto>();
        Map<String, Produto> pro = this.getAllProdutos();
        disponiveis = pro.values().stream().filter(l->l.getDisponibilidadeLoja()).collect(Collectors.toList());
        
        if(disponiveis.isEmpty())
            throw new ProdutoInexistenteException();
        
            return disponiveis;
    }
    
    /**
     * Metodo que devolve a loja mais proxima das coordenadas x e y  
     * 
     * @param  disponiveis  lista de lojas da app
     * @param  u  Utilizador que procura a loja mais proxima
     * @param  x a coordenada x atual
     * @param  y a coordenada y atual
     * 
     * @return proximo  loja mais proxima
     */
    public Voluntarios proximo (List<Voluntarios> disponiveis, Utilizador u, double x, double y){
        double minimo = Double.MAX_VALUE, d;
        Voluntarios vol = null;
        
        for(Voluntarios v : disponiveis){
            d = dist(x,y, v.getGPSx(), v.getGPSy());
            if(d < minimo){
                minimo = d;
                vol = v.clone();
            }
        }
        return vol;
    }
    
    /**
     * Metodo que devolve o produto mais barato das coordenadas x e y  
     * 
     * @param  disponiveis  lista de produtos da app
     * @param  u  Utilizador que procura o produto mais barato
     * 
     * @return proximo  loja mais proxima
     */
    public Produto barato (List<Produto> disponiveis, Utilizador u){
        double minimo = Double.MAX_VALUE, d;
        Produto prod = null;
        for (Produto p : disponiveis){
            d = p.getPreco();
            if(d < minimo){
                minimo = d;
                prod = p.clone();
            }
        }
        return prod;
    }
    
    /**
     * Metodo que devolve o produto mais barato numa condicionante de distancia  
     * 
     * @param  disponiveis  lista de produtos da app
     * @param  u  Utilizador que procura o produto mais barato em conta a distancia 
     * @param  x a coordenada x atual
     * @param  y a coordenada y atual
     * @param  dist a distancia entre o utilizador e a loja que vende o produto pretendido
     * 
     * @return baratoProximo  o produto mais barato numa condicionante de distancia  
     */
    public Produto baratoProximo (List<Produto> disponiveis, Utilizador u, double x, double y, double dist){
        double minimo = Double.MAX_VALUE, dpreco, d;
        Loja l = null;
        Produto prod = null;
        for (Produto p : disponiveis){
            d = dist(x,y, l.getGPSx(), l.getGPSy());
            dpreco = p.getPreco();
                if(dpreco < minimo && dist >= d){
                minimo = dpreco;
                prod = p.clone();
                }
            
        }
        return prod;
    }
    
    
    /*
                *******METODOS DE VALIDACAO*******
     */ 
    /**
     * Metodo que valida o email
     * 
     * @param  email   o email
     * 
     * @return  true se o email for valido, false em caso contrario
     */
    public boolean emailValido (String email) throws EmailInvalidoException {
        try {
            existeEmail(email);
        }
        catch (EmailInexistenteException e) {
            if ((email.contains(".com") || email.contains(".pt")) && email.contains("@") )
                return true;
            else 
                throw new EmailInvalidoException(email);
        }
            
        return false;
    }
    /**
     * Metodo que verifica se um codigo e valido
     * 
     * @param  codigo o codigo a testar
     * 
     * @return true caso corresponda a um codigo verdadeiro, false caso contrario
     */
    public boolean codigoValido (String codigo) throws CodigoInvalidoException {
       try {
           existeCodigo(codigo);
        }
        catch (CodigoInexistenteException c){
            if ((codigo.contains("u")) ||(codigo.contains("v")) || (codigo.contains("l")) || (codigo.contains("t")))
                return true;
            else 
                throw new CodigoInvalidoException(codigo);
            }
            return false;
   }
        /*if (codigo.equals(code))
            return true;
        else
            throw new CodigoInvalidoException(code);*/
    
    /**
     * Metodo que verifica se um nome e valido
     * 
     * @param  nome o nome a testar
     * 
     * @return true caso corresponda a um nome verdadeiro, false caso contrario
     */
    public boolean nomeValido (String nome) throws NomeInvalidoException {
        int i;
        char c;
        
        for (i = 0; i < nome.length(); i++) {
            c = nome.charAt(i);
            if (!Character.isLetter(c) && c != ' ')
                throw new NomeInvalidoException(nome);
        }

        return true;
    }
    
    /**
     * Metodo que verifica se a password de confirmaçao coincide com a inserida 
     * 
     * @param  password a password inserida
     * @param  pass a password a confirmar se coincide
     * 
     * @return true caso correspondam, false caso contrario
     */
    public boolean passwordValida (String password, String pass) throws PasswordInvalidaException {
        if (password.equals(pass))
            return true;
        else
            throw new PasswordInvalidaException(pass);
    }
    
    
    
    /*
                *******METODOS QUE ENVOLVEM FICHEIROS*******
     */  
    
    /**
     * Metodo que abre um ficheiro
     * 
     * @return Dados do ficheiro lido
     */



     public Dados abrirFicheiro(String nomeFicheiro) throws FileNotFoundException, IOException, ClassNotFoundException, ClassCastException{

         FileInputStream in = new FileInputStream(nomeFicheiro);
         parser(nomeFicheiro);
        in.close();
        return this;
    }

    public void parser(String fich){
        try {
            BufferedReader reader = new BufferedReader(new FileReader(fich));
            String conteudo = "";
            for(conteudo=reader.readLine();conteudo!=null;conteudo=reader.readLine()){
                adicionaLinha(conteudo);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Metodo que guarda os dados num ficheiro
     * 
     */
    public void guardaFicheiro(String nomeFicheiro) throws FileNotFoundException, IOException{
        FileOutputStream out = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream o = new ObjectOutputStream(out);
        o.writeObject(this);
        o.flush();
        o.close();
    }

    public void carregaLogs(String fich) throws FileNotFoundException, IOException{
        List<String> dadosS = Dados.lerLogs(fich);
        dadosS.forEach(this::adicionaLinha);
    }

    private static List<String> lerLogs(String fich) throws FileNotFoundException, IOException{
        List<String> linhas = new ArrayList<>();
        BufferedReader br = new BufferedReader(new FileReader(fich));
        String linha;

        while((linha = br.readLine()) != null && !(linha.equals("Logs")));
        while((linha = br.readLine()) != null && !(linha.equals("\n"))){
            linhas.add(linha);
        }
        br.close();
        return linhas;
    }

    private void adicionaLinha(String csv){
        String[] divisor;
        String tipo, aux, cod, cod1, cod2, nome, email, morada;
        int nif, estado;
        double cx, cy, raio ,peso, precoKm;

        divisor = csv.split(":");
        tipo = divisor[0];
        aux = divisor[1];
        StringBuilder sb = new StringBuilder();
        //this.registarAceite(ea);
        switch (tipo) {
            case "Utilizador" : {
                divisor = aux.split(",");
                cod = divisor[0];
                email = sb.append(divisor[0] + "@mail.pt").toString();
                nome = divisor[1];
                cx = Double.parseDouble(divisor[2]);
                cy = Double.parseDouble(divisor[3]);
                Utilizador u = new Utilizador(cod, email, nome, "na", "desconhecida", LocalDate.now(), -1, cx, cy, -1, new ArrayList<>());
                this.registarUtilizador(u);
            }
            case "Voluntario" : {
                divisor = aux.split(",");
                cod = divisor[0];
                email = sb.append(divisor[0] + "@mail.pt").toString();
                nome = divisor[1];
                cx = Double.parseDouble(divisor[2]);
                cy = Double.parseDouble(divisor[3]);
                raio = Double.parseDouble(divisor[4]);
                Voluntarios v = new Voluntarios(cod, email, nome, "na", cx, cy, -1, raio, new ArrayList<>(), new ArrayList<>());
                this.registarVoluntario(v);
            }
            case "Transportadora" : {
                divisor = aux.split(",");
                cod = divisor[0];
                email = sb.append(divisor[0] + "@mail.pt").toString();
                nome = divisor[1];
                cx = Double.parseDouble(divisor[2]);
                cy = Double.parseDouble(divisor[3]);
                nif = Integer.parseInt(divisor[4]);
                raio = Double.parseDouble(divisor[5]);
                precoKm = Double.parseDouble(divisor[6]);
                Transportadora t = new Transportadora(cod, email, nome, "na", cx, cy,
                        nif, raio, -1, -1, precoKm, -1, new ArrayList<>(), new ArrayList<>());
                this.registarTransportadora(t);
            }
            case "Loja" : {
                divisor = aux.split(",");
                cod = divisor[0];
                email = sb.append(divisor[0] + "@mail.pt").toString();
                nome = divisor[1];
                Loja l = new Loja(cod, email, nome, "na", -1, -1, -1, new ArrayList<>(), new ArrayList<>());
                this.registarLoja(l);
            }
            case "Encomenda" : {
                divisor = aux.split(",");
                cod = divisor[0];
                cod1 = divisor[1];
                cod2 = divisor[2];
                peso = Double.parseDouble(divisor[3]);
                Encomenda e = new Encomenda(cod, cod1, cod2, peso, new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), -1, "normal");
                this.registarEncomenda(e);
            }
            case "Aceite" : {
                divisor = aux.split(",");
                cod = divisor[0];
                EncomendaAceite ea = new EncomendaAceite(cod, "na", "na", -1, new ArrayList<>(), new ArrayList<>(), new ArrayList<>(),-1, "normal");
            }
            default : System.out.println("Linha inválida!");
        }
            System.out.println("DONE!");
    }

    /**
     * Metodo que devolve o tempo resultante do deslocamento entre dois pontos
     * 
     * @param  x a coordenada x atual
     * @param  y a coordenada x atual
     * @param  xDestino a coordenada x do destino
     * @param  yDestino a coordenada y do destino
     * 
     * @return tempo do deslocamento entre dois pontos
     */
     public double tempoVol (double x, double y, double xDestino, double yDestino){
         double distancia = this.dist(x, y, xDestino, yDestino);
         return distancia/(5/3600); //velocidade default de uma pessoa a andar a pe (5km/h)
    }
    
    /**
     * Metodo que devolve o tempo resultante do deslocamento entre dois pontos
     * 
     * @param  x a coordenada x atual
     * @param  y a coordenada x atual
     * @param  xDestino a coordenada x do destino
     * @param  yDestino a coordenada y do destino
     * 
     * @return tempo do deslocamento entre dois pontos
     */
     public double tempoTrans(double x, double y, double xDestino, double yDestino){
         double distancia = this.dist(x, y, xDestino, yDestino);
         return distancia/(80/3600); //velocidade default de um camiao da empresa transportadora (80km/h)
    }
    
    public void removeEncomenda (String emailUser, String emailLoja, String codProd, Encomenda e){
        Produto p = this.produtos.get(codProd);
        Loja loj = this.lojas.get(emailLoja);
        Utilizador u = this.utilizadores.get(emailUser);
        
        List<Encomenda> lista = p.getEncomendas();
        lista.remove(e);
        p.setEncomendas(lista);
        
        List<Encomenda> list = u.getEncomendas();
        list.remove(e);
        loj.setEncomendas(list);
            
        List<Encomenda> lis = u.getEncomendas();
        lis.remove(e);
        u.setEncomendas(lis);
    }    
    
    public void alteraAntesEntrega(Entregas en, String emailUser, String emailLoja, String codEncomenda, Encomenda e){
        Encomenda enc = this.encomendas.get(codEncomenda);
        Loja loj = this.lojas.get(emailLoja);
        
        this.registarLoja(loj);
        
        this.insereEncomendaUtilizador(emailUser, e.clone());
        this.insereEncomendaLoj(emailLoja, codEncomenda, e.clone());
        this.insereEntregaTransportadora(en.getCodeEmpresa(),en.clone());
        this.insereEntregaVoluntario(en.getCodeEmpresa(), en.clone());
    }
}