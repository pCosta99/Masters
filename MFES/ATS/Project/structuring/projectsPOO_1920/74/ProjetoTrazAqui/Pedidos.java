import java.util.Map;
import java.util.TreeMap;
import java.util.HashMap;
import java.util.stream.Stream;
import java.util.List;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.io.Serializable;
import java.util.function.Predicate;
public class Pedidos implements Serializable{
    private Map<String, Pedido> pedidos;
    /**
     * Construtor por omissao da classe
     */
    public Pedidos(){
        this.pedidos = new HashMap<>();
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros Map<String,Pedido>
     */ 
    public Pedidos(Map<String, Pedido> pedidos){
        this.pedidos = new HashMap<>();
        for(Map.Entry<String, Pedido> p : pedidos.entrySet())
            this.pedidos.put(p.getKey(), p.getValue().clone());
    }
    /**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */
    public Pedidos(Pedidos p){
        this.pedidos = p.getPedidos();
    }
    /**
     * Devolve o Map<String,Pedido> entidades da classe
     * @return Map<String,Pedido> entidades
     */
    public Map<String, Pedido> getPedidos(){
        Map<String, Pedido> ret = new HashMap<String,Pedido>();
        for(Map.Entry<String, Pedido> p : this.pedidos.entrySet())
            ret.put(p.getKey(), p.getValue().clone());
        return ret;
    }
     /**
     * Atualiza a Map<String,Pedido> entidades da classe
     * @param pedidos novo pedidos da classe
     */
    public void setPedidos(Map<String, Pedido> pedidos){
        this.pedidos = new HashMap<>();
        for(Map.Entry<String, Pedido> p : pedidos.entrySet())
            this.pedidos.put(p.getKey(), p.getValue().clone());
    }
    /**
     * Faz clone da classe
     * @return o clone da classe
     */
    public Pedidos clone(){
        return new Pedidos(this);
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
        Pedidos p = (Pedidos) o;
        return this.pedidos.equals(p.getPedidos());
    }
     /**
     * Metodo que coloca numa String algumas variaveis de instancia da Encomenda
     */ 
    public String toString(){
        StringBuilder sb = new StringBuilder();
        for(Pedido enc :  this.pedidos.values())
            sb.append(enc).append("\n");
        return sb.toString();
    }

    /**
     * Metodo que procura se uma dada encomenda existe na classe
     * @param String que representa o codigo da encomenda
	 * @return boolean resultante da verificaçao
     */
    public boolean existeEncomenda(String cod){
        return this.pedidos.containsKey(cod);
    }
    /**
     * Metodo que adiciona um pedido à classe
     * @param Pedido enc
     */
    public void adicionaPedido(Pedido enc){
        this.pedidos.put(enc.getCodEncomenda(),enc.clone());
    }
    /**
     * Metodo que dado uma coordenada e um raio verifica que encomendas se encontram nesse raio
     * @param Coordenada c, double raio
     * @return List<Encomenda>
     */
    public List<Encomenda> encomendasPossiveis(Coordenada c, double raio){
        return this.pedidos.values().stream()
                                    .map(v->v.getEncomenda())
                                    .filter(v -> v.getLocalizacaoLoja().distancia(c) < raio && v.getLocalizacaoUtilizador().distancia(c) < raio)
                                    .collect(Collectors.toList());
    }
    /**
     * Metodo que dado uma Encomenda remove o pedido que diz respeito a esta
     * @param Encomenda e
     */
    public void removePedido(Encomenda e){
        this.pedidos.remove(e.getCodEncomenda());
    }
    /**
     * Metodo que dado um codigo de uma Encomenda remove o pedido que diz respeito a esta
     * @param String e
     */
    public void removePedido(String e){
        this.pedidos.remove(e);
    }
    /**
     * Metodo que dado um codigo de um encomenda procura na classe o Pedido correspondente e devolve o Encomenda desse pedido
     * @param String cod
     * @return Encomenda obtida
     */ 
    public Encomenda getEncomenda(String cod){
        return this.pedidos.get(cod).getEncomenda();
    }
    /**
     * Metodo que dado o codigo devolve o pedido correspondente
     * @param String cod
     * @return Pedido obtido
     */
    public Pedido getPedido(String cod) throws PedidoNaoExistenteException{
        if(!this.pedidos.containsKey(cod)) throw new PedidoNaoExistenteException("Não existe encomenda com código " + cod + ".");
        return this.pedidos.get(cod).clone();
    }
    /**
     * Metodo que dado um codigo e um codigo de Transportadora/Voluntario procura o pedido correspondente ao codigo e atualiza a 
     * variavel transporte para o codigo de Transportadora/Voluntario
     * @param String codigo,String codTransp
     */
    public void atualizaTransporte(String codigo,String codTransp){
        Pedido p = this.pedidos.get(codigo);
        p.setTransporte(codTransp);
    }
    /**
     * Metodo que dado um codigo e um codigo de Loja procura o pedido correspondente ao codigo e atualiza a 
     * variavel loja para o codigo de loja
     * @param String codigo,String loja
     */
    public void atualizaLoja(String codigo, String loja){
        Pedido p = this.pedidos.get(codigo);
        p.setLoja(loja);
    }
    /**
     * Metodo que dado um codigo e um boolean procura o pedido correspondente ao codigo e atualiza a 
     * variavel pendenteUtilizador para o novo boolean
     * @param String codigo,boolean e
     */
    public void atualizaPendenteUtilizador(String codigo, boolean e){
        Pedido p = this.pedidos.get(codigo);
        p.setPendenteUtilizador(e);
    }
    /**
     * Metodo que dado um codigo e um boolean procura o pedido correspondente ao codigo e atualiza a 
     * variavel pendenteTransporte para o novo boolean
     * @param String codigo,boolean e
     */
    public void atualizaPendenteTransporte(String codigo, boolean e){
        Pedido p = this.pedidos.get(codigo);
        p.setPendenteTransporte(e);
    }
    /**
     * Metodo que dado um codigo e um boolean procura o pedido correspondente ao codigo e atualiza a 
     * variavel aceite para o novo boolean
     * @param String codigo,boolean e
     */
    public void atualizaAceite(String codigo, boolean e){
        Pedido p = this.pedidos.get(codigo);
        p.setAceite(e);
    }
    /**
     * Metodo que dado um codigo e um double procura o pedido correspondente ao codigo e atualiza a 
     * variavel custo para o novo double
     * @param String codigo,double e
     */
    public void atualizaCusto(String codigo, double c){
        if(this.pedidos.containsKey(codigo))
            this.pedidos.get(codigo).setCusto(c);
    }
    /**
     * Metodo que dado uma List<String> de codigos procura os codigos na classe e devolve uma List<Encomenda> com as 
     * encomendas correspondentes se existirem
     * @param List<String> codigos
     * @return List<Encomenda> obtida
     */	
    public List<Encomenda> getListEncomendas(List<String> codigos){
        List<Encomenda> ret = new ArrayList<>();
        Pedido p = null;
        for(String c: codigos){
            p = this.pedidos.get(c);
            if(p != null)
                ret.add(p.getEncomenda());
        }
        return ret;
    }
   /**
    * Metodo que ddo uma List<String> de codigos devolve todos as encomendas dos pedidos que se encontram prontos a entregar
    * @param List<String> codigos
    * @return List<Encomenda> obtida
    */ 	
   public List<Encomenda> getListEncomendasPorEntregar(List<String> codigos){
        List<Encomenda> ret = new ArrayList<>();
        Pedido p = null;
        for(String c: codigos){
            p = this.pedidos.get(c);
            if(p != null && p.ready())
                ret.add(p.getEncomenda());
        }
        return ret;
    }

   /* public List<Encomenda> getListEncomendas(List<String> codigos){
        List<Encomenda> ret = new ArrayList<>();
        Pedido p = null;
        for(String c: codigos){
            p = this.pedidos.get(c);
            if(p != null)
                ret.add(p.getEncomenda());
        }
        return ret;
    }*/
    /**
     * Metodo que dado uma List<String> de codigos procura os codigos na classe e devolve uma List<Pedido> com os 
     * pedidos correspondentes se existirem
     * @param List<String> codigos
     * @return List<Encomenda> obtida
     */	
    public List<Pedido> getListPedidos(List<String> codigos){
        List<Pedido> ret = new ArrayList<>();
        Pedido p = null;
        for(String c: codigos){
            p = this.pedidos.get(c);
            if(p != null)
                ret.add(p.clone());
        }
        return ret;
    }

    /**
     * Metodo que dado um codigo procura os pedidos relacionados com ele e se estes tiverem depedentes da aceitaçao do utilizador devolve 
     * uma a List com esses pedidos .
     * @param String codigo
     * @return List<SimpleEntry<Encomenda,Double>> obtido
     */ 
  public List<Pedido> encomendasPendentesUtilizador(String cod){

        return this.pedidos.values().stream()
                                    .filter(v->v.getPendenteUtilizador() && v.getEncomenda().getCodUtilizador().equals(cod))
                                    .map(Pedido::clone)
                                    .collect(Collectors.toList());
    }
    /**
     * Metodo que dado um codigo de uma Transportadora/Voluntario devolve a lista de encomendas dos pedidos que estao pendentes de transporte
     * @param String cod 
     * @return List<Encomenda> obtida
     	*/
    public List<Encomenda> encomendasPendentesTransporte(String cod){
        return this.pedidos.values().stream()
                                    .filter(v->v.getPendenteTransporte() && v.getTransporte().equals(cod))
                                    .map(v->v.getEncomenda())
                                    .collect(Collectors.toList());
    }
    /**
     * Metodo que dado um codigo de um Utilizador verifica os pedidos que estao associados a esse codigo e devolve a Lista de Encomendas
     * dos pedidos
     * @param String codU
     * @List<Encomenda> obtida
     */
    public List<Encomenda> encomendasNoSistema(String codU){
        return this.pedidos.values().stream()
                                    .map(Pedido::getEncomenda)
                                    .filter(v->v.getCodUtilizador().equals(codU))
                                    .map(Encomenda::clone)
                                    .collect(Collectors.toList());
    }

    public void rejeitaEnc(String codUser, String codEnc) throws PedidoNaoExistenteException{
        Pedido e = this.pedidos.get(codEnc);
        if(e == null) throw new PedidoNaoExistenteException("Pedido nao existe");
        e.adicionaRejeitaram(codUser);
    }
}
