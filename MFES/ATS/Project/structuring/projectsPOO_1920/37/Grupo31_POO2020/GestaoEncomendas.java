
/**
 * Escreva a descrição da classe GestaoEncomendas aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collections;
import java.io.*;
public class GestaoEncomendas implements Serializable{
    // todas as encomendas efetuadas pelos users serão inseridas neste map
    private Map<String/*codU*/, Map<String/*codLoja*/, List<Encomenda>>> encomendas;
    // irá conter os cods de encomendas aceites pelas empresas ou voluntários
    private Map<String/*codEnc*/,String/*codDistribuidor*/> encAceites; // Map<Codigo da encomenda, codigo de quem a aceitou>
    
    public GestaoEncomendas(){
        this.encomendas = new HashMap<>();
        this.encAceites = new HashMap<>();
    }
    
    public GestaoEncomendas(Map<String,Map<String,List<Encomenda>>> encomenda, Map<String,String> encAceites){
        setEncomenda(encomenda);
        setEncomendasAceites(encAceites);
    }
    
    
    public GestaoEncomendas (GestaoEncomendas gt){
        this.encomendas = gt.getEncomendas();
        this.encAceites = gt.getEncAceites();
    }
   
    /**
     * Gets
     */
    
    public Map<String,String> getEncAceites(){
        return encAceites;
    }


    public Map<String,Map<String, List<Encomenda>>> getEncomendas(){
        //Map<String, Encomenda> res = new HashMap<>();
        //pelos values()
        //for(Encomenda e: this.encomenda.values()){
        //    res.put(e.getCodEnc(),e.clone());
        //}
        return encomendas;
    }
    
    /**
     * Sets
     */
    public void setEncomenda(Map<String, Map<String, List<Encomenda>>> encomenda2) {
        this.encomendas = encomenda2;
    }
    
    public void setEncomendasAceites(Map<String,String> encAceites){
        this.encAceites = encAceites;
    }

    //to clone
    public GestaoEncomendas clone(){
        return new GestaoEncomendas(this);
    }
    
    
    //equals to
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        GestaoEncomendas ge = (GestaoEncomendas) obj;
        return (ge.getEncomendas().equals(this.encomendas) &&
                ge.getEncAceites().equals(this.encAceites));
    }
    
    
    //tostring
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append ("\nGestão de encomendas: ").append(this.encomendas.values());
        sb.append ("\nEncomendas aceites: ").append (this.encAceites);
        return sb.toString();
    }
    
    
    // retorna todos os códigos dos clientes que efetuaram encomendas
    public Set<String> todosCodigosClientes(){
         return this.encomendas.keySet();
    }
    
    // adiciona uma encomenda. Tem de ver pelo codU e codLoja
    public void addEncomenda(Encomenda enc){
        // procura no primeiro map pela key de codigo de utilizador
        if(!this.encomendas.containsKey(enc.getCodU())){
            // se não existir um map com a key, cria um map com a key desse codigo
            this.encomendas.put(enc.getCodU(), new HashMap<>());
        }
        
        // procura no segundo map pela key de codigo de loja
        if(!this.encomendas.get(enc.getCodU()).containsKey(enc.getCodU())){
            // se não existir, cria um arraylist com a key
            this.encomendas.get(enc.getCodU()).put(enc.getCodLoja(), new ArrayList<>());
        }

        // depois de verificar os maps, insere a encomenda no arraylist específico
        this.encomendas.get(enc.getCodU()).get(enc.getCodLoja()).add(enc.clone());
        
    }

    // retorna um map com todas encomendas de um utilizador, onde a key é a compra em cada loja
    public String printEncUtilizador(String codU){
        StringBuilder sb = new StringBuilder();
        //sb.append ("\nGestão de encomendas: ").append(this.encomendas.values());
        // this.encomendas.get(codU);
        try{
            for(Map.Entry<String, List<Encomenda>> map : this.encomendas.get(codU).entrySet()){
                sb.append ("\nLoja: ").append(map.getKey());
                for(Encomenda e : map.getValue()){
                    sb.append ("\n-Encomenda: ").append(e.getCodEnc()).append(", ");
                    sb.append ("-Valor total: ").append(e.calculaValorTotal()).append(", ");
                    sb.append ("-Data: ").append(e.getDataEncomenda()).append(", ");
                    if(e.getdistribuidor()!=null){
                        sb.append("-Encomenda entregue por: " + e.getdistribuidor()).append(", ");
                    }
                    for(LinhaEncomenda linha : e.getLinhas()){
                        sb.append("\n->Código do Produto: ").append(linha.getCodProduto()).append(", ");
                        sb.append("->Descrição do Produto: ").append(linha.getDescricao()).append(", ");
                        sb.append("->Preço: ").append(linha.getPreco()).append(", ");
                        sb.append("->Quantidade: ").append(linha.getQuantidade()).append(", ");
                        sb.append("->IVA: ").append(linha.getImposto());
                    }
                }
            }
            return sb.toString();
        }
        catch(NullPointerException e){
            return "Não existem encomendas deste utilizador";
        }
    }
   
    // Dado um código de encomenda retorna a encomenda
    public Encomenda getEncomenda(String codEnc){
        // 1) map de fora
        for (Map.Entry<String, Map<String, List<Encomenda>>> mapFora : encomendas.entrySet()) {
            // 2) entrar no map interno
            for (Map.Entry<String, List<Encomenda>> mapDentro : mapFora.getValue().entrySet()) {
                // 3) Todas as encomendas da lista
                for(Encomenda e : mapDentro.getValue()){
                    if (e.getCodEnc().equals(codEnc)){
                        return e;
                    }
                }
            }
        }
        // se não encontrar nenhuma encomenda, retorna null
        return null;
    }
    
    // dado um codigo de encomenda e um codigo de utilizador, remove essa encomenda
    public void removeEncomenda(String codEnc, String codU, String codLoja){
        this.encomendas.get(codU).get(codLoja)
                .removeIf(enc -> enc.getCodEnc().equals(codEnc)); // vai ao arraylist e remove a encomenda
    }
    
    // remove a encomenda das aceites
    public void removeEncAceite(String codEnc){
        encAceites.remove(codEnc); 
    }
            
    // dado um codigo de encomenda, adiciona uma encomenda à lista de aceites
    public void adicionaEncomendaAceite(String codEnc, String codEntrega){
        encAceites.put(codEnc,codEntrega);
    }

    // dado um codigo de encomenda, remove uma encomenda da lista de aceites
    public void removeEncomendaAceite(String codEnc){
        encAceites.remove(codEnc);
    }

    // calcula o top 10 de utilizadores que efetuaram encomendas
public List<Map.Entry<String,Integer>> top10Utilizadores(GestaoTotal gt) {
        // lista de pares
        List<Map.Entry<String,Integer>> lista = new ArrayList<>();
        
        // 1)------------------------
        for (Map.Entry<String, Map<String, List<Encomenda>>> mapFora : encomendas.entrySet()) {
            // a) pegar na key (codigo de utilizador)
            String k = mapFora.getKey();
            // e no inteiro para ter o tamanho da lista de encomendas
            int v = 0;
            // entrar no map interno
            for (Map.Entry<String, List<Encomenda>> mapDentro : mapFora.getValue().entrySet()) {
                v += mapDentro.getValue().size();
            }
            // b) adicionar ao treeMap
            lista.add(new AbstractMap.SimpleEntry<String, Integer>(k, v));
        }
        
        // utilizar um comparator para dar sort à lista pelos values
        Collections.sort(lista, new ComparatorTop10Users());

        // lista só com o top10
        List<Map.Entry<String,Integer>> top10 = lista.stream().limit(10).collect(Collectors.toList());

        return top10;
  }

    public List<Encomenda> historicoLojas(String codLoja){
        List<Encomenda> historico = new ArrayList<>();
        
        // 1) todas as keys do map de fora
        for (Map.Entry<String, Map<String, List<Encomenda>>> mapFora : encomendas.entrySet()) {
            // se existir um mapa com a key do codLoja
            if(encomendas.get(mapFora.getKey()).containsKey(codLoja)){
                // pega na lista e adiciona uma cópia de cada encomenda no historico
                for(Encomenda e : encomendas.get(mapFora.getKey()).get(codLoja)){
                    historico.add(e.clone());
                }
            }
        }
      return historico;
    }

    // print do historico das lojas
    public String printHistoricoLojas(List<Encomenda> historico, String codLoja){
        StringBuilder sb = new StringBuilder();
        sb.append ("\nLoja: ").append(codLoja);
        for(Encomenda e : historico){
            sb.append("\n-----------------------------------------");
            sb.append ("\n-Encomenda: ").append(e.getCodEnc()).append(", ");
            sb.append ("-Valor total: ").append(e.calculaValorTotal()).append(", ");
            sb.append ("-Data: ").append(e.getDataEncomenda()).append(", ");
            if(e.getdistribuidor()!=null){
                sb.append("-Encomenda entregue por: " + e.getdistribuidor()).append(", ");
            }
            for(LinhaEncomenda linha : e.getLinhas()){
                sb.append("\n->Código do Produto: ").append(linha.getCodProduto()).append(", ");
                sb.append("->Descrição do Produto: ").append(linha.getDescricao()).append(", ");
                sb.append("->Preço: ").append(linha.getPreco()).append(", ");
                sb.append("->Quantidade: ").append(linha.getQuantidade()).append(", ");
                sb.append("->IVA: ").append(linha.getImposto());
            }
        }
        return sb.toString();
    }

    // retorna um Map<K,V> onde K = codEncomenda, V = estado (entregue ou pendente)
    public Map<String,String> historico(String codigo) {
        Map<String,String> historico = new HashMap<>();
        // 1) Todas as keys do map de fora
        for (Map.Entry<String, Map<String, List<Encomenda>>> mapFora : encomendas.entrySet()) {
            // 2) Todas as keys do map de dentro
            for (Map.Entry<String, List<Encomenda>> mapDentro : mapFora.getValue().entrySet()) {
                // 3) Todas as encomendas da lista
                for(Encomenda e : mapDentro.getValue()){
                    // verifica quem entregou a encomenda
                    if(e.getdistribuidor()!=null && e.getdistribuidor().equals(codigo)){
                        // string estado para verificar o estado da encomenda
                        String estado = "Entregue";
                        // adiciona como <CodEnc,estado>
                        historico.put(e.getCodEnc(), estado);
                    }
                }
            }
        }
	    return historico;
    }

    // calcula faturacao
    public double faturacaoLojas(String codLoja){
        double res = 0;
        // todas as keys do map de fora
        for (Map.Entry<String, Map<String, List<Encomenda>>> mapFora : encomendas.entrySet()) {
            // pega na key
            String k = mapFora.getKey();
            // verifica se existe um map com codLoja
            if(this.encomendas.get(k).containsKey(codLoja)){
                // se existir, calcula o valor total de cada encomenda e soma
                for(Encomenda e : this.encomendas.get(k).get(codLoja)){
                    // ao res
                    res = res + e.calculaValorTotal();
                }
            }
            // caso não exista um map com codLoja, passa à frente
        }
        return res;
    }

    // calcular o total de encomendas medicas e normais
    public void totalEncomendas(){
        int medicas = 0;
        int normais = 0;
        // todas as keys do map de fora
        for (Map.Entry<String, Map<String, List<Encomenda>>> mapFora : encomendas.entrySet()) {
            // todas as keys do map de dentro
            for (Map.Entry<String, List<Encomenda>> mapDentro : mapFora.getValue().entrySet()) {
                // 3) Todas as encomendas da lista
                for(Encomenda e : mapDentro.getValue()){
                    // verifica se a encomenda é uma médica
                    if (e instanceof Medicas){
                        medicas = medicas + 1;
                    }
                    else{
                        normais = normais + 1;
                    }
                }
            }
        }
        System.out.println("Existem " + medicas + " encomendas médicas entregues.");
        System.out.println("Existem " + normais + " encomendas normais entregues.");
    }

    // mudar o estado de uma encomenda
	public void mudarEstadoEncomenda(String nr, String distribuidor) {
        // todas as keys do map de fora
        for (Map.Entry<String, Map<String, List<Encomenda>>> mapFora : encomendas.entrySet()) {
            // todas as keys do map de dentro
            for (Map.Entry<String, List<Encomenda>> mapDentro : mapFora.getValue().entrySet()) {
                // 3) Todas as encomendas da lista
                for(Encomenda e : mapDentro.getValue()){
                    // confirma se o codigo da encomenda é igual ao nr que recebeu
                    if(e.getCodEnc().equals(nr) && 
                        // verifica no mapa de aceites que existe
                        encAceites.containsKey(nr) &&
                        // verifica se o distribuidor da encomenda é o mesmo que está no mapa
                        distribuidor.equals(encAceites.get(nr))){
                            e.setdistribuidor(distribuidor);
                        }

                    }
                }
            }
        }
    
    // print das encomendas pendentes
	public void printEncPendentes(GestaoTotal gt, String codVol) {
        List<String> codigos = new ArrayList<>();
        for (Map.Entry<String, String> map : encAceites.entrySet()) {
            if(map.getValue()!=null && map.getValue().equals(codVol)){
                codigos.add(map.getKey());
            }
        }
        System.out.println(codigos);
    }
    
}
