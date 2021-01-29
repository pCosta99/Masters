
/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.Collections;
import java.util.Set;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.time.LocalDate;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
import java.util.Date;

public class GestaoEncomendas implements Serializable {
    
     /* 
      * 
      * variaveis de instancia
      */
     private Map<String,Encomenda> encomendas;
     
     /* 
      * 
      * construtores
      */
     public GestaoEncomendas() {    
         this.encomendas = new HashMap<>();
     } 
     
     public GestaoEncomendas(Map<String,Encomenda> encomendas) {
         this.encomendas = encomendas.values()
                                     .stream()
                                     .collect(Collectors.toMap(e -> e.getCodigoEncomenda(),e -> e.clone()));
     }
     
     public GestaoEncomendas(GestaoEncomendas ge) {
         this.encomendas = ge.getEncomendas();    
     }
     
     public Map<String,Encomenda> getEncomendas() {
         return this.encomendas.values()
                               .stream()
                               .collect(Collectors.toMap(e -> e.getCodigoEncomenda(),e -> e.clone())); 
     }
     
     /* 
      * 
      * metodos de instancia
      */
     public void addEncomenda(Encomenda e) {
         this.encomendas.put(e.getCodigoEncomenda(), e.clone());    
     }
  
     public void removeEncomenda(String codEncomenda) {
         this.encomendas.remove(codEncomenda);    
     }
     
     public Encomenda getEncomenda(String codEncomenda) {
         return existeEncomenda(codEncomenda) ? this.encomendas.get(codEncomenda).clone() : null; 
     }
     
     public int totalEncomendas() {
         return this.encomendas.size();
     }
     
     public boolean existeEncomenda(String codigoEncomenda) {
         return this.encomendas.containsKey(codigoEncomenda);
     }
     
     public void setDistanciaEncomenda(String codEncomenda, double distancia) {
         if(encomendas.containsKey(codEncomenda)) {
             this.encomendas.get(codEncomenda).setDistancia(distancia);
         }
     }
     
     public void setEstadoEncomenda(String codEnc, EstadoEncomenda estadoEncomenda) {
         if(this.encomendas.containsKey(codEnc)) {
             this.encomendas.get(codEnc).setEstadoEncomenda(estadoEncomenda);
             System.out.println(codEnc + ": " + estadoEncomenda.toString());
         }
     }
     
     public void setEstadoEncomendas(List<String> codigosEncs, EstadoEncomenda estadoEncomenda) {
         for(String codEnc: codigosEncs) {
             setEstadoEncomenda(codEnc, estadoEncomenda);
             System.out.println(codEnc + ": " + estadoEncomenda.toString());
         }
     }
     
     public List<Encomenda> getEncomendasPendentesUtilizador(String codUtilizador) {
         return this.encomendas.values()
                               .stream()
                               .filter(e -> e.getCodigoUtilizador().equals(codUtilizador) && e.getEstadoEncomenda() == EstadoEncomenda.ORCAMENTO_TRANSPORTADOR)
                               .map(e -> e.clone())
                               .collect(Collectors.toList());
     }
     
     public List<String> getCodEncomendasPendentesUtilizador(String codUtilizador) {
         return this.encomendas.values()
                               .stream()
                               .filter(e -> e.getCodigoUtilizador().equals(codUtilizador) && e.getEstadoEncomenda() == EstadoEncomenda.ORCAMENTO_TRANSPORTADOR)
                               .map(e -> e.getCodigoEncomenda())
                               .collect(Collectors.toList());
     }
     
     public List<Encomenda> getEncomendasEntreguesUtilizador(String codUtilizador) {
         return this.encomendas.values()
                               .stream()
                               .filter(e -> e.getCodigoUtilizador().equals(codUtilizador) && e.getEstadoEncomenda() == EstadoEncomenda.ENTREGUE)
                               .map(e -> e.clone())
                               .collect(Collectors.toList());
     }
     
     public List<String> getCodEncomendasPendentesLoja(String codLoja) {
         return this.encomendas.values()
                               .stream()
                               .filter(e -> e.getCodigoLoja().equals(codLoja) && e.getEstadoEncomenda() == EstadoEncomenda.SOLICITADO)
                               .map(e -> e.getCodigoEncomenda())
                               .collect(Collectors.toList());
     }
     
     public List<Encomenda> getEncomendasPendentesLevantamento() {
        return this.encomendas.values()
                              .stream()
                              .filter(e -> e.getEstadoEncomenda() == EstadoEncomenda.DISPONIBILIZADO_LOJA)
                              .map(e -> e.clone())
                              .collect(Collectors.toList());
     }
     
     public List<String> getCodEncomendasPendentesLevantamento() {
        return this.encomendas.values()
                              .stream()
                              .filter(e -> e.getEstadoEncomenda() == EstadoEncomenda.DISPONIBILIZADO_LOJA)
                              .map(e -> e.getCodigoEncomenda())
                              .collect(Collectors.toList());
     }
     
     public List<String> getCodEncomendasPendentesLevantamentoTransportadora(String codTransportadora) {
        return this.encomendas.values()
                              .stream()
                              .filter(e -> e.getEstadoEncomenda() == EstadoEncomenda.ACEITE_UTILIZADOR && e.getCodigoTransportador().equals(codTransportadora))
                              .map(e -> e.getCodigoEncomenda())
                              .collect(Collectors.toList());
     }
     
     public List<Encomenda> getEncomendasPendentesLevantamentoTransportadora(String codTransportadora) {
        return this.encomendas.values()
                              .stream()
                              .filter(e -> e.getEstadoEncomenda() == EstadoEncomenda.ACEITE_UTILIZADOR && e.getCodigoTransportador().equals(codTransportadora))
                              .map(e -> e.clone())
                              .collect(Collectors.toList());
     }
     
     public void orcamentarTransporte(String codEncomenda, String codTransportadora, double precoTransporte) {
         if(this.encomendas.containsKey(codEncomenda)) {
             if(this.encomendas.get(codEncomenda).getEstadoEncomenda() == EstadoEncomenda.DISPONIBILIZADO_LOJA) {
                 this.encomendas.get(codEncomenda).setPrecoTransporte(precoTransporte);
                 this.encomendas.get(codEncomenda).setEstadoEncomenda(EstadoEncomenda.ORCAMENTO_TRANSPORTADOR);
                 this.encomendas.get(codEncomenda).setCodigoTransportador(codTransportadora);
             }
         }
     }
     
     public List<String> getCodEncomendasPendentesTransporte(String codigoTransportador) {
        return this.encomendas.values()
                              .stream()
                              .filter(e -> e.getEstadoEncomenda() == EstadoEncomenda.ACEITE_TRANSPORTADOR && e.getCodigoTransportador().equals(codigoTransportador))
                              .map(e -> e.getCodigoEncomenda())
                              .collect(Collectors.toList());
     }
     
     public void levantarEncomenda(String codEncomenda, String codTransportador) {
         if(this.encomendas.containsKey(codEncomenda)) {
             this.encomendas.get(codEncomenda).setEstadoEncomenda(EstadoEncomenda.ACEITE_TRANSPORTADOR);
             this.encomendas.get(codEncomenda).setCodigoTransportador(codTransportador);
         }
     }

     public void transportarEncomenda(String codEncomenda, String codTransportador) {
         if(this.encomendas.containsKey(codEncomenda)) {
             this.encomendas.get(codEncomenda).setEstadoEncomenda(EstadoEncomenda.ENTREGUE);
             this.encomendas.get(codEncomenda).setDataEntrega(new Date());
             this.encomendas.get(codEncomenda).setCodigoTransportador(codTransportador);
         }
     }
     
     public List<Encomenda> getEncomendasUtilizador(String codUtilizador, Date dataIncico, Date dataFim) {
         return this.encomendas.values()
                               .stream()
                               .filter(e -> e.getCodigoUtilizador().equals(codUtilizador) && 
                                            e.getDataEncomenda().after(dataIncico) && 
                                            e.getDataEncomenda().before(dataFim))
                               .map(e -> e.clone())
                               .collect(Collectors.toList());
     }
     
     public List<Encomenda> getEncomendasTransportador(String codTransportador, Date dataIncico, Date dataFim) {
         return this.encomendas.values()
                               .stream()
                               .filter(e -> e.getCodigoTransportador().equals(codTransportador) && 
                                            e.getDataEncomenda().after(dataIncico) && 
                                            e.getDataEncomenda().before(dataFim))
                               .map(e -> e.clone())
                               .collect(Collectors.toList());
     }
     
     public List<Encomenda> getEncomendasLoja(String codLoja, Date dataIncico, Date dataFim) {
         return this.encomendas.values()
                               .stream()
                               .filter(e -> e.getCodigoLoja().equals(codLoja) && 
                                            e.getDataEncomenda().after(dataIncico) && 
                                            e.getDataEncomenda().before(dataFim))
                               .map(e -> e.clone())
                               .collect(Collectors.toList());
     }
     
     public double totalFaturado(String codTransportadora, Date dataIncicio, Date dataFim) {
         return this.encomendas.values()
                               .stream()
                               .filter(e -> e.getCodigoTransportador().equals(codTransportadora) && 
                                            e.getDataEncomenda().after(dataIncicio) && 
                                            e.getDataEncomenda().before(dataFim))
                               .mapToDouble(e -> e.getPrecoTransporte())
                               .sum();
     }
     
     public List<String>top10Utilizadores() {
         // Criar map com codigo de utilizador e lista de encomendas associadas
         Map<String,List<String>> utilizadorEncomendas = new HashMap<String,List<String>>();
         
         for(Encomenda e : this.encomendas.values()) {
             if(!utilizadorEncomendas.containsKey(e.getCodigoUtilizador())) {
                 utilizadorEncomendas.put(e.getCodigoUtilizador(), new ArrayList<>());
             }
             utilizadorEncomendas.get(e.getCodigoUtilizador()).add(e.getCodigoEncomenda());
         }
         
         // a partir do primeiro map criar um segundo com codigo de utilizador e numero de encomendas
         // a razao de ser um double é porque o mesmo comparator é também utilizado no metodo List<String>top10Transportadores()
         Map<String,Double> utilizadoresNumeroEncs = new HashMap<>();
         
         Iterator<Map.Entry<String, List<String>>> itr = utilizadorEncomendas.entrySet().iterator(); 
         
         while(itr.hasNext()) { 
             Map.Entry<String, List<String>> entry = itr.next();  
             utilizadoresNumeroEncs.put(entry.getKey(), Double.valueOf(entry.getValue().size()));
         }
         
         MapStringDoubleComparator comparator = new MapStringDoubleComparator(utilizadoresNumeroEncs);
         
         // criar map ordenado (descending)
         TreeMap<String, Double> utilizadorNumeroEncsOrdenado = new TreeMap<String, Double>(comparator);
         
         utilizadorNumeroEncsOrdenado.putAll(utilizadoresNumeroEncs);
         
         // retornar a lista de codigos de utilizadores pretendida
         return utilizadorNumeroEncsOrdenado.keySet()
                                            .stream()
                                            .limit(10)
                                            .collect(Collectors.toList());                  
     }
     
     public List<String>top10Transportadores() {
         // Criar map com codigo de transportador e lista de preços de transporte associadas
         Map<String,List<Double>> transportadorPrecosTransporte = new HashMap<>();
         
         for(Encomenda e : this.encomendas.values()) {
             if(!transportadorPrecosTransporte.containsKey(e.getCodigoTransportador())) {
                 transportadorPrecosTransporte.put(e.getCodigoTransportador(), new ArrayList<>());
             }
             transportadorPrecosTransporte.get(e.getCodigoTransportador()).add(e.getPrecoTransporte());
         }
         
         // a partir do primeiro map criar um segundo com codigo de utilizador e o total de preços
         Map<String,Double> transportadoresTotalPreco = new HashMap<>();
         
         Iterator<Map.Entry<String, List<Double>>> itr = transportadorPrecosTransporte.entrySet().iterator(); 
         
         while(itr.hasNext()) { 
             Map.Entry<String, List<Double>> entry = itr.next();
             transportadoresTotalPreco.put(entry.getKey(), entry.getValue().stream().mapToDouble(Double::doubleValue).sum());
         }
         
         MapStringDoubleComparator comparator = new MapStringDoubleComparator(transportadoresTotalPreco);
         
         // criar map ordenado descending
         TreeMap<String, Double> transportadorTotalPrecoOrdenado = new TreeMap<String, Double>(comparator);
         
         return transportadorTotalPrecoOrdenado.keySet()
                                               .stream()
                                               .limit(10)
                                               .collect(Collectors.toList());                 
     }
   
     public boolean equals(Object o) {
         if (this == o) 
           return true;
         if ((o == null) || (this.getClass() != o.getClass()))
           return false;
         GestaoEncomendas ge = (GestaoEncomendas) o;
         return this.encomendas.equals(ge.getEncomendas()); 
     }
     
     public String toString() {
         StringBuffer sb = new StringBuffer();
         for (Encomenda e: this.encomendas.values()) {
             sb.append(e.toString());
             sb.append("\n\n");
         }
         return sb.toString(); 
     }
     
     public GestaoEncomendas clone() {
         return new GestaoEncomendas(this); 
     }
}
