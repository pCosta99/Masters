import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.Collections;



public class Encomenda  {
    
   //Variaveis de instância
   private String idEncomenda; //numero da encomenda
   private LocalDate data; //data da encomenda
   private Transportador transportador;
   private Loja loja;
   private Cliente cliente;
   private List<LinhaEncomenda> linhas;
   private boolean encAceite; //diz se a encomenda foi aceite (por parte do Voluntario sempre aceite)
   private boolean pronta_Entregar;
   private boolean t_Empresa;
   private boolean t_Voluntario;
  
   /**
     * Construtor vazio
     */
   public Encomenda(){
       this.idEncomenda = "";
       this.data = null;
       this.transportador = null;
       this.loja = null;
       this.cliente = null;
       this.encAceite = false;
       this.pronta_Entregar=false;
       this.t_Empresa=false;
       this.t_Voluntario = false;
       this.linhas = new ArrayList<>();
   }
    
   /**
     * Construtor parametrizado 
     */
   public Encomenda(String idEncomenda, LocalDate data, Transportador transportador,
   Loja loja, Cliente cliente, boolean encAceite, boolean pronta_Entregar,boolean t_Empresa,
   boolean t_Voluntario,List<LinhaEncomenda> linhasEnc){
       this.idEncomenda = idEncomenda;
       this.data = data;
       this.transportador = transportador;
       this.loja = loja;
       this.cliente = cliente;
       this.encAceite = encAceite;
       this.pronta_Entregar=pronta_Entregar;
       this.t_Empresa=t_Empresa;
       this.t_Voluntario = t_Voluntario;
       setEncomendas(linhasEnc);
   }
    
   /**
     * Construtor de cópia 
     */
   public Encomenda(Encomenda e){
        this.idEncomenda = e.getIdEncomenda();
        this.data = e.getData();
        this.transportador = e.getTransportador();
        this.loja = e.getLoja();
        this.cliente = e.getCliente();
        this.encAceite = e.getEncAceite();
        this.pronta_Entregar=e.getP_Entregar();
        this.t_Empresa=e.getT_Empresa();
        this.t_Voluntario = e.getT_Voluntario();
        this.linhas = e.getLinhas();
   }
    
   // Getters
 
   public String getIdEncomenda(){
        return this.idEncomenda;
   }
   
   public LocalDate getData(){
        return this.data;
   }
   
   public Transportador getTransportador(){
       return this.transportador;
   }
   
   public Loja getLoja(){
       return this.loja;
   }
   
   public Cliente getCliente(){
       return this.cliente;
   }
   
   public boolean getEncAceite (){
       return this.encAceite;
   }
   
   public boolean getP_Entregar(){
       return this.pronta_Entregar;
   }
    
   public boolean getT_Empresa(){
       return this.t_Empresa;
   }
   
   public boolean getT_Voluntario(){
       return this.t_Voluntario;
   }
    
   public List<LinhaEncomenda> getLinhas() {
      return this.linhas.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());        
    }
  
   // Setters
   
   public void setIdEncomenda(String i){
        this.idEncomenda = i;
   } 
   
   public void setData(LocalDate dt){
        this.data = dt;
   }

   public void setTransportador(Transportador t){
       this.transportador = t;
   }
   
   public void setLoja(Loja l){
       this.loja = l;
   }
   
   public void setCliente(Cliente c){
       this.cliente = c;
   }
    
   public void setEncAceite(boolean ea){
       this.encAceite = ea;
   }
   
   public void setPronta_Entregar(boolean pe){
       this.pronta_Entregar = pe;
   }
   public void setT_Empresa(boolean te){
       this.t_Empresa = te;
   }
   public void setT_Voluntario(boolean tv){
       this.t_Voluntario = tv;
   }
   public void setEncomendas(List<LinhaEncomenda> linhasEnc) {
        this.linhas = new ArrayList<>();
        for(LinhaEncomenda le : linhasEnc) {
            this.linhas.add(le.clone());
        }
   }
   
   /**
     * Metodo Equals
     */
   public boolean equals(Object o){
        if (this == o)
            return true;
            
        if (o == null || this.getClass() != o.getClass())
            return false;
            
        Encomenda e = (Encomenda) o;
        
        return this.idEncomenda.equals(e.getIdEncomenda()) &&
        this.data.equals(e.getData()) &&
        this.encAceite == e.getEncAceite() && this.pronta_Entregar==e.getP_Entregar() && this.t_Empresa==e.getT_Empresa() &&
        this.t_Voluntario == e.getT_Voluntario() && this.linhas.equals(e.getLinhas());
   }
    
   public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Id da encomenda: " + this.getIdEncomenda()+"\n");
        sb.append("Data da encomenda: " + this.getData() + "\n");
        sb.append("Transportador: " + this.getTransportador() + "\n");
        sb.append("Loja: " + this.getLoja() + "\n");
        sb.append("Cliente: " + this.getCliente() + "\n");
        sb.append(this.linhas.toString());
        return sb.toString(); 
   }  
    
   //Clone
    
   public Encomenda clone (){
        return new Encomenda(this);    
   }
   
   public double pesoTotalEncomenda(){
       double peso=0;
       
       for (LinhaEncomenda linha : linhas){
            peso += linha.pesoTotal();
            
        }    
       return peso;
   }
   
   /** Funcao que calcula o valor total da encomenda **/
   public double valorEncomenda(){
       double distancia1;//distancia da empresaTransp ate Loja
       double distancia2; //distancia da Loja ate casa do cliente
       double distancia_total;
       double valor=0;
       
       if(transportador instanceof EmpresaTransp){
       distancia1 = Math.sqrt(Math.pow((transportador.getCoordenadas().getLatitude()-loja.getCoordenadas().getLatitude()), 2) + 
       Math.pow((transportador.getCoordenadas().getLongitude()-loja.getCoordenadas().getLongitude()), 2));
       
       distancia2 = Math.sqrt(Math.pow((loja.getCoordenadas().getLatitude()-cliente.getCoordenadas().getLatitude()), 2) + 
       Math.pow((loja.getCoordenadas().getLongitude()-cliente.getCoordenadas().getLongitude()), 2));
       
       distancia_total = distancia1+ distancia2;
       
       if(this.pesoTotalEncomenda() <=5){ //se o peso for ate 5kg cobra se 5 euros, mais que isso cobra-se 50cent a mais por kg
           valor =  distancia_total * ((EmpresaTransp)transportador).getPrecoKm() + 5;
       }else{
           if(this.pesoTotalEncomenda() >5)
                valor =  distancia_total * ((EmpresaTransp)transportador).getPrecoKm() + 5 + (this.pesoTotalEncomenda() - 5) * 0.5 ;
       }
       
      } 
      return valor;
   }
}