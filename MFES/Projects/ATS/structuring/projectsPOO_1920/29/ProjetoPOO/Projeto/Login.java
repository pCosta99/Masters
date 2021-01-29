import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.io.Serializable;
import java.util.Collections;

public class Login implements Serializable
{   
    //Armazena cada Utilizador, Voluntario, Transportadora e Loja na sua lista respetiva
    private Map<String, Utilizador> AllUtilizadores;
    private Map<String, Voluntario> AllVoluntarios;
    private Map<String, Transportadora> AllTransportadoras;
    private Map<String, Loja> AllLojas;
    private Map<String, Encomenda> pedidosEncomendas;
    private Map<String, Encomenda> encConcluidas;
    private ArrayList<String> usuariosenc; //usado para top 10 utilizadores
    private ArrayList<Double> kmT; //usado para top 10 transportadoras
    
    GestaoEncomendas ge = new GestaoEncomendas();
    
    public Login(){
        this.AllUtilizadores = new HashMap<>();
        this.AllVoluntarios = new HashMap<>();
        this.AllTransportadoras = new HashMap<>();
        this.AllLojas = new HashMap<>();
        this.pedidosEncomendas = new HashMap<>();
        this.encConcluidas = new HashMap<>();
        this.usuariosenc = new ArrayList<>();
        this.kmT = new ArrayList<>();
    }
    
    public Login(Map<String,Utilizador> u, Map<String,Voluntario> v, Map<String, Transportadora> t, Map<String, Loja> l, Map<String, Encomenda> pedEnc, Map<String, Encomenda> encConc){
        setAllUtilizadores(u);
        setAllVoluntarios(v);
        setAllTransportadoras(t);
        setAllLojas(l);
        setPedidosEncomendas(pedEnc);
        setEncConcluidas(encConc);
    }
    
    public Login(Login lo){
        this.AllUtilizadores = lo.getAllUtilizadores();
        this.AllVoluntarios = lo.getAllVoluntarios();
        this.AllTransportadoras = lo.getAllTransportadoras();
        this.AllLojas = lo.getAllLojas();
        this.usuariosenc= lo.getusuariosenc();
        this.kmT = lo.getKmT();
       
    }    
    
    //Getters
    public ArrayList<Double> getKmT(){ 
      return this.kmT;
    } 
    public Map<String, Utilizador> getAllUtilizadores(){
        Map<String, Utilizador> u = new HashMap<>();
        for(Map.Entry<String, Utilizador> e : this.AllUtilizadores.entrySet()){
            u.put(e.getKey(), e.getValue().clone());
        }
        return u;
    }
    
    public Map<String, Voluntario> getAllVoluntarios(){
        Map<String, Voluntario> v = new HashMap<>();
        for(Map.Entry<String, Voluntario> e : this.AllVoluntarios.entrySet()){
            v.put(e.getKey(), e.getValue().clone());
        }
        return v;
    }   
    
    public Map<String, Transportadora> getAllTransportadoras(){
        Map<String, Transportadora> t = new HashMap<>();
        for(Map.Entry<String, Transportadora> e : this.AllTransportadoras.entrySet()){
            t.put(e.getKey(), e.getValue().clone());
        }
        return t;
    }   
    
    public Map<String, Loja> getAllLojas(){
        Map<String, Loja> l = new HashMap<>();
        for(Map.Entry<String, Loja> e : this.AllLojas.entrySet()){
            l.put(e.getKey(), e.getValue().clone());
        }
        return l;
    }
    
    public Map<String, Encomenda> getPedidosEncomendas(){
      Map<String, Encomenda> e = new HashMap<>();
      for(Map.Entry<String, Encomenda> entry : this.pedidosEncomendas.entrySet()){
          e.put(entry.getKey(), entry.getValue().clone());
        }    
      return e;  
    }
    
    public Map<String, Encomenda> getEncConcluidas(){
      Map<String, Encomenda> e = new HashMap<>();
      for(Map.Entry<String, Encomenda> entry : this.encConcluidas.entrySet()){
          e.put(entry.getKey(), entry.getValue().clone());
        }    
      return e;  
    }
    
    public ArrayList<String> getusuariosenc(){ 
      return this.usuariosenc;
    }
    
    //Setters
    
    public void setAllUtilizadores(Map<String, Utilizador> u){
        this.AllUtilizadores = new HashMap<>();
        for(Map.Entry<String, Utilizador> e: u.entrySet()){
            this.AllUtilizadores.put(e.getKey(), e.getValue().clone());
        }
    } 
    
    public void setAllVoluntarios(Map<String, Voluntario> v){
        this.AllVoluntarios = new HashMap<>();
        for(Map.Entry<String, Voluntario> e: v.entrySet()){
            this.AllVoluntarios.put(e.getKey(), e.getValue().clone());
        }
    } 
    
    public void setAllTransportadoras(Map<String, Transportadora> t){
        this.AllTransportadoras = new HashMap<String, Transportadora>();
        for(Map.Entry<String, Transportadora> e: t.entrySet()){
            this.AllTransportadoras.put(e.getKey(), e.getValue().clone());
        }
    } 
    
    public void setAllLojas(Map<String, Loja> l){
        this.AllLojas = new HashMap<String, Loja>();
        for(Map.Entry<String, Loja> e: l.entrySet()){
            this.AllLojas.put(e.getKey(), e.getValue().clone());
        }
    } 
    
    public void setPedidosEncomendas(Map<String, Encomenda> pedEnc){
      this.pedidosEncomendas = new HashMap<>();
      for(Map.Entry<String, Encomenda> entry: pedEnc.entrySet()) {
         this.pedidosEncomendas.put(entry.getKey(), entry.getValue().clone());
        }    
    }
    
    public void setEncConcluidas(Map<String, Encomenda> encConc){
      this.encConcluidas = new HashMap<>();
      for(Map.Entry<String, Encomenda> entry: encConc.entrySet()) {
         this.encConcluidas.put(entry.getKey(), entry.getValue().clone());
        }    
    }
    
    //toString
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizadores registados: ").append(this.AllUtilizadores).append("\n").
           append("Voluntarios registados: ").append(this.AllVoluntarios).append("\n").
           append("Transportadoras registadas: ").append(this.AllTransportadoras).append("\n").
           append("Lojas registadas: ").append(this.AllLojas).append("\n");
        return sb.toString();
    }   
    
    public Login clone(){
        return new Login(this);
    }    
    
    //Registar um utilizador, um voluntário, uma empresa transportadora e uma loja (novo)
    
    public void nregistarUtilizador(String codU, String nomeU, GPS gpsU, String pwU) throws JaRegistadoException{
        Utilizador u = new Utilizador(codU, nomeU, gpsU, pwU);
        if (this.AllUtilizadores.containsKey(u.getCodUtilizador())) throw new JaRegistadoException("Este utilizador ja foi registado! : " + u.getCodUtilizador());
        else this.AllUtilizadores.put(codU,u.clone());
    }    
    
    public void nregistarVoluntario(String codV, String nomeV, GPS gpsV, double raioV, String pwV) throws JaRegistadoException{
        Voluntario v = new Voluntario(codV, nomeV, gpsV, raioV, pwV, 0);
        if (this.AllVoluntarios.containsKey(v.getCodVoluntario())) throw new JaRegistadoException("Este Voluntario ja foi registado! : " + v.getCodVoluntario());
        else this.AllVoluntarios.put(codV, v.clone());
    }
    
    public void nregistarTransportadora(String CodEmpresaT, String NomeT, GPS gpsT, String NifT, double RaioT, double precokmT, String pwT) throws JaRegistadoException{
        Transportadora t = new Transportadora(CodEmpresaT, NomeT, gpsT, NifT, RaioT, precokmT, pwT, 0, 0,0);
        if (this.AllTransportadoras.containsKey(t.getCodEmpresa())) throw new JaRegistadoException("Esta Transportadora ja foi registada! : " + t.getCodEmpresa());
        else this.AllTransportadoras.put(CodEmpresaT, t.clone());
    }    
    
    public void nregistarLoja(String CodLoja, String NomeL, GPS gpsL, String pwL) throws JaRegistadoException{
        Loja l = new Loja(CodLoja, NomeL, gpsL, pwL);
        if (this.AllLojas.containsKey(l.getCodLoja())) throw new JaRegistadoException("Esta Loja ja foi registada! : " + l.getCodLoja());
        else this.AllLojas.put(CodLoja,l.clone());
    }   
    
    public void nregistarEncomenda(String CodEncomenda,String CodUtilizador,String loja,double peso,ArrayList<LinhaEncomenda> linha)throws JaRegistadoException{
       Encomenda e = new Encomenda(CodEncomenda,CodUtilizador,loja,peso,linha);
       if (this.pedidosEncomendas.containsKey(e.getCodEncomenda())) throw new JaRegistadoException("Esta Encomenda ja foi pedida! : " + e.getCodEncomenda());
        else this.pedidosEncomendas.put(CodEncomenda,e.clone());
    }
    
    //Registar um utilizador, um voluntário, uma empresa transportadora e uma loja (a partir do CSV)
    
    public void readregistarUtilizador(String codU, String nomeU, GPS gpsU, String pwU) throws JaRegistadoException{
        Utilizador u = new Utilizador(codU, nomeU, gpsU, pwU);
        if (this.AllUtilizadores.containsKey(u.getCodUtilizador())) throw new JaRegistadoException("Este utilizador ja foi registado! : " + u.getCodUtilizador());
        else this.AllUtilizadores.put(codU,u.clone());
    }    
    
    public void readregistarVoluntario(String codV, String nomeV, GPS gpsV, double raioV, String pwV, int classi) throws JaRegistadoException{
        Voluntario v = new Voluntario(codV, nomeV, gpsV, raioV, pwV, classi);
        if (this.AllVoluntarios.containsKey(v.getCodVoluntario())) throw new JaRegistadoException("Este Voluntario ja foi registado! : " + v.getCodVoluntario());
        else this.AllVoluntarios.put(codV, v.clone());
    }
    
    public void readregistarTransportadora(String CodEmpresaT, String NomeT, GPS gpsT, String NifT, double RaioT, double precokmT, String pwT, int classi, double fat,double km) throws JaRegistadoException{
        Transportadora t = new Transportadora(CodEmpresaT, NomeT, gpsT, NifT, RaioT, precokmT, pwT, classi, fat,km);
        if (this.AllTransportadoras.containsKey(t.getCodEmpresa())) throw new JaRegistadoException("Esta Transportadora ja foi registada! : " + t.getCodEmpresa());
        else this.AllTransportadoras.put(CodEmpresaT, t.clone());
    }    
    
    public void readregistarLoja(String CodLoja, String NomeL, GPS gpsL, String pwL) throws JaRegistadoException{
        Loja l = new Loja(CodLoja, NomeL, gpsL, pwL);
        if (this.AllLojas.containsKey(l.getCodLoja())) throw new JaRegistadoException("Esta Loja ja foi registada! : " + l.getCodLoja());
        else this.AllLojas.put(CodLoja,l.clone());
    }   
    
    public void readregistarEncomenda(String CodEncomenda,String CodUtilizador,String loja,double peso,ArrayList<LinhaEncomenda> linha)throws JaRegistadoException{
       Encomenda e = new Encomenda(CodEncomenda,CodUtilizador,loja,peso,linha);
       if (this.pedidosEncomendas.containsKey(e.getCodEncomenda())) throw new JaRegistadoException("Esta Encomenda ja foi pedida! : " + e.getCodEncomenda());
        else this.pedidosEncomendas.put(CodEncomenda,e.clone());
    }
    
    public void registarEncomendaAceite(String codEncomenda){
        if (this.pedidosEncomendas.containsKey(codEncomenda))
            this.encConcluidas.put(codEncomenda, this.pedidosEncomendas.get(codEncomenda));
    }    
    
    //Validar o acesso à aplicação utilizando as credenciais (Codigo e password)
    
    public boolean validaUtilizador(String CodU, String pass){
        return this.AllUtilizadores.containsKey(CodU) && this.AllUtilizadores.get(CodU).getPwU().equals(pass);
    }    
    
    public boolean validaVoluntario(String CodV, String pass){
        return this.AllVoluntarios.containsKey(CodV) && this.AllVoluntarios.get(CodV).getPwV().equals(pass);
    }  
    
    public boolean validaTransportadora(String CodT, String pass){
        return this.AllTransportadoras.containsKey(CodT) && this.AllTransportadoras.get(CodT).getPwT().equals(pass);
    }    
    
    public boolean validaLoja(String CodL, String pass){
        return this.AllLojas.containsKey(CodL) && this.AllLojas.get(CodL).getPwL().equals(pass);
    }    
    
   //classificar um voluntario
   public void classificaVoluntario(String codV, int classificacao)throws NaoExisteException{ 
       if(!this.AllVoluntarios.containsKey(codV)) throw new NaoExisteException("Esse Voluntario nao existe! :"+codV);
       else this.AllVoluntarios.get(codV).setClassificacao((this.AllVoluntarios.get(codV).getClassificacao() + classificacao) / 2);
    }

   //classificar transportadora
   public void classificaTransportadora(String codT, int classificacao)throws NaoExisteException{ 
       if(!this.AllTransportadoras.containsKey(codT)) throw new NaoExisteException("Essa Transportadora nao existe! :"+codT);
       else this.AllTransportadoras.get(codT).setClassificacao((this.AllTransportadoras.get(codT).getClassificacao() + classificacao) / 2);
   }
   
   public void adicionarUsuario(String codU){ 
          this.usuariosenc.add(codU);
   }
   public void adicionarKm(Double km){ 
          this.kmT.add(km);
   }
   public void encomendasDisponiveisV(String codU){
       for(Map.Entry<String, Voluntario> v: this.AllVoluntarios.entrySet()){
           for(Map.Entry<String, Encomenda> e: this.pedidosEncomendas.entrySet()){
               if(e.getValue().getCodUtilizador().equals(codU) && v.getValue().inRangeV(this.AllLojas.get(e.getValue().getCodLoja()), this.AllUtilizadores.get(codU)))
                    System.out.println("\nVoluntario: " + v.getKey() + "\nEncomenda: " + e.getValue() + "Custo: " + ge.custoV(this.AllLojas.get(e.getValue().getCodLoja()), this.AllUtilizadores.get(codU), v.getValue(), e.getValue()));
           }    
       }    
   }
   
   public void encomendasDisponiveisT(String codU){
       for(Map.Entry<String, Transportadora> t: this.AllTransportadoras.entrySet()){
           for(Map.Entry<String, Encomenda> e: this.pedidosEncomendas.entrySet()){
               if(e.getValue().getCodUtilizador().equals(codU) && t.getValue().inRangeT(this.AllLojas.get(e.getValue().getCodLoja()), this.AllUtilizadores.get(codU)))
                   System.out.println("\nTransportadora: " + t.getKey() + "\nEncomenda: " + e.getValue() + "Custo: " + ge.custoT(this.AllLojas.get(e.getValue().getCodLoja()), this.AllUtilizadores.get(codU), t.getValue(), e.getValue()));
           }    
       } 
   }
   
   //Top10Utilizadores 
   public static Map<String, Integer> getWordFrequencies(List<String> words) {
        Map<String, Integer> wordFrequencies = new LinkedHashMap<String, Integer>();
        if (words != null) {
            for (String word : words) {
                if (word != null) {
                    word = word.trim();
                    if (!wordFrequencies.containsKey(word)) {
                        wordFrequencies.put(word, 0);
                    }
                    int count = wordFrequencies.get(word);
                    wordFrequencies.put(word, ++count);
                }
            }
        }
        return wordFrequencies;
    } 
    
   //Somar faturado de uma empresa
   
   public void fatsum(String codU, String codT, String codE, String codL){
       this.AllTransportadoras.get(codT).somaFat(ge.custoT(this.AllLojas.get(codL), this.AllUtilizadores.get(codU), this.AllTransportadoras.get(codT), this.pedidosEncomendas.get(codE)));
       //custoT(Loja l, Utilizador u, Transportadora t, Encomenda e)
   }
   
   
    public double kmsum(String codT,String codL,String codU){ 
       this.AllTransportadoras.get(codT).somakm(ge.kmtotal(this.AllTransportadoras.get(codT),this.AllLojas.get(codL),this.AllUtilizadores.get(codU)));
       return this.AllTransportadoras.get(codT).getKm();
    }
    
   public void pedidoToConcluido(Encomenda e){
       this.encConcluidas.put(e.getCodEncomenda(), e.clone());
       this.pedidosEncomendas.remove(e.getCodEncomenda());
    } 
    
   public ArrayList<Double> sortlista(ArrayList<Double> list){ 
       Collections.sort(list);
       Collections.reverse(list);
       return list;
    }
   public ArrayList<Double> so10(ArrayList<Double> list){ 
       ArrayList<Double> listnova=new ArrayList<>();
      for(int i = 0;i<11 && i < list.size() ;i++){ 
         listnova.add(list.get(i));
        }
      return listnova;
    }
   
   public Map<String,Double> buscatransportadoras(ArrayList<Double> lista){ 
       Map<String,Double> listanova = new LinkedHashMap<>();
       for(int i=0;i < lista.size();i++){ 
          for(Map.Entry<String,Transportadora> t : this.AllTransportadoras.entrySet()){
             
                if(Double.compare(lista.get(i), t.getValue().getKm()) == 0){ 
                  
                  listanova.put(t.getKey(),t.getValue().getKm());
                }
            }
           
        }
       
       return listanova;
    }
}

