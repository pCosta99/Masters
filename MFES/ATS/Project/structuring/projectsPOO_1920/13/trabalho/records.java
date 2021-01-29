import java.util.*;
import java.io.*;
import java.nio.file.*;
import java.util.stream.Collectors;
import java.util.Collections;
public class records
{
   private  Map<String, encomenda> encomendas;
   private  Map<String, Utilizador> users;
   private  Map<String, Loja> lojas;
   private  Map<String, Transportadora> trans;
   private  Map<String, Voluntario> voluntarios;
   private  List<String> aceites;
   private  List<String> inativos;
   public records(String txt){
       this.users= new HashMap<>();
       this.lojas= new HashMap<>();
       this.trans= new HashMap<>();
       this.voluntarios= new HashMap<>();
       this.encomendas= new HashMap<>();
       this.aceites=new ArrayList<>();
       this.inativos=new ArrayList<>();
       String file = ler(txt);
       List<String> linhas = new ArrayList(Arrays.asList(file.split(":")));
       List<Character>  oc = new ArrayList<>();
       Iterator<String> itr = null; 
       oc.add('u');
       oc.add('v');
       oc.add('t');
       oc.add('l');
       oc.add('e');
       oc.add('r');
       oc.add('f');
       itr = linhas.iterator(); 
       while (itr.hasNext()) 
       {  String s=itr.next();
           if (oc.contains(s.charAt(0))){
               List<String> lista = new ArrayList(Arrays.asList(s.split("\\r?\\n")));
               lista.remove(new String("Utilizador"));
               lista.remove(new String("Voluntario"));
               lista.remove(new String("Loja"));
               lista.remove(new String("Encomenda"));
               lista.remove(new String("Transportadora"));
               allocar(lista);
               
           }
       
           } 
       
   }
   
   public String ler(String txt) {
       String path = txt;
       try {
            String content = Files.readString(Paths.get(path));
            return (content);
        } 
        catch (IOException e) {
            e.printStackTrace();
        }
        return("");
    }
   
       
   public void allocar(List<String> bruv){
       for(String a: bruv){
           if(a.charAt(0)=='u'){
               List<String> temp = new ArrayList(Arrays.asList(a.split(",")));
               adicionaUtilizador(new Utilizador(temp.get(0),temp.get(1),Double.parseDouble(temp.get(2)),Double.parseDouble(temp.get(3))));
           }
           if(a.charAt(0)=='v'){
               List<String> temp = new ArrayList(Arrays.asList(a.split(",")));
                if (temp.size()>2)
                   adicionaVoluntario(new Voluntario(temp.get(0),temp.get(1),Double.parseDouble(temp.get(2)),Double.parseDouble(temp.get(3)),Double.parseDouble(temp.get(4))));
               else{
                   this.inativos.add(temp.get(0));
               }
               
           }
           if(a.charAt(0)=='t'){
               List<String> temp = new ArrayList(Arrays.asList(a.split(",")));
               adicionaTransportadora(new Transportadora(temp.get(0),temp.get(1),Double.parseDouble(temp.get(2)),Double.parseDouble(temp.get(3)),Double.parseDouble(temp.get(4)),Double.parseDouble(temp.get(5))));
           }
           if(a.charAt(0)=='l'){
               List<String> temp = new ArrayList(Arrays.asList(a.split(",")));
               adicionaLoja(new Loja(temp.get(0),temp.get(1),Double.parseDouble(temp.get(2)),Double.parseDouble(temp.get(3))));
           }
           if(a.charAt(0)=='e'){
               List<String> temp = new ArrayList(Arrays.asList(a.split(",")));
               if (temp.size()>1)
                   adicionaenc(temp.get(0),temp.get(1),temp.get(2),Double.parseDouble(temp.get(3)),temp);
               else{
                   this.aceites.add(a);
               }
           }
       }
   }
   
   public void adicionaUtilizador(Utilizador u){
       this.users.put(u.getcod(),u.clone());
   }
   public void adicionaVoluntario(Voluntario v){
       this.voluntarios.put(v.getcod(),v.clone());
   }
   public void adicionaTransportadora(Transportadora t){
       this.trans.put(t.getcod(),t.clone());
   }
   public void adicionaLoja(Loja l){
       this.lojas.put(l.getcod(),l.clone());
   }
   public void adicionaenc(String enc,String user,String loja,double peso,List<String> linhas){
       encomenda e=new encomenda(enc,user,loja,peso);
       Loja m=this.lojas.get(loja);
       linhas.remove(0);
       linhas.remove(0);
       linhas.remove(0);
       linhas.remove(0);
       Iterator<String> itr = null;
       itr = linhas.iterator();
       while (itr.hasNext()){
           LinhaEncomenda li = new LinhaEncomenda(itr.next(),itr.next(),Double.parseDouble(itr.next()),Double.parseDouble(itr.next()));
           e.adicionaLinha(li);
           m.adicionaproduto(li);
       }
       this.encomendas.put(e.getcod(),e.clone());
       this.lojas.put(loja,m);
   }
   
   public Map<String,Loja> getAllLojas() {
        return this.lojas.entrySet().stream().collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue().clone()));
   }
   public Map<String,encomenda> getAllenc() {
        return this.encomendas.entrySet().stream().collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue().clone()));
   }
   public Map<String,Utilizador> getAlluser() {
        return this.users.entrySet().stream().collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue().clone()));
   }
   public List<String> getaceites(){
       List<String> res = new ArrayList<>();
       for(String s : aceites) {
           res.add(s);
       }
        return res;
    }
   public List<String> getinativos(){
      List<String> res = new ArrayList<>();
      for(String s : inativos) {
          res.add(s);
      }
      return res;
    }
   
}                        


