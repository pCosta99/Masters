import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.TreeSet;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;
import java.util.Random;
import java.io.*;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import ClassesAux.*;
import Interfaces.*;
import Exceptions.*;
public class Central implements Serializable
{
    private Map<String,Encomenda> encomendas;                   //idEncomenda e encomendas
    private Map<String,Utilizador> utilizadores;                //idUsuaio e usuario
    private Map<String,Loja> lojas;                             //idLojas e lojas
    private Map<String,Entregador> entregadores;                //idEntregador e entregadores
    private Set<String> encAceitas;                             //idEncomendas
    private List<List<String>> propostas;                       //List=["idUtilizador","idEncomenda","idEntregador","Preco"]
    private Map<String,Entrega> entregas;                       //idEncomenda e idEntregador,idRecetor,LocalData
    private Condicoes condicoes;                                //metreologia,transito,feriado,fator

    //Construtores,Set,Get,equals,clone e toString;
    
    public Central()
    {
        this.encomendas=new HashMap<>();
        this.lojas=new HashMap<>();
        this.utilizadores= new HashMap<>();
        this.entregadores=new HashMap<>();
        this.encAceitas=new TreeSet<>();
        this.propostas=new ArrayList<>();
        this.entregas=new  HashMap<>();
        this.condicoes= new Condicoes();
    }
    public Central(Central central){
        this.encomendas=central.getEncomendas();
        this.lojas=central.getLojas();
        this.utilizadores= central.getUtilizadores();
        this.entregadores=central.getEntregadores();
        this.encAceitas=central.getEncAceitas();
        this.propostas=central.getPropostas();
        this.condicoes=central.getCondicoes();
    }
    public Map<String,Encomenda> getEncomendas(){
     return this.encomendas.values().stream().collect(Collectors.toMap(Encomenda::getCodEncomenda,Encomenda::clone));
    }
    public void setEncomendas(Map<String,Encomenda> reg){
        this.encomendas = reg.values().stream().collect(Collectors.toMap(Encomenda::getCodEncomenda,Encomenda::clone));
    }
    public Map<String,Utilizador> getUtilizadores(){
        return this.utilizadores.values().stream().collect(Collectors.toMap(Utilizador::getCodUtilizador,Utilizador::clone));
    }
    public void  setUtilizadores(Map<String,Utilizador> uti){
        this.utilizadores = uti.values().stream().collect(Collectors.toMap(Utilizador::getCodUtilizador,Utilizador::clone));
    }
    public Map<String,Entregador> getEntregadores(){
       return this.entregadores.values().stream().collect(Collectors.toMap(Entregador::getCodigo,Entregador::clone));
    }
    public void setEntregadores(Map<String,Entregador> ent){
        this.entregadores = ent.values().stream().collect(Collectors.toMap(Entregador::getCodigo,Entregador::clone));
    }
    public Map<String,Loja> getLojas(){
        return this.lojas.values().stream().collect(Collectors.toMap(Loja::getCodLoja,Loja::clone));
    }
    public  void setLojas(Map<String,Loja> loj){
        this.lojas = loj.values().stream().collect(Collectors.toMap(Loja::getCodLoja,Loja::clone));
    }
    public Map<String,Entrega> getEntregas(){
        return this.entregas.values().stream().map(p->p.clone()).collect(Collectors.toMap(Entrega::getIdEncomenda,Entrega::clone));
    }
    public void setEntregas(Map<String,Entrega> ent){
        this.entregas=ent.values().stream().map(p->p.clone()).collect(Collectors.toMap(Entrega::getIdEncomenda,Entrega::clone));
    }
    public Set<String> getEncAceitas(){
        return this.encAceitas.stream().collect(Collectors.toSet());
    }
    public void setEncAceitas(Set<String> acei){
        this.encAceitas = acei.stream().collect(Collectors.toSet());
    }
    public Condicoes getCondicoes(){return this.condicoes.clone();}
    public void setCondicoes(Condicoes p){this.condicoes=p.clone();}
    public List<List<String>> getPropostas(){
        List<List<String>> res = new ArrayList<>();
        for(List<String> a:this.propostas){
            List<String> aux= new ArrayList<>();
            for(String b:a){aux.add(b);}
            res.add(a);
        }
        return res;
    }
    public void setPropostas(List<List<String>> prop){
        this.propostas = new ArrayList<List<String>>();
        for(List<String> a:prop){
            List<String> aux = new ArrayList<>();
            for(String b:a){aux.add(b);}
            this.propostas.add(a);
        }
    }
    public GPS getGps(String id){
    GPS res=new GPS();
    switch(id.substring(0, 1)){
        case "u" :if(utilizadores.containsKey(id)) res=utilizadores.get(id).getGps();break;
        case "v" :if(entregadores.containsKey(id)) res=entregadores.get(id).getGps();break;
        case "t" :if(entregadores.containsKey(id)) res=entregadores.get(id).getGps();break;
        case "l" :if(lojas.containsKey(id))        res=lojas.get(id).getGps();break;
        }
    return res;
    }
    public String toString(){
        StringBuilder sb =new StringBuilder();
        for(Utilizador a:this.utilizadores.values()) sb.append(a);
        for(Entregador a:this.entregadores.values()) sb.append(a);
        for(Loja a:this.lojas.values()) sb.append(a);
        for(Encomenda a:this.encomendas.values()) sb.append(a);
        for(String a:this.encAceitas) sb.append("Aceite:").append(a).append("\n");
        for (List<String> a:this.propostas){
        sb.append("Proposta;").append(a.get(0)).append(",").append(a.get(1)).append(",").append(a.get(2))
        .append(",").append(a.get(3)).append("\n");
        }
        for(Entrega a:this.entregas.values())sb.append(a);
        sb.append(this.condicoes);
        return sb.toString();
    }
    public boolean equals(Central o){
        if (this==o) return true;
        if ((o == null) || (this.getClass() != o.getClass()))
           return false;
           Central p = (Central) o;
           return (p.getUtilizadores().equals(this.utilizadores) && 
           p.getEntregadores().equals(this.entregadores) &&
           p.getEncomendas().equals(this.encomendas) &&
           p.getEncAceitas().equals(this.encAceitas) &&
           p.getCondicoes().equals(this.condicoes)
           );
    }
    
    //Carregar alguns Objetos para testes;
    
    public void load  (String ficheiro) throws FileNotFoundException,IOException,Exception
    {
            BufferedReader buff= new BufferedReader(new FileReader (ficheiro));
            String linha=buff.readLine();
            while (linha!=null)
            {
                var linhas=linha.split(":");
                //System.out.println(linhas[1]);
                switch(linhas[0]){
                case"Utilizador":adicionaUtilizador(linhas[1]);break;
                case"Voluntario":adicionaVoluntario(linhas[1]);break;
                case"Transportadora":adicionaTransportadora(linhas[1]);break;
                case"Loja":adicionaLoja(linhas[1]);break;
                case"Encomenda":adicionaEncomenda(linhas[1]);break;
                case"Aceite":adicionaAceita(linhas[1]);break;
                case"EncomendaMedica":adicionaEncomendaMedica(linhas[1]);break;
               }
               linha=buff.readLine();
            }
            buff.close();
    }
    public void reset() {
        this.encomendas=new HashMap<>();
        this.lojas=new HashMap<>();
        this.utilizadores= new HashMap<>();
        this.entregadores=new HashMap<>();
        this.encAceitas=new TreeSet<>();
        this.propostas=new ArrayList<>();
        this.entregas=new  HashMap<>();
        this.condicoes= new Condicoes();
    }
    
    //Registro e LogIn

    public void adicionaUtilizador(String utilizador)throws IdInvalido,AlreadyExists,Exception{
        String [] array=utilizador.split(",");
        if(!array[0].startsWith("u")) throw new IdInvalido();
        if(this.utilizadores.containsKey(array[0])) throw new AlreadyExists();
          Utilizador res=new Utilizador(utilizador);            //new Utilizador(utilizador) throws Exceptions
          this.utilizadores.put(res.getCodUtilizador(),res);    //pois a string pode nao ter elementos sufcientes dando Array.PointFailure
                                                                //os parses de strings para double ou int podem dar errado
    }
    public void adicionaVoluntario(String voluntario)throws IdInvalido,AlreadyExists,Exception{
        String [] array=voluntario.split(",");
        if(!array[0].startsWith("v")) throw new IdInvalido();
        if(this.entregadores.containsKey(array[0])) throw new AlreadyExists();
        Voluntario res=new Voluntario(voluntario);
        this.entregadores.put(res.getCodigo(),res);
    }
    public void adicionaTransportadora(String transportadora)throws IdInvalido,AlreadyExists,Exception{
        String [] array=transportadora.split(",");
        if(!array[0].startsWith("t")) throw new IdInvalido();
        if(this.entregadores.containsKey(array[0])) throw new AlreadyExists();
        Transportadora res=new Transportadora(transportadora);
        this.entregadores.put(res.getCodigo(),res);
    }
    public void adicionaLoja(String loja) throws IdInvalido,AlreadyExists,Exception{
        String [] array=loja.split(",");
        if(!array[0].startsWith("l")) throw new IdInvalido();
        if(this.lojas.containsKey(array[0])) throw new AlreadyExists();
         Loja res=new Loja(loja);
         this.lojas.put(res.getCodLoja(),res);
    }
    public boolean logIn (String id,String chave){
        boolean res=false;
        switch(id.substring(0, 1)){//id.subString(0,1) com id="u9098"="u" começa em 0 e tem 1 de comprimento od.subString(1,2)="90";
        case "u" :if(utilizadores.containsKey(id) && chave.equals("aaaa")) res=true;break;
        case "v" :if(entregadores.containsKey(id)&& chave.equals("aaaa")) res=true;break;
        case "t" :if(entregadores.containsKey(id)&& chave.equals("aaaa")) res=true;break;
        case "l" :if(lojas.containsKey(id)&& chave.equals("aaaa")) res=true;break;
        }
        return res;
     }
    
     //Encomendas
    
    public void adicionaEncomenda(String encomenda) throws NotFound,IdInvalido,AlreadyExists,Exception{
        String [] array=encomenda.split(",");
        if(!array[0].startsWith("e")) throw new IdInvalido();
        if(this.encomendas.containsKey(array[0])) throw new AlreadyExists();
        if(!this.utilizadores.containsKey(array[1])||!this.lojas.containsKey(array[2])) throw new NotFound();
        Encomenda res=new EncomendaNormal(encomenda);
        this.encomendas.put(res.getCodEncomenda(),res);
    }
    public void adicionaEncomendaMedica(String encomenda) throws NotFound,IdInvalido,AlreadyExists,Exception{
        String [] array=encomenda.split(",");
        if(!array[0].startsWith("e")) throw new IdInvalido();
        if(this.encomendas.containsKey(array[0])) throw new AlreadyExists();
        if(!this.utilizadores.containsKey(array[1])||!this.lojas.containsKey(array[2])) throw new NotFound();
        Encomenda res=new EncomendaMedica(encomenda);
        this.encomendas.put(res.getCodEncomenda(),res);
    }
    //Encomenda:e5003,u78,l58,23.618465,p10,Condicionador,4.293345,42.83252
    public String adicionaEncomenda(String idUtilizador,String idLoja,String produtos,double peso) 
        throws NotFound,ProductNotFound,Exception{
        int i=0;int rand_int1=0;
        Encomenda res=null;
        String cod="";
        Random rand = new Random();
        rand_int1 = rand.nextInt(1000);
        cod="e"+rand_int1;
        boolean flag=false;
        while (this.encomendas.containsKey(cod)||i<1000){
            rand_int1 = rand.nextInt(1000);
            cod="e"+rand_int1;i++;
        }
        if(!this.utilizadores.containsKey(idUtilizador)||!this.lojas.containsKey(idLoja)) throw new NotFound();
        Loja l=this.lojas.get(idLoja);
        String [] arr=produtos.split(",");
        for(int j=0;j<arr.length;j=j+4){
            if(l.tenhoProduto(arr[j])==false) throw new ProductNotFound ();
            if ((arr[j]).startsWith("pm")) flag=true;
            }
        if (flag) res=new EncomendaMedica (cod+","+idUtilizador+","+idLoja+","+peso+","+produtos);
        else res=new EncomendaNormal(cod+","+idUtilizador+","+idLoja+","+peso+","+produtos);
        this.encomendas.put(res.getCodEncomenda(),res);
        //System.out.println(cod+","+idUtilizador+","+idLoja+","+peso+","+produtos);
        //System.out.println(res.toString());
        return (cod+","+idUtilizador+","+idLoja+","+peso+","+produtos);
    }
    public void adicionaAceita(String aceite) throws NotFound,AlreadyExists{
        if(!this.encomendas.containsKey(aceite)) throw new NotFound ();
        if(this.encAceitas.contains(aceite)) throw new AlreadyExists();
        this.encAceitas.add(aceite);
    }
    public boolean possoEntregar(Encomenda enc,Entregador ent){
     double raio = ent.getRaio();
     if(!this.encAceitas.contains(enc.getCodEncomenda()))
     {
        //System.out.println("Nao esta em encAceites");
        if(enc instanceof EncomendaMedica==false || (enc instanceof EncomendaMedica && ent.aceitoTransporteMedicamentos()==true))
        {
         //System.out.println("Nao e EncomendaMedica"); 
        GPS utiGps=getGps(enc.getCodUtilizador());GPS lojaGps=getGps(enc.getCodLoja());GPS entGps=ent.getGps();
        if (entGps.estaoDentroDoRaio(raio,lojaGps) && entGps.estaoDentroDoRaio(raio,utiGps)) 
            {/*System.out.println("Esta dentro do raio")*/;return true;};
        }
     }//System.out.println("esta em encAceites");
     return false;
    }
    public String encQuePossoEntregar(String id) throws NotFound{
        List<Encomenda> res=new ArrayList<>();                       //for(Encomenda e:this.encomendas.values()){
        if(!this.entregadores.containsKey(id))throw new NotFound();  //System.out.println(e.toString());
         Entregador t = this.entregadores.get(id);                   //if(possoEntregar(e,t)) System.out.println(e.toString());                 
         return this.encomendas.values().stream().filter(p->possoEntregar(p,t)).map(e->e.toString()).reduce("",String::concat);
    }
    /*public int filaNaLoja(String idLoja){
        int res=(int)this.encomendas.values().stream().filter(p->p.getCodLoja().equals(idLoja)).filter(p->!this.encAceitas.contains(idLoja)).count();
        return res;
    }*/
    public double precoRotaTransportadora(String encomendas,String entregador)throws NotEntregadorCobrador,IdInvalido{
       double distancia=0,raio;
       int filaTotal=0;
       double res;
       Set<String> lojasIdo = new TreeSet<>();
       GPS gpsAux=null,gpsAnterior;
       String idLoja="/a";
       if (!(this.entregadores.get(entregador) instanceof EntregadorCobrador)) throw new NotEntregadorCobrador();
       EntregadorCobrador tr=(EntregadorCobrador)this.entregadores.get(entregador);
       raio=tr.getRaio();
       Encomenda encAux=null;
       gpsAnterior=tr.getGps();
       var linhas=encomendas.split(",");
        for(int i=0;i<linhas.length;i++){
          if(!this.encomendas.containsKey(linhas[i])) throw new IdInvalido ();
          encAux=this.encomendas.get(linhas[i]);
          idLoja=encAux.getCodLoja();
               if(!lojasIdo.contains(idLoja)) 
               {
                gpsAux=this.lojas.get(idLoja).getGps();
                //System.out.println(gpsAux.toString());
                distancia+=gpsAnterior.distancia(gpsAux);
                //System.out.println(distancia);
                gpsAnterior=gpsAux;
                filaTotal+=this.lojas.get(idLoja).getFila();
                lojasIdo.add(idLoja);
               }
               gpsAux=this.utilizadores.get(encAux.getCodUtilizador()).getGps();
               //System.out.println(gpsAux.toString());
               distancia+=gpsAnterior.distancia(gpsAux);
               gpsAnterior=gpsAux;
               //System.out.println(distancia);
        }
       res=((distancia/tr.getVelMedia())+(filaTotal*(Loja.getTempoEsperaFila())))*tr.getVelMedia()*tr.getPrecokm()*this.condicoes.getFator();
       res=res/linhas.length;
       return res;
        //return this.parametros.precoTotal(filaTotal,tr.getPrecokm(),linhas.length,distancia);
    }
    
    //Propostas
    
    //proposta= List=["idUtilizador","idEncomenda","idTransportadora","Preco"]
    public void adicionaProposta(String entregador,String encomenda,String preco) throws CantDeliverEncomenda,NotFound{
       List<String> aux=new ArrayList<>(4);
       if(!this.entregadores.containsKey(entregador)) throw new NotFound();
       if(!this.encomendas.containsKey(encomenda)) throw new NotFound();
       Encomenda enc=this.encomendas.get(encomenda);
       Entregador t=this.entregadores.get(entregador);
       if(!possoEntregar(enc,t)) throw new CantDeliverEncomenda();
        aux.add(enc.getCodUtilizador());
        aux.add(encomenda);
        aux.add(entregador);
        aux.add(preco);
        this.propostas.add(aux);
    }
    public String verPropostas(String idUtilizador){
       //List=["idUtilizador","idEncomenda","idTransportadora","Preco"]
       StringBuilder sb=new StringBuilder();
        for(List<String> entrada:this.propostas){
            if(entrada.get(0).equals(idUtilizador))
           {
            sb.append("Encomenda:").append(entrada.get(1));
            sb.append(" Entregador:").append(entrada.get(2));
            sb.append(" Preco:").append(entrada.get(3));
            sb.append("\n");
           }
        }
        return sb.toString();
    }
    public void aceitarProposta(String idUtilizador,String idEntregador,String idEncomenda) throws PropostaInvalida,NotFound{
        boolean verifica=false;
        if(!this.entregadores.containsKey(idEntregador)) throw new NotFound();
        if(!this.encomendas.containsKey(idEncomenda)) throw new NotFound();
        Encomenda e=this.encomendas.get(idEncomenda);
        Entregador t=this.entregadores.get(idEntregador);
        for(List<String> entrada:this.propostas){
        if(entrada.get(1).equals(idEncomenda) && entrada.get(0).equals(idUtilizador)&& entrada.get(2).equals(idEntregador))
            verifica=true;
        }
        if(!verifica) throw new PropostaInvalida();
        this.encAceitas.add(idEncomenda);
        this.propostas.removeIf(n->n.get(1).equals(idEncomenda));
    }
    public void aceitarEntregar(String idEncomenda,String idVoluntario) throws NotFound,CantDeliverEncomenda{
       if(!this.entregadores.containsKey(idVoluntario)) throw new NotFound();
       if(!this.encomendas.containsKey(idEncomenda)) throw new NotFound();
       Encomenda e=this.encomendas.get(idEncomenda);
       Entregador t=this.entregadores.get(idVoluntario);
       if(!possoEntregar(e,t)) throw new CantDeliverEncomenda();
       this.encAceitas.add(idEncomenda);
       this.propostas.removeIf(n->n.get(1).equals(idEncomenda));
    }
    
    //Entregas
    
    //data tem de estar eg:2007-12-03
    public void adicionarEntrega(String idEncomenda,String idEntregador,String data,double preco) throws NotFound,AlreadyExists{
      if (!this.encAceitas.contains(idEncomenda)) throw new NotFound();
      if (this.entregas.containsKey(idEncomenda)) throw new AlreadyExists();
        Encomenda enc= this.encomendas.get(idEncomenda);
        Loja l = this.lojas.get(enc.getCodLoja());
        Utilizador u =this.utilizadores.get(enc.getCodUtilizador());
        Entregador e= this.entregadores.get(idEntregador);
        Entrega res = new Entrega(idEncomenda,idEntregador,enc.getCodUtilizador(),preco);
        if(data!="now"){
            LocalDate localDate = LocalDate.parse(data);
            res.setDataEntrega(localDate);
        }
        double distancia = (e.getGps().distancia(l.getGps())+l.getGps().distancia(u.getGps()));
        e.addKmAndados(distancia);
        if (e instanceof EntregadorPontos){
        EntregadorPontos v = (EntregadorPontos) e;
        v.somaPontos(distancia*this.condicoes.getFator());
        }
        this.entregas.put(idEncomenda,res);
    }
    public String verEntregasTempo(String sDataDe,String sDataAte){
        LocalDate dataDe = LocalDate.parse(sDataDe);
        LocalDate dataAte = LocalDate.parse(sDataAte);
        return this.entregas.values().stream().filter(n->n.getDataEntrega().isAfter(dataDe))
        .filter(n->n.getDataEntrega().isBefore(dataAte)).map(n->n.toString()).reduce("",String::concat);
    }
    public String verEntregasDe(String id) throws NotFound{
     if(!this.entregadores.containsKey(id)) throw new NotFound();
     return this.entregas.values().stream().filter(n->n.getIdEntregador().equals(id)).map(n->n.toString()).reduce("",String::concat);
    }
    public String verificarEntrega(String idEncomenda){
     if(this.entregas.containsKey(idEncomenda)) return this.entregas.get(idEncomenda).toString();
     else if (this.encAceitas.contains(idEncomenda)) return "Por Entregar";
     else if (this.encomendas.containsKey(idEncomenda)) return "Ainda Nao Aceita";
     return "Id Invalido";
    }
    public String minhasEntregas(String idUtilizador){
    return this.entregas.values().stream().filter(n->n.getIdRecetor().equals(idUtilizador)).map(n->n.toString()).reduce("",String::concat);
    }
    
    //Querys
    
    public String top10Utilizadores(){
     Map <String,Integer> aux = new HashMap<>();
     StringBuilder res=new StringBuilder();
     this.entregas.values().stream().forEach(n->aux.put(n.getIdRecetor(),0));
     this.entregas.values().stream().forEach(n->aux.put(n.getIdRecetor(),aux.get(n.getIdRecetor())+1));
     aux.entrySet().stream().sorted((n1,n2) -> n2.getValue()-n1.getValue()).limit(10)
     .forEach(n->res.append(n.getKey()).append("-").append(n.getValue()).append("\n"));
     return res.toString();
    }
    /*public int distanciaEncomenda(String idEncomenda,String idEntregador){
     Encomenda enc=this.encomendas.get(idEncomenda);
     GPS gpsLoja=getGps(enc.getCodLoja());
     GPS gpsUti=getGps(enc.getCodUtilizador());
     GPS gpsTra=getGps(idEntregador);
     return (int) (gpsTra.distancia(gpsLoja)+gpsLoja.distancia(gpsUti));
    }*/
    public String top10Transportadoras(){
     Map <String,Integer> aux = new HashMap<>();
     StringBuilder res=new StringBuilder();
     this.entregadores.values().stream().filter(n->n instanceof Transportadora).sorted((n1,n2) -> (int) (n2.getKmAndados() - n1.getKmAndados()))
     .limit(10).forEach(n->res.append(n.getCodigo()).append("-").append(n.getKmAndados()).append("\n"));
     return res.toString();
    }
    public double faturacaoTransportadora(String idEntregador,String sDataDe,String sDataAte){
     LocalDate dataDe = LocalDate.parse(sDataDe);
     LocalDate dataAte = LocalDate.parse(sDataAte);
     return this.entregas.values().stream().filter(n->n.getIdEntregador().equals(idEntregador))
     .filter(n->n.getDataEntrega().isAfter(dataDe)).filter(n->n.getDataEntrega().isBefore(dataAte)).mapToDouble(n->n.getPreco()).sum();
    }
    
    //Extra
    
    public double veClassificacao(String idEntregador) throws NotFound{
       if(!this.entregadores.containsKey(idEntregador)) throw new NotFound();
       return this.entregadores.get(idEntregador).getClassificacao();
    }
    public void classifica(String idEntregador,double voto)throws NotFound{
      if(!this.entregadores.containsKey(idEntregador)) throw new NotFound();
      this.entregadores.get(idEntregador).classifica(voto);
    }
    public boolean aceitoTransporteMedicamentos(String idEntregador) throws NotFound{
      if(!this.entregadores.containsKey(idEntregador)) throw new NotFound();
      return this.entregadores.get(idEntregador).aceitoTransporteMedicamentos();
    }
    public void aceitaMedicamentos(String idEntregador,boolean b) throws NotFound{
      if(!this.entregadores.containsKey(idEntregador)) throw new NotFound();
      this.entregadores.get(idEntregador).aceitaMedicamentos(b);
    }
    public void setFila(String loja,int fila)throws ValorInvalido{
        if (fila<0) throw new ValorInvalido();
        this.lojas.get(loja).setFila(fila);
    }
    public double getPontos(String id) throws NotFound{
        if(!this.entregadores.containsKey(id)) throw new NotFound();
        EntregadorPontos e =(EntregadorPontos) this.entregadores.get(id);
        return e.getPontos();
    }
    public String showCatalogo(String idLoja) throws NotFound{
        if(!this.lojas.containsKey(idLoja)) throw new NotFound();
        return this.lojas.get(idLoja).showCatalogo ();
    }
    public void adicionaProduto(String idLoja,String codProd,String descricao,double preco)throws NotFound,IdInvalido{
        if(!this.lojas.containsKey(idLoja)) throw new NotFound();
        if(!codProd.startsWith("p")) throw new IdInvalido();
        this.lojas.get(idLoja).adicionaProduto(codProd,descricao,preco);
    }
    public String showLojas(){
        return this.lojas.values().stream().map(n->n.toString()).reduce("",String::concat);
    }
    public void retirarPontos(String id,double x) throws NotFound,NotEntregadorPontos{
        if (!this.entregadores.containsKey(id)) throw new NotFound();
        Entregador ent=this.entregadores.get(id);
        if(!(ent instanceof EntregadorPontos)) throw new NotEntregadorPontos();
        EntregadorPontos e=(EntregadorPontos) ent;
        e.retiraPontos(x);
    }
    
    //Moderador Opçoes
    
    public boolean logInModerador (String pass){
        if (pass.equals("1234")) return true; else return false;
    }
    public void removeUtilizador(String idUtilizador)throws NotFound{
        if(!this.utilizadores.containsKey(idUtilizador)) throw new NotFound();
        this.utilizadores.remove(idUtilizador);
    }
    public void removeEntregador(String idVoluntario)throws NotFound{
        if(!this.entregadores.containsKey(idVoluntario)) throw new NotFound();
        this.entregadores.remove(idVoluntario);
    }
    public void removeLoja(String idLoja) throws NotFound{
        if(this.lojas.containsKey(idLoja)) throw new NotFound();
         this.lojas.remove(idLoja);
    }
    public void guarda(String ficheiro)throws FileNotFoundException,IOException{
        PrintWriter pr = new PrintWriter (new FileOutputStream(new File(ficheiro)));
        pr.print(this.toString());
        pr.close();
    }
    public void setPontosPorKmVoluntario(double x)throws ValorInvalido{
        if(x<0) throw new ValorInvalido();
        Voluntario.pontosPorKm=x;
    }
    public void atualizaFator(int metreologia,int transito,boolean feriado) throws ValorInvalido{
        this.condicoes.atualizaFator(metreologia,transito,feriado);
    }
    public void setTempoPorFila (double t)throws ValorInvalido{
        if (t<0) throw new ValorInvalido();
        Loja.setTempoEsperaFila(t);
    }
    public void setVelocidadeEntregador (double v)throws ValorInvalido{
        if (v<0) throw new ValorInvalido();
        Transportadora.setVelMedia(v);
    }
    public void apagar (String data){
        LocalDate dataDe = LocalDate.parse(data);
        this.entregas.values().stream().filter(n->n.getDataEntrega().isBefore(dataDe))
        .forEach(n->{this.entregas.remove(n.getIdEncomenda());
            this.encAceitas.remove(n.getIdEncomenda());
            this.entregas.remove(n.getIdEncomenda());});
    }
}
