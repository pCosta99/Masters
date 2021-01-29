package projeto.model;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Predicate;
import java.util.stream.Collectors;


/* classe destinada a gerir cada entidade, através de um Map, que guarda o código e a instância da classe em questão
 * 
 * classe também onde são criados métodos de geração automática de string, double, int e double
 * 
 */ 

public class Gestao implements Serializable {

	public Map<String, Entidade> utilizadores = new HashMap<String, Entidade>();
	public Map<String, Entidade> voluntarios = new HashMap<String, Entidade>();
	public Map<String, Entidade> empresas = new HashMap<String, Entidade>();
	public Map<String, Entidade> lojas = new HashMap<String, Entidade>();
	public Map<String, Encomenda> encomendas = new HashMap<String, Encomenda>();
	public Map<String, Encomenda> aceites = new HashMap<String, Encomenda>();
	public Map<String, EntidadeTransportadora> entregadores = new HashMap<String,EntidadeTransportadora>();	
	public Map<String, Entidade> entidades = new HashMap<String, Entidade>();
	
	
	
	/*
	public Map<String, Entidade> getUtilizadores() {
		return utilizadores;
	}




	public void setUtilizadores(Map<String, Entidade> utilizadores) {
		this.utilizadores = utilizadores;
	}




	public Map<String, Entidade> getVoluntarios() {
		return voluntarios;
	}




	public void setVoluntarios(Map<String, Entidade> voluntarios) {
		this.voluntarios = voluntarios;
	}




	public Map<String, Entidade> getEmpresas() {
		return empresas;
	}




	public void setEmpresas(Map<String, Entidade> empresas) {
		this.empresas = empresas;
	}




	public Map<String, Entidade> getLojas() {
		return lojas;
	}




	public void setLojas(Map<String, Entidade> lojas) {
		this.lojas = lojas;
	}




	public Map<String, Encomenda> getEncomendas() {
		return encomendas;
	}




	public void setEncomendas(Map<String, Encomenda> encomendas) {
		this.encomendas = encomendas;
	}




	public Map<String, Encomenda> getAceites() {
		return aceites;
	}




	public void setAceites(Map<String, Encomenda> aceites) {
		this.aceites = aceites;
	}




	public Map<String, EntidadeTransportadora> getEntregadores() {
		return entregadores;
	}




	public void setEntregadores(Map<String, EntidadeTransportadora> entregadores) {
		this.entregadores = entregadores;
	}*/




	public Gestao() {
		parse();
		/*
		this.utilizadores = new HashMap<>();
		this.voluntarios = new HashMap<>();
		this.empresas = new HashMap<>();
		this.lojas = new HashMap<>();
		this.encomendas = new HashMap<>();
		this.aceites = new HashMap<>();
		this.entregadores = new HashMap<>();
		*/
	}
	
	
	
	// COMPARADORES ---> SORT
	public Set<Utilizador> ordenaUtilizadores() {
	      TreeSet<Utilizador> res = new TreeSet<>(new ComparaEncomendasEfetuadasCresc());
	      for (Entidade a: this.utilizadores.values())
	         res.add((Utilizador) a.clone());
	      return res;   
	        
	 } 
	
	public Set<Empresa> ordenaEmpresa(){
		TreeSet<Empresa> res = new TreeSet<>(new ComparaKmsCresc());
		for(Entidade a: this.empresas.values()) {
			res.add((Empresa) a.clone());	
		}		
		return res;
	}
	/*
	public Set<Entidade> alunosOrdemDecrescenteNumero() { 
		return this.empresas.values().stream()
	                               .map(Entidade::clone)
	                               .collect(Collectors.toCollection(() -> new TreeSet<Entidade>(new ComparaKmsCresc())));
	 }*/
	
	
	
	
	 /**
     * Este método devolve a lista das entidades que satisfazem o predicado p
     */
	public List<Entidade> entidadeRespeita(Predicate<Entidade> p) {
	      return this.entidades.values().
	                  stream().
	                  filter(a -> p.test(a)).
	                  map(Entidade::clone).
	                  collect(Collectors.toList());
	 }
	
	
	
	
	
	
	
	
	
	
	
	/* métodos que guardam instâncias e ficheiro de objetos */
    public void guardaEstado(String nomeFicheiro) throws FileNotFoundException, IOException {
        FileOutputStream fos = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        
        oos.writeObject(this); 
        oos.flush();
        oos.close();
              
    }
    
    /*Método que recupera uma instância de turma gravada em ficheiro de objectos*/
    public static Gestao carregaEstado(String nomeFicheiro) throws FileNotFoundException, IOException, ClassNotFoundException {
    
		FileInputStream fis = new FileInputStream(nomeFicheiro);
		ObjectInputStream ois = new ObjectInputStream(fis);
		
		Gestao gestao = (Gestao) ois.readObject();
		ois.close();
		return gestao;
	
	}
	
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

 	public List<String> parse(){
         List<String> linhas = lerFicheiro("dados.csv"); 
         //System.out.println(linhas);
         String[] linhaPartida;
         for (String linha : linhas) {
                 linhaPartida = linha.split(":", 2);
                 switch(linhaPartida[0]){
                 case "Utilizador": 
                     Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                     //System.out.println(u.toString()); 
                     this.utilizadores.put(u.getCodigo(), u);
                     this.entidades.put(u.getCodigo(), u);
                     break;
                 case "Loja": 
                     Loja l = parseLoja(linhaPartida[1]);
                     //System.out.println(l.toString());
                     this.lojas.put(l.getCodigo(), l);
                     this.entidades.put(l.getCodigo(), l);
                     break;
                 case "Voluntario":
             		Voluntario v = parseVoluntario(linhaPartida[1]);
             		//System.out.println(v.toString());
             		this.voluntarios.put(v.getCodigo(), v);
             		this.entregadores.put(v.getCodigo(), v);
             		this.entidades.put(v.getCodigo(),v);
             		break;
                 case "Transportadora":
                 	Empresa e = parseEmpresa(linhaPartida[1]);
                 	//System.out.println(e.toString());
                 	this.empresas.put(e.getCodigo(), e);
                 	this.entregadores.put(e.getCodigo(), e);
                 	this.entidades.put(e.getCodigo(), e);
                 	break;
                 case "Encomenda":
                 	Encomenda enc = parseEncomenda(linhaPartida[1]);
                 	//System.out.println(enc.toString());
                 	this.encomendas.put(enc.getCodigo(), enc);
                 	break;
                 case "Aceite":
                 	EncomendasAceites ea = parseAceites(linhaPartida[1]);
                 	//System.out.println(ea.toString());
                 	this.aceites.put(ea.getCodigo(), ea);
                 	break;
                 default: 
                         System.out.println("-----");
                         //TODO : rever default
                         break;
                 }
                 
                 
         }
         System.out.println("--");
         return linhas;
         
   }
 	
 	
 	                                

 	  



 	public  Utilizador parseUtilizador(String input){
 		  String[] campos = input.split(",");
           String codUtilizador = campos[0]; 
           String nome = campos[1];
           double gpsx = Double.parseDouble(campos[2]);
           double gpsy = Double.parseDouble(campos[3]);
           return new Utilizador(codUtilizador, nome, gpsx,gpsy);
 	  }

 	  public  Loja parseLoja(String input){
 		  String[] campos = input.split(",");
 		  String codLoja = campos[0]; 
 		  String nomeLoja = campos[1];
 		  return new Loja(codLoja,nomeLoja);
 	  }
 	  
 	  public  Voluntario parseVoluntario(String input) {
 		  String[] campos = input.split(",");
 		  String codVoluntario = campos[0]; 
           String nome = campos[1];
           double gpsx = Double.parseDouble(campos[2]);
           double gpsy = Double.parseDouble(campos[3]);
           double raio = Double.parseDouble(campos[4]);
           //v9,Miriam Miranda Pinto,42.477493,50.403458,128.0
 		  return new Voluntario(codVoluntario, nome, gpsx, gpsy, raio);
 	  }
 	  
 	  public  Empresa parseEmpresa(String input) {
 		  String[] campos = input.split(",");
 		  String codEmpresa = campos[0]; 
           String nome = campos[1];
           double gpsx = Double.parseDouble(campos[2]);
           double gpsy = Double.parseDouble(campos[3]);
           double NIF = Double.parseDouble(campos[4]);
           double raio = Double.parseDouble(campos[5]);
           double precoKm = Double.parseDouble(campos[6]);
           return new Empresa(codEmpresa, NIF, nome, gpsx, gpsy, precoKm, raio);
           //t30,PORTIR TRANSITÁRIOS,86.89932,81.5264,156351440,80.0,1.5
 	  }
 	  
 	  public  Encomenda parseEncomenda(String input){
 		  String[] campos = input.split(",");
 		  int i = 4;
 		  ArrayList<LinhaEncomenda> le = new ArrayList<LinhaEncomenda>();
 		  String codEncomenda = campos[0]; 
 		  String codUtilizador = campos[1];
 		  //System.out.println(codUtilizador);
 		  String codLoja = campos[2];
 		  double peso = Double.parseDouble(campos[3]);
 		  while(i < campos.length) {
 			  String  codProduto= campos[i++];
 			  String desc = campos[i++];
 			  double qt = Double.parseDouble(campos[i++]);
 			  double unit = Double.parseDouble(campos[i++]);
 			  LinhaEncomenda l = new LinhaEncomenda(codProduto,desc,qt,unit);
 			  le.add(l);  
 		  }
 		  
 		  return new Encomenda(codEncomenda, codUtilizador, codLoja, peso, le);
 	  }
 	  
 	  
 	  
 	  public  EncomendasAceites parseAceites(String input){
 		  String[] campos = input.split(",");
 		  String codEncomenda = campos[0];
 		  return new EncomendasAceites(codEncomenda);
 	  }
 	  
 	  

 	  
 	  
 	  
 	  
 	  
 	  public  List<String> lerFicheiro(String nomeFich) {
 	        List<String> lines = new ArrayList<>();
 	        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
 	        catch(IOException exc) { System.out.println(exc.getMessage()); }
 	        return lines;
 	  }
}
