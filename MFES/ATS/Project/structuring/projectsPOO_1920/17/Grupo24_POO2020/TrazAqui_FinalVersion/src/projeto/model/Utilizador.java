package projeto.model;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;


public class Utilizador extends Entidade implements Serializable {
	
	private int numEncomendasEfetuadas;
	private Gestao g;
	private ArrayList<String> entregasEfetuadas;
	//Constructors
	public Utilizador() {
		super();
		this.numEncomendasEfetuadas = 0;

	}
	
	public Utilizador(Utilizador u) {
		super(u);
		this.numEncomendasEfetuadas = u.numEncomendasEfetuadas;
	}
	
	public Utilizador(Entidade u) {
		super(u);
	}
	
	public Utilizador(String codigo, String email, String password, String nome, double coordX, double coordY) {
		super(codigo, email, password, nome, coordX, coordY);
		this.numEncomendasEfetuadas = 0;
		//this.entregasEfetuadas = null;
	}
	
	public Utilizador(String codigo, String nome, double coordX, double coordY) {
		super(codigo, nome, coordX, coordY);
		this.numEncomendasEfetuadas = 0;
		this.setPassword(Gerador.stringGenerator());
		this.setEmail(Gerador.stringGenerator());
		ArrayList<String> nova = new ArrayList<String>();
		nova.add("->");
		this.setEntregasEfetuadas(nova);
	}
	
	public Utilizador(String nome, String email, String pass) {
		super(nome, email, pass);
		this.setCodigo(Gerador.stringGenerator());
		ArrayList<String> nova = new ArrayList<String>();
		nova.add("->");
		this.setEntregasEfetuadas(nova);
		this.setCoordX(Gerador.doubleGenerator());
		this.setCoordY(Gerador.doubleGenerator());
	}
	
	public Utilizador(String cod, String nome) {
		super(cod, nome);
	}
	
	
	
	//CLONE, TOSTRING E EQUALS
	public boolean equals(Object o) {
		if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador u = (Utilizador) o;
        Utilizador em = (Utilizador) o;
		return (this.getCodigo().equals(u.getCodigo()) && this.getEmail().equals(em.getEmail()));
	}
	
	 public Utilizador clone(){
		 return new Utilizador(this);
	 }
	 
	 
	 public String toString() {
		StringBuilder sb = new StringBuilder("");
		sb.append(" Nome ='").append(this.getNome()).append('\'');
		sb.append(", Email do utilizador = '").append(this.getEmail()).append('\'');
		sb.append(" Codigo do utilizador = '").append(this.getCodigo()).append('\'');
		sb.append(" ");
		return sb.toString();
	 }
	 
	 
	 public String toStringv2() {

        StringBuffer buffer = new StringBuffer();
        buffer.append(this.getNome());
        buffer.append("\n");
        buffer.append(this.getEmail());
        buffer.append("\n");
        buffer.append(this.getCodigo());
        buffer.append("\n");

        return buffer.toString();
    }
	 
	 
	 
	// GETTERS E SETTERS
	public int getNumEncomendasEfetuadas() {
		return numEncomendasEfetuadas;
	}

	public ArrayList<String> getEntregasEfetuadas() {
		return entregasEfetuadas;
	}

	public void setEntregasEfetuadas(ArrayList<String> entregasEfetuadas) {
		this.entregasEfetuadas = entregasEfetuadas;
	}

	public void setNumEncomendasEfetuadas(int numEncomendasEfetuadas) {
		this.numEncomendasEfetuadas = numEncomendasEfetuadas;
	}
	
	
	
	
	 
	 // OUTROS MÉTODOS
	
	 public ArrayList<String> verHistoricoEntregar(){
		 return this.getEntregasEfetuadas();
	 }
	 
	 public void classificar(double i, EntidadeTransportadora e) {
		 int q = e.getQtClassificacoes();
		 double x = (i + e.getClassificacao()) / 2;
		 e.setQtClassificacoes(q + 1);
		 e.setClassificacao(x);
	 }
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
 
	 /*public int validarCredenciais() {
		 if(acessoOk == true) {
			 return 1;
		 }
		 
		 else {
			 return 0;
		 }
	 }*/
	 
	 
	 
	 
	 
	 
	 
	 
	 /*
	 
	 public void guardaEstado(String nomeFicheiro) throws FileNotFoundException, IOException {
	      FileOutputStream fos = new FileOutputStream(nomeFicheiro);
	      ObjectOutputStream oos = new ObjectOutputStream(fos);
	      
	      oos.writeObject(this); //guarda __esta__ instância de uma só vez!
	      oos.flush();
	      oos.close();
	            
	  }
	 
	    public static Utilizador carregaEstado(String nomeFicheiro) throws FileNotFoundException, IOException, ClassNotFoundException {
        
		FileInputStream fis = new FileInputStream(nomeFicheiro);
		ObjectInputStream ois = new ObjectInputStream(fis);
		
		Utilizador novoUtilizador = (Utilizador) ois.readObject();
		ois.close();
		return novoUtilizador;
		
		}
	    
	    public void carregaEstadov1(String nomeFicheiro) throws FileNotFoundException, IOException, ClassNotFoundException {
        
			FileInputStream fis = new FileInputStream(nomeFicheiro);
			ObjectInputStream ois = new ObjectInputStream(fis);
			
			Utilizador novoUtilizador = (Utilizador) ois.readObject();
			ois.close();
			
			//faz overwrite à informação das variáveis de instância
			String nome = novoUtilizador.getNome();
			this.setNome(nome);
			String cod = novoUtilizador.getCodigo();
			this.setCodigo(cod);
		}
	 
	 
	 
	 
	 
	*/
	 
	
	
	


}
