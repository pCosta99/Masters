package projeto.model;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;


public class Entidade implements Comparable<Entidade>, Serializable{
	
	
	
	// ATRIBUTOS COMUM À TODAS AS ENTIDADES
	private String codigo;
	private String email;
	private String password;
	private String nome;
	private double coordX;
	private double coordY;
	public Gestao g;
	//private ArrayList<String> historico;
	
	//private boolean acessoOk;
	
	
	
	// CONSTRUTORES
	public Entidade(String codigo, String email, String password, String nome, double coordX, double coordY) {
		this.codigo = codigo;
		this.email = email;
		this.password = password;
		this.nome = nome;
		this.coordX = coordX;
		this.coordY = coordY;
		//this.acessoOk = true;
	}

	public Entidade() {
		this.codigo = Gerador.stringGenerator();
		this.password = Gerador.stringGenerator();
		this.email = Gerador.stringGenerator();
		this.nome = "";
		this.coordX = Gerador.doubleCoordenadasGenerator();
		this.coordY = Gerador.doubleCoordenadasGenerator();
		//this.acessoOk = true;
	}
	
	public Entidade(Entidade g) {
		this.codigo = g.getCodigo();
		this.email = g.getEmail();
		this.password = g.getPassword();
		this.nome = g.getNome();
		this.coordX = g.getCoordX();
		this.coordY = g.getCoordY();
		//this.acessoOk = g.acessoOk;
	}
	
	public Entidade(String codigo, String nome, double coordX, double coordY) {
		this.codigo = codigo;
		this.nome = nome;
		this.coordX = coordX;
		this.coordY = coordY;
		//this.acessoOk = true;
		this.email =  Gerador.stringGenerator();
		
	}
	
	public Entidade(String codigo, String nome) {
		this.codigo = codigo;
		this.nome = nome;
		//this.acessoOk = true;
		this.password = Gerador.stringGenerator();
		this.email =  Gerador.stringGenerator();
		this.coordX = Gerador.doubleCoordenadasGenerator();
		this.coordY = Gerador.doubleCoordenadasGenerator();
		
		
	}
	
	public Entidade(String nome, String email, String pass) {
		this.nome = nome;
		this.email = email;
		this.password = pass;
		this.coordX = Gerador.doubleCoordenadasGenerator();
		this.coordY = Gerador.doubleCoordenadasGenerator();
		
	}
	
	
	//CLONE, TOSTRING E EQUALS
	public Entidade clone() {
		return new Entidade(this);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder("Entidade{");
		sb.append("nome = '").append(this.nome).append('\'');
		sb.append(", email ='").append(this.email).append('\'');
		sb.append(", codigo ='").append(this.codigo).append('\'');
		sb.append('}');
		return sb.toString();
	}

	public boolean equals(Object o) {
		if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Entidade u = (Entidade) o;
        Entidade em = (Entidade) o;
		return (this.codigo.equals(u.getCodigo()) && this.email.equals(em.getEmail()));
	}

	
	
	// GETTERS E SETTERS
	public String getCodigo() {
		return codigo;
	}

	public void setCodigo(String codigo) {
		this.codigo = codigo;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getNome() {
		return nome;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

	public double getCoordX() {
		return coordX;
	}

	public void setCoordX(double coordX) {
		this.coordX = coordX;
	}

	public double getCoordY() {
		return coordY;
	}

	public void setCoordY(double coordY) {
		this.coordY = coordY;
	}
		
	

	
	

	@Override
	public int compareTo(Entidade o) {
		int r = this.codigo.compareTo(o.getCodigo());
		r = r != 0 ? r: this.nome.compareTo(o.getNome());
		return r;
	}
	
	
	public String formatCSV(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getClass().getSimpleName() + ",");
        sb.append(this.codigo + ",");
        sb.append(this.nome);
        return sb.toString();
    }

    public void toCSV(String nfich) throws IOException{
        PrintWriter pw = new PrintWriter(nfich);
        pw.write(this.formatCSV() + "\n");
        pw.flush();
        pw.close();
    }




	
    
    
    
    
    
	
	
}
