package model;

import interfaces.ILogin;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe que implementa as informações de login
 */
public class Login implements ILogin, Serializable {
	private Map<String,String> users;
	private Map<String,String> volunts;
	private Map<String,String> empresas;
	private Map<String,String> lojas;

	/**
	 * Construtor vazio do login
	 */
	public Login(){
		this.users = new HashMap<>();
		this.volunts = new HashMap<>();
		this.empresas = new HashMap<>();
		this.lojas = new HashMap<>();
	}

	/**
	 * Construtor do login
	 * @param empresas empresas
	 * @param volunts voluntários
	 * @param lojas lojas
	 * @param users users
	 */
	public Login(Map<String, String> users, Map<String, String> volunts,
					Map<String, String> empresas, Map<String, String> lojas){
		setUsers(users);
		setVolunts(volunts);
		setEmpresas(empresas);
		setLojas(lojas);
	}

	/**
	 * Construtor do login
	 * @param login login
	 */
	public Login(Login login) {
		setUsers(login.getUtilizadores());
		setVolunts(login.getVoluntarios());
		setEmpresas(login.getEmpresas());
		setLojas(login.getLojas());
	}

	/**
	 * Devolve os utilizadores
	 * @return utilizadores
	 */
	public Map<String,String> getUtilizadores() {
		Map<String,String> aux = new HashMap<>();
		this.users.forEach(aux::put);
		return aux;
	}

	/**
	 * Devolve os voluntários
	 * @return nif
	 */
	public Map<String,String> getVoluntarios() {
		Map<String,String> aux = new HashMap<>();
		this.volunts.forEach(aux::put);
		return aux;
	}

	/**
	 * Devolve as empresas
	 * @return empresas
	 */
	public Map<String,String> getEmpresas() {
		Map<String,String> aux = new HashMap<>();
		this.empresas.forEach(aux::put);
		return aux;
	}

	/**
	 * Devolve as lojas
	 * @return lojas
	 */
	public Map<String,String> getLojas() {
		Map<String,String> aux = new HashMap<>();
		this.lojas.forEach(aux::put);
		return aux;
	}

	/**
	 * Substitui os utilizadores
	 * @param lista utilizadores
	 */
	public void setUsers(Map<String,String> lista){
		this.users = new HashMap<>();

		lista.forEach((key, value) -> this.users.put(key, value));

	}

	/**
	 * Substitui os voluntários
	 * @param lista voluntários
	 */
	public void setVolunts(Map<String,String> lista){
		this.volunts = new HashMap<>();

		lista.forEach((key, value) -> this.volunts.put(key, value));

	}

	/**
	 * Substitui as empresas
	 * @param lista empresas
	 */
	public void setEmpresas(Map<String,String> lista){
		this.empresas = new HashMap<>();

		lista.forEach((key, value) -> this.empresas.put(key, value));

	}

	/**
	 * Substitui as lojas
	 * @param lista lojas
	 */
	public void setLojas(Map<String,String> lista){
		this.lojas = new HashMap<>();

		lista.forEach((key, value) -> this.lojas.put(key, value));

	}

	/**
	 * vê se o login de utilizador é válido
	 * @param user user
	 * @param password password
	 * @return se o login do utilizador é válido
	 */
	public boolean loginUtilizador(String user, String password){
		if (this.users.containsKey(user))
			return this.users.get(user).equals(password);
		else return false;
	}

	/**
	 * vê se o login do voluntário é válido
	 * @param volunts voluntário
	 * @param password password
	 * @return se o login do voluntário é válido
	 */
	public boolean loginVoluntario(String volunts, String password){
		if (this.volunts.containsKey(volunts))
			return this.volunts.get(volunts).equals(password);
		else return false;
	}

	/**
	 * vê se o login da empresa é válido
	 * @param empresas empresa
	 * @param password password
	 * @return se o login da empresa é válido
	 */
	public boolean loginEmpresa(String empresas, String password){
		if (this.empresas.containsKey(empresas))
			return this.empresas.get(empresas).equals(password);
		else return false;
	}

	/**
	 * vê se o login da loja é válido
	 * @param lojas loja
	 * @param password password
	 * @return se o login da loja é válido
	 */
	public boolean loginLoja(String lojas, String password){
		if (this.lojas.containsKey(lojas))
			return this.lojas.get(lojas).equals(password);
		else return false;
	}

	/**
	 * Adiciona um utilizador ao login
	 * @param user user
	 * @param password password
	 * @return se o user e a password são válidos
	 */
	public boolean addUtilizador(String user, String password) {
		if (!this.users.containsKey(user))
			this.users.put(user,password);
		else return false;
		return true;
	}

	/**
	 * Adiciona um voluntário ao login
	 * @param user user
	 * @param password password
	 * @return se o user e a password são válidos
	 */
	public boolean addVoluntario(String user, String password) {

		if (!this.volunts.containsKey(user))
			this.volunts.put(user, password);
		else return false;
		return true;
	}

	/**
	 * Adiciona uma empresa ao login
	 * @param user user
	 * @param password password
	 * @return se o user e a password são válidos
	 */
	public boolean addEmpresa(String user, String password) {
		if (!this.empresas.containsKey(user))
			this.empresas.put(user,password);
		else return false;
		return true;
	}

	/**
	 * Adiciona uma loja ao login
	 * @param user user
	 * @param password password
	 * @return se o user e a password são válidos
	 */
	public boolean addLoja(String user, String password) {
		if (!this.lojas.containsKey(user))
			this.lojas.put(user,password);
		else return false;
		return true;
	}

	/**
	 * @return clone de um login
	 */
	public Login clone() {
		return new Login(this);
	}

	/**
	 * Grava dados para um ficheiro
	 */
	public void gravarDados() throws IOException {
		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream("./dados/login.obj"));

		o.writeObject(this);
		o.flush();
		o.close();
	}

	/**
	 * Lê dados de um ficheiro
	 * @return login
	 */
	public Login lerDados() throws IOException, ClassNotFoundException {
		ObjectInputStream o = new ObjectInputStream(new FileInputStream("./dados/login.obj"));
		Login t = (Login) o.readObject();
		o.close();

		return t;
	}
}
