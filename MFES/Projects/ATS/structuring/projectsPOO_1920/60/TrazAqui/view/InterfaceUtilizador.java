package view;

import interfaces.*;

import java.util.*;

/**
 * Classe que implementa o view do controller user
 */
public class InterfaceUtilizador {

	/**
	 * Imprime o menu do utilizador
	 */
    public static void menu() {
		System.out.println(".........................");
		System.out.println(".....MENU UTILIZADOR.....");
		System.out.println(".........................\n");
        System.out.println("1. Solicitar entrega de encomenda");
        System.out.println("2. Aceder à informação das entregas num dado período, por voluntário ou transportador");
        System.out.println("3. Classificar voluntário ou empresa");
        System.out.println("4. Voltar ao menu de início de sessão\n");
    }

	/**
	 * Imprime as lojas do sistema
	 * @param lojas lojas do sistema
	 */
    public static void listaLojas(Map<String, ILoja> lojas) {
    	System.out.println("Escolha que loja deseja visitar: ");

    	for(Map.Entry<String,ILoja > entry : lojas.entrySet())
    		System.out.println(entry.getValue());
    }

	/**
	 * Imprime os produtos de uma determinada loja
	 * @param lista lista de produtos da loja
	 */
    public static void produtosLoja(Map<String, IProduto> lista) {
    	System.out.println("Escolha que produto deseja comprar: ");
    	for(Map.Entry<String,IProduto> p : lista.entrySet()){
    		System.out.println(p.getKey() + " -- " + p.getValue().getProduto());
    	}
	}

	/**
	 * Imprime a lista dos voluntários do sistema
	 * @param voluntarios voluntários do sistema
	 */
	public static void listaVoluntarios(Map<String, IVoluntario> voluntarios) {
    	System.out.println("Voluntários do sistema:");

		for(Map.Entry<String,IVoluntario> entry : voluntarios.entrySet())
			System.out.println(entry.getKey() + " -- " + entry.getValue().getNome());
	}

	/**
	 * Imprime uma dada string
	 * @param s string
	 */
	public static void message(String s) { System.out.print(s); }

	/**
	 * Imprime as transportadoras do sistema
	 * @param empresas transportadoras do sistema
	 */
	public static void listaEmpresas(Map<String, IEmpresa> empresas) {
		System.out.println("Transportadoras do sistema:");

		for(Map.Entry<String,IEmpresa> entry : empresas.entrySet())
			System.out.println(entry.getKey() + " -- " + entry.getValue().getNome());
	}

	/**
	 * Imprime uma encomenda
	 * @param e encomenda
	 */
	public static void printEncomenda(IEncomenda e) { System.out.println(e); }

	/**
	 * Imprime que o voluntário dado vai entregar a encomenda
	 * @param v voluntário
	 */
	public static void voluntarioFound(String v) {
		System.out.println("A sua encomenda vai ser transportada pelo voluntário " + v);
		System.out.println("Obrigada por utilizar o sistema!");
	}

	/**
	 * Imprime que a empresa dado vai entregar a encomenda
	 * @param e empresa
	 */
	public static void empresaFound(String e) {
		System.out.println("A sua encomenda vai ser transportada pela empresa " + e);
		System.out.println("Obrigada por utilizar o sistema!");
	}
}
