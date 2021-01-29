package view;

import interfaces.IEmpresa;
import interfaces.IEncomenda;
import interfaces.IVoluntario;

import java.util.Map;
import java.util.Set;

/**
 * Classe que implementa o view do controller voluntário
 */
public class InterfaceVoluntario {

    /**
     * Imprime o menu do voluntário
     */
    public static void menu() {
        System.out.println(".........................");
        System.out.println(".....MENU VOLUNTÁRIO.....");
        System.out.println(".........................\n");
        System.out.println("1. Aceitar entregar encomendas");
        System.out.println("2. Escolher encomenda a ir buscar");
        System.out.println("3. Validar transporte de encomenda e registar duração");
        System.out.println("4. Aceder à informação das entregas num dado período, por voluntário ou transportador");
        System.out.println("5. Alterar o estado de transporte de medicamentos.");
        System.out.println("6. Voltar ao menu de início de sessão\n");
    }

    /**
     * Imprime se o voluntário está disponível para fazer entregas ou não
     * @param v voluntário
     */
    public static void printLivre(IVoluntario v){
        if(v.isLivre()) System.out.println("Agora está disponível para fazer entregas.");
        else System.out.println("Já não se encontra disponível para fazer entregas");
    }

    /**
     * Imprime uma dada string
     * @param s string
     */
    public static void message(String s) {
        System.out.print(s);
    }

    /**
     * Imprime as encomendas disponíveis
     * @param s encomendas
     */
    public static void x(Set<IEncomenda> s) {
        System.out.println("As encomendas disponíveis:");
        System.out.println(s);
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
}
