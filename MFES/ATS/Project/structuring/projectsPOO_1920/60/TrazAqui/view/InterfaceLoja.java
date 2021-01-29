package view;

import interfaces.IEmpresa;
import interfaces.IEncomenda;
import interfaces.IVoluntario;

import java.util.List;
import java.util.Map;

/**
 * Classe que implementa o view do controller loja
 */
public class InterfaceLoja {

    /**
     * Imprime o menu da loja
     */
    public static void menu() {
        System.out.println(".........................");
        System.out.println("........MENU LOJA........");
        System.out.println(".........................\n");
        System.out.println("1. Sinalizar encomenda a ser entregue");
        System.out.println("2. Indicar quantidade de pessoas em fila de espera");
        System.out.println("3. Aceder à informação das entregas num dado período, por voluntário ou transportador");
        System.out.println("4. Voltar ao menu de login");
    }

    /**
     * Imprime a lista de encomendas feitas para a loja
     * @param encs encomendas
     * @param encomendasAceites encomendas aceites
     * @return número de encomendas na loja
     */
    public static int listaEncs(String loja, Map<String, IEncomenda> encs, List<String> encomendasAceites){
        System.out.println("Escolha que encomenda está pronta: ");
        int i = 0;
        for(Map.Entry<String,IEncomenda> entry : encs.entrySet())
            if(entry.getValue().getVendedor().equals(loja) && !encomendasAceites.contains(entry.getKey())) {
                System.out.println(entry.getKey());
                i++;
            }
        return i;
    }

    /**
     * Imprime uma dada string
     * @param s string
     */
    public static void message(String s) { System.out.print(s);}

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
