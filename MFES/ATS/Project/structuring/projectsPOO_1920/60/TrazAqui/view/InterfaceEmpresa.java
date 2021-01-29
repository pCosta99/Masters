package view;

import interfaces.IEmpresa;
import interfaces.IEncomenda;
import interfaces.IVoluntario;

import java.util.List;
import java.util.Map;

/**
 * Classe que implementa o view para o controller das empresas
 */
public class InterfaceEmpresa {

    /**
     * Imprime o menu empresa
     */
    public static void menu() {
        System.out.println(".........................");
        System.out.println(".......MENU EMPRESA......");
        System.out.println(".........................\n");
        System.out.println("1. Aceitar entregar encomendas");
        System.out.println("2. Determinar custo do transporte de encomenda");
        System.out.println("3. Validar transporte de encomenda e registar duração e custo");
        System.out.println("4. Aceder à informação das entregas num dado período, por voluntário ou transportador");
        System.out.println("5. Alterar o estado de transporte de medicamentos.");
        System.out.println("6. Voltar ao menu de início de sessão\n");
    }

    /**
     * Imprime se a empresa está disponível para fazer entregas ou não
     * @param e empresa
     */
    public static void printLivre(IEmpresa e){
        if(e.isLivre()) System.out.println("Agora está disponível para fazer entregas!\n");
        else System.out.println("Já não se encontra disponível para fazer entregas...\n");
    }

    /**
     * Imprime uma dada string
     * @param s string
     */
    public static void message(String s) {
        System.out.print(s);
    }

    /**
     * Imprime a lista dos voluntários do sistema
     * @param voluntarios voluntários do sistema
     */
    public static void listaVoluntarios(Map<String, IVoluntario> voluntarios) {
        System.out.println("Voluntários do sistema:");

        for(Map.Entry<String, IVoluntario> entry : voluntarios.entrySet())
            System.out.println(entry.getKey() + " -- " + entry.getValue().getNome());
    }

    /**
     * Imprime as transportadoras do sistema
     * @param empresas transportadoras do sistema
     */
    public static void listaEmpresas(Map<String, IEmpresa> empresas) {
        System.out.println("Transportadoras do sistema:");

        for(Map.Entry<String, IEmpresa> entry : empresas.entrySet())
            System.out.println(entry.getKey() + " -- " + entry.getValue().getNome());
    }

    /**
     * Imprime uma encomenda
     * @param e encomenda
     */
    public static void printEncomenda(IEncomenda e) { System.out.println(e); }
}
