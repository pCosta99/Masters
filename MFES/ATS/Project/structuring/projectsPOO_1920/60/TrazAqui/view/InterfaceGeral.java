package view;

import interfaces.IEmpresa;
import interfaces.ILoja;
import interfaces.IUser;
import interfaces.IVoluntario;

import java.util.AbstractMap.SimpleEntry;
import java.util.List;
import java.util.Map;

/**
 * Classe que implementa o view geral
 */
public class InterfaceGeral {

    /**
     * Imprime o menu principal
     */
    public static void menu() {
        System.out.println("...................");
        System.out.println(".....TRAZ AQUI.....");
        System.out.println("...................\n");
        System.out.println("1. Sou utilizador");
        System.out.println("2. Sou voluntário");
        System.out.println("3. Sou empresa transportadora");
        System.out.println("4. Sou loja");
        System.out.println("5. Registar novo utilizador");
        System.out.println("6. Registar novo voluntário");
        System.out.println("7. Registar nova empresa");
        System.out.println("8. Registar nova loja");
        System.out.println("9. Exibir voluntários da plataforma");
        System.out.println("10.Exibir empresas transportadoras da plataforma");
        System.out.println("11.Exibir lojas da plataforma");
        System.out.println("12.Exibir utilizadores da plataforma");
        System.out.println("13.Exibir a faturação de uma empresa num período de tempo");
        System.out.println("14.Exibir os 10 utilizadores que mais utilizam o sistema");
        System.out.println("15.Exibir as 10 transportadoras que mais utilizam o sistema");
        System.out.println("16.Ler os dados de um ficheiro");
        System.out.println("17.Gravar os dados em ficheiro");
        System.out.println("0. Sair");
    }

    /**
     * Imprime uma dada string
     * @param s string
     */
    public static void message(String s) { System.out.print(s); }

    /**
     * Imprime os voluntários do sistema
     * @param list voluntários
     */
    public static void showVoluntarios(Map<String, IVoluntario> list) {
        list.forEach((key,value) -> System.out.println(key + " -- " + value));
    }

    /**
     * Imprime as empresas do sistema
     * @param list empresas
     */
    public static void showEmpresas(Map<String, IEmpresa> list) {
        list.forEach((key,value) -> System.out.println(key + " -- " + value));
    }

    /**
     * Imprime as lojas do sistema
     * @param list lojas
     */
    public static void showLojas(Map<String, ILoja> list) {
        list.forEach((key,value) -> System.out.println(key + " -- " + value));
    }

    /**
     * Imprime os utilizadores do sistema
     * @param list utilizadores
     */
    public static void showUsers(Map<String, IUser> list) {
        list.forEach((key,value) -> System.out.println(key + " -- " + value));
    }

    /**
     * Imprime mensagem para inserir data
     * @param b boolean
     */
    public static void date(boolean b){
        if (b) System.out.println("Insira a data de ínicio: ");
        else System.out.println("Insira a data do fim: ");
    }

    /**
     * Imprime a faturação
     * @param fat faturação
     */
    public static void printFat(double fat){
        System.out.println(fat);
    }

    /**
     * Imprime os 10 utilizadores que mais utilizam o sistema
     * @param lista utilizadores
     */
    public static void top10Users(List<SimpleEntry<IUser, Integer>> lista){
        int i = 1;
        for(SimpleEntry<IUser,Integer> s : lista){
            System.out.print(i + "º - ");
            System.out.println("User: " + s.getKey() + " que realizou " + s.getValue() + " encomendas\n");
            i++;
        }
    }

    /**
     * Imprime as 10 empresas que mais utilizam o sistema
     * @param lista empresas
     */
    public static void top10Empresas(List<SimpleEntry<IEmpresa, Double>> lista){
        int i = 1;
        for(SimpleEntry<IEmpresa,Double> s : lista){
            System.out.print(i + "º - ");
            System.out.println("Empresa: " + s.getKey() + " que fez " + s.getValue() + " Kms\n");
            i++;
        }
    }

}
