/**
 * Classe que abstrai a utilização da classe Scanner, escondendo todos os
 * problemas relacionados com excepções, e que oferece métodos simples e
 * robustos para a leitura de valores de tipos simples e String.
 *
 * É uma classe de serviços, como Math e outras de Java, pelo que devem ser
 * usados os métodos de classe implementados.
 *
 * Utilizável em BlueJ, NetBeans, CodeBlocks ou Eclipse.
 *
 * Utilização típica:  int x = Input.lerInt();
 *                     String nome = Input.lerString();
 *
 * @author F. Mário Martins
 * @version 1.0 (6/2006)
 */
import static java.lang.System.out;
import static java.lang.System.in;

import java.io.Serializable;
import java.util.Scanner;
import java.util.InputMismatchException;

public class Input implements Serializable {

    /**
     * Função com o objetivo de ler um inteiro.
     * @param pergunta, Pergunta a efetuar antes de ler o inteiro.
     * @param erro, Mensagem a retornar caso ocorra erro.
     * @return Inteiro lido.
     */
    public static int lerInt(String pergunta, String erro) {
        Scanner input = new Scanner(in);
        boolean ok = false;
        int i = 0;
        while(!ok) {
            out.print(pergunta);
            try {
                i = input.nextInt();
                ok = true;
            }
            catch(InputMismatchException e) {
                out.println(erro);
                input.nextLine();
            }
        }
        //input.close();
        return i;
    }

    /**
     * Função com o objetivo de ler um inteiro num determinado intervalo.
     * @param pergunta, Pergunta a efetuar antes de ler o inteiro.
     * @param erro, Mensagem a retornar caso ocorra erro.
     * @param min, Limite inferior do intervalo.
     * @param max, Limite superior do intervalo.
     * @return Inteiro lido.
     */
    public static int lerInt(String pergunta, String erro, int min, int max) {
        Scanner input = new Scanner(in);
        boolean ok = false;
        int i = 0;
        while(!ok) {
            out.print(pergunta);
            try {
                i = input.nextInt();
                if(min <= i && i <= max) ok = true;
                else out.println(erro);
            }
            catch(InputMismatchException e) {
                out.println(erro);
                input.nextLine();
            }
        }
        //input.close();
        return i;
    }

    /**
     * Função com o objetivo de ler uma string.
     * @param pergunta, Pergunta a efetuar antes de ler a string.
     * @param erro, Mensagem a retornar caso ocorra erro.
     * @return String lida.
     */
    public static String lerString(String pergunta, String erro) {
        Scanner input = new Scanner(in);
        boolean ok = false;
        String txt = null;
        while(!ok) {
            out.print(pergunta);
            try {
                txt = input.nextLine();
                ok = true;
            }
            catch(InputMismatchException e) {
                out.println(erro);
                input.nextLine();
            }
        }
        //input.close();
        return txt;
    }

    /**
     * Função com o objetivo de ler um char.
     * @param pergunta, Pergunta a efetuar antes de ler o char.
     * @param erro, Mensagem a retornar caso ocorra erro.
     * @param accepted, Chars que podem ser aceites.
     * @return Char lido.
     */
    public static char lerChar(String pergunta, String erro, String accepted) {
        String accept = accepted.toLowerCase().concat(accepted.toUpperCase());
        Scanner input = new Scanner(in);
        boolean ok = false;
        char txt = '\0';
        while(!ok) {
            out.print(pergunta);
            try {
                txt = input.nextLine().charAt(0);
                if(accept.indexOf(txt) == -1)
                    out.println(erro);
                else
                    ok = true;
            }
            catch(InputMismatchException | IndexOutOfBoundsException e) {
                out.println(erro);
                input.nextLine();
            }
        }
        //input.close();
        return txt;
    }

    /**
     * Função com o objetivo de ler um double.
     * @param pergunta, Pergunta a efetuar antes de ler o double.
     * @param erro, Mensagem a retornar caso ocorra erro.
     * @return Double lido.
     */
    public static double lerDouble(String pergunta, String erro){
        Scanner input = new Scanner(in);
        boolean ok = false;
        double d = 0.0;
        while(!ok){
            out.print(pergunta);
            try {
                d = input.nextDouble();
                ok = true;
            }
            catch(InputMismatchException e){
                out.println(erro);
                input.nextLine();
            }
        }
        //input.close();
        return d;
    }

    /**
     * Função com o objetivo de ler uma Localização.
     * @return Localização lida.
     */
    public static Localizacao lerLocalizacao(){
       out.println("Localização: ");
       double latitude = lerDouble("\tLongitude: ","\tLongitude inválida!");
       double longitude = lerDouble("\tLatitude: ","\tLatitude inválida!");
       return new Localizacao(latitude,longitude);
    }
}