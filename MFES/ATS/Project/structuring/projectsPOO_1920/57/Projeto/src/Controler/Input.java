/**
 *  Classe que recebe e valida o imput recebido
 */
package Controler;

import Model.Coordenadas;
import Model.GestTrazAqui;
import View.Apresentacao;

import java.io.Serializable;
import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Scanner;

public class Input implements Serializable {

    /**
     *  Método que lê sim ou não
     *
     * @param a         Apresentação
     * @param message   String que representa a mensagem a apresentar
     * @return          true ou false dependendo do imput
     */
    public boolean lerSN(Apresentacao a, String message){
        Scanner s = new Scanner(System.in);
        String line;

        do{
            a.printMessage(message);
            line = s.nextLine();
        } while (!line.toUpperCase().equals("S") && !line.toUpperCase().equals("N"));

        return line.toUpperCase().equals("S");
    }

    /**
     *  Método que lê um double
     *
     * @param a         Apresentação
     * @param message   String que representa a mensagem a apresentar
     * @param min       Valor máximo do Double
     * @param max       Valor mínimo do Double
     * @return          Double lido
     */
    public double lerDouble(Apresentacao a, String message,int min,int max){
        Scanner s = new Scanner(System.in);
        double n = -1;

        do{
            a.printMessage(message);
            try {
                String line = s.nextLine();
                n = Double.parseDouble(line);
            } catch (NumberFormatException nfe) {
                a.printMessage(nfe.getMessage());
                n = -1;
            }
        } while (n < min || n > max);

        return n;
    }

    /**
     *  Método que lê uma data
     *
     * @param a         Apresentação
     * @param message   String que representa a mensagem a apresentar
     * @return          Data lida
     */
    public LocalDateTime lerData(Apresentacao a, String message){
        Scanner s = new Scanner(System.in);
        boolean val = true;
        LocalDateTime data = null;
        String[] date;

        do{
            a.printMessage(message);
            try {
                date = s.nextLine().split("-",3);
                data = LocalDateTime.of(Integer.parseInt(date[2]),Integer.parseInt(date[1]),Integer.parseInt(date[0]),0,0);
                val = false;
            } catch (DateTimeException dte ) {
                a.printMessage("Data inválida");
            }catch (NumberFormatException ignored){
            }
        } while (val);

        return data;
    }

    /**
     *  Método que lê um codigo de loja
     *
     * @param a         Apresentação
     * @param message   String que representa a mensagem a apresentar
     * @param c         GestTrazAqui c
     * @return          String que representa um code
     */
    public String lerStringLoja(Apresentacao a, String message, GestTrazAqui c) {
        Scanner s = new Scanner(System.in);
        String line;

        do{
            a.printMessage(message);
            line = s.nextLine();
        } while (!c.containsLoja(line));

        return line;
    }

    /**
     *  Método que lê um código de encomenda
     *
     * @param a         Apresentação
     * @param message   String que representa a mensagem a apresentar
     * @param list      Lista de codigos de encomenda
     * @return          Codigo encomenda
     */
    public String lerStringSolicitarEnc(Apresentacao a, String message, List<String> list) {
        Scanner s = new Scanner(System.in);
        String line;

        if(list.size() == 0)
            a.printErroSemEncomenda();

        do{

            a.printMessage(message);
            line = s.nextLine();
        } while (!list.contains(line));

        return line;
    }

    /**
     *  Método que lê uma coordenada
     *
     * @param a Apresentação
     * @return  Coordenada lida
     */
    public Coordenadas lerCoordenada(Apresentacao a){
        Scanner s = new Scanner(System.in);
        String[] line;
        double lat,lon = 0;

        do{
            a.printMessage("Introduza a latitude ([-90,90]) e a longitude ([-180,180]), por exemplo: 30 20");
            line = s.nextLine().split(" ",2);
            try{
                lat = Double.parseDouble(line[0]);
                if(line.length == 2)
                    lon = Double.parseDouble(line[1]);
            } catch (NumberFormatException nfe) {
                a.printMessage(nfe.getMessage());
                lat = 100;
            }
        } while (line.length != 2 || lat < -90 || lat > 90 || lon < -180 || lon > 180);

        return new Coordenadas(lat,lon);
    }

    /**
     *  Método que lê um código de encomenda entrego por um estafeta
     *
     * @param a         Apresentação
     * @param message   String que representa a mensagem a apresentar
     * @param c         GestTrazAqui
     * @param code      Codigo estafeta
     * @return          Codigo encomenda
     */
    public String lerStringEncomenda(Apresentacao a, String message, GestTrazAqui c, String code) {
        Scanner s = new Scanner(System.in);
        String line;

        do{
            a.printMessage(message);
            line = s.nextLine();
        } while (!c.containsEncomendaEstafeta(line, code));

        return line;
    }

    /**
     * Método que valida uma linha de encomenda
     *
     * @param line      Array com os valores de uma linha de encomenda
     * @param storeCode Código loja
     * @param c         GestTrazAqui
     * @return          true caso seja valida false caso inválida
     */
    private boolean linhaEncomendaValida(String[] line, String storeCode, GestTrazAqui c) {
        String[] tmp;

        for (String s: line) {
            tmp = s.split(" ");
            if (!c.containsProdutoLoja(storeCode, tmp[0]))
                return false;
        }
        return true;
    }

    /**
     *  Método que lê uma linha de encomenda
     *
     * @param a         Apresentação
     * @param message   String que representa a mensagem a apresentar
     * @param c         GestTrazAqui
     * @param storeCode Codigo loja
     * @return          Valores de uma linha de encomenda
     */
    public String[] lerLinhaEncomenda(Apresentacao a, String message, GestTrazAqui c, String storeCode){
        Scanner s = new Scanner(System.in);
        String line;
        String[] linhaPartida;

        do{
            a.printMessage(message);
            line = s.nextLine();
            linhaPartida = line.split(" \\| ");
        } while (!linhaEncomendaValida(linhaPartida, storeCode, c));

        return linhaPartida;
    }
}
