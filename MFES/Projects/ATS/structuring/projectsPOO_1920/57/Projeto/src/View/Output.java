package View;

import java.io.Serializable;
import java.util.List;

public class Output implements Serializable {
    /**
     * Apresenta screen clear
     */
    public void clear() {
        for(int i = 0; i<5; i++)
            System.out.println("");
    }

    /**
     * Apresenta print line
     * @param size tamanho
     */
    private void printLine(int size) {
        for(int i=0; i<size; i++)
            System.out.print("-");

        System.out.println("");
    }

    /**
     * Apresenta Menu
     * @param menu      menu
     * @param message   mensagem
     * @param type      type
     */
    public void printMenus(String []menu, String message, int type){

        int size, length=message.length();

        for(String linha: menu)
            if(linha.length() + 4 > length)
                length = linha.length() + 4;

        if(length < 20)
            length = 20;

        printLine(length);
        System.out.println(message);
        printLine(length);

        size = menu.length;
        for(int i = 0;i < size;i++)
            System.out.println(i+1+" | "+menu[i]);
        if(type == 0)
            System.out.println("0 | Sair");
        else
            System.out.println("0 | Voltar atrás");
        printLine(length);
    }

    /**
     * Apresenta mensagem e arraylist
     * @param message mensagem
     * @param arr     list de strings
     */
    public void printArray(String message, List<String> arr) {
        System.out.println("\n" + message);

        for(String line : arr)
            System.out.println(line);

        System.out.print("\n");
    }

    /**
     * Apresenta tabela com mensagem e arraylist
     * @param message mensagem
     * @param arr     list de strings
     */
    public void printTable(String message, List<String> arr) {
        System.out.println("\n" + message);

        for(int i=0; i<arr.size(); i++)
            System.out.println(String.format("%2d %1s",(i+1), ") ") + arr.get(i));

        System.out.print("\n");
    }

    /**
     * Apresenta mensagem
     * @param message mensagem
     */
    public void printMessage(String message) {
        System.out.println(message);
    }
}
