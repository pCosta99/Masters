package View;


import java.io.Serializable;
import java.util.List;
/**
 * Classe Pagina
 *
 * @author Grupo 4
 * @version 2020
 */

public class Pagina implements Serializable {

    int numlinhas;
    int numColunas;
    private List<String> lista;

    public Pagina(int numlinhas, int numColunas, List<String> lista) {
        this.numlinhas = numlinhas;
        this.numColunas = numColunas;
        this.lista = lista;
    }

    public void showPage() {
        int i = 0;

        while (i >= 0 && i < this.lista.size()) {
            System.out.println("\nNumero elementos:" + lista.size());
            for (int l = 0; l < this.numlinhas; l++) {
                for (int c = 0; c < this.numColunas; c++) {
                    if (i >= lista.size()) break;
                    System.out.printf("%s ", lista.get(i++));
                }
                System.out.println();
            }
        }
    }
}