package Visualizador;

import java.io.Serializable;

public class Visualizador implements IVisualizador, Serializable {
    public static void printString(Object a) {
        System.out.println(a);
    }
}
