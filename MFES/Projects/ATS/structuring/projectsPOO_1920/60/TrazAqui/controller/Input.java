package controller;

import view.InterfaceGeral;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * Classe que implementa os inputs do programa
 */
public class Input {
    /**
     * Lê uma string do input
     * @return string
     */
    public static String lerString() {
        Scanner input = new Scanner(System.in);
        boolean valido = false;
        String s = "";
        while(!valido) {
            try {
                s = input.nextLine();
                valido = true;
            } catch (InputMismatchException e) {
                InterfaceGeral.message("Inválido\n");
                InterfaceGeral.message(">> ");
                input.next();
            }
        }
        return s;
    }

    /**
     * Lê um int do input
     * @return int
     */
    public static int lerInt() {
        Scanner input = new Scanner(System.in);
        boolean valido = false;
        int s = 0;
        while(!valido) {
            try {
                s = input.nextInt();
                valido = true;
            } catch (InputMismatchException e) {
                InterfaceGeral.message("Inválido\n");
                InterfaceGeral.message(">> ");
                input.next();
            }
        }
        return s;
    }

    /**
     * Lê um double do input
     * @return double
     */
    public static double lerDouble() {
        Scanner input = new Scanner(System.in);
        boolean valido = false;
        double s = 0.0;
        while(!valido) {
            try {
                s = input.nextDouble();
                valido = true;
            } catch (InputMismatchException e) {
                InterfaceGeral.message("Inválido\n");
                InterfaceGeral.message(">> ");
                input.next();
            }
        }
        return s;
    }

    /**
     * Lê um boolean do input
     * @return boolean
     */
    public static boolean lerBoolean() {
        Scanner input = new Scanner(System.in);
        boolean valido = false;
        boolean s = false;
        while(!valido) {
            try {
                s = input.nextBoolean();
                valido = true;
            } catch (InputMismatchException e) {
                InterfaceGeral.message("Inválido\n");
                InterfaceGeral.message(">> ");
                input.next();
            }
        }
        return s;
    }
}
