package com.javamarlon;

/**
 * Classe que abstrai a utilização da classe Scanner, escondendo todos os
 * problemas relacionados com excepções, e que oferece métodos simples e
 * robustos para a leitura de valores de tipos simples.
 */

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.InputMismatchException;
import java.util.Scanner;

import static java.lang.System.out;

public class Input implements Serializable {

    /**
     * Métodos de Classe
     */

    private Scanner input;

    public Input() {
        this.input = new Scanner(System.in);
    }

    public String lerString() {
        boolean ok = false;
        String txt = "";
        while (!ok) {
            try {
                txt = input.next();
                ok = true;
            } catch (InputMismatchException e) {
                out.println("Texto Inválido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return txt;
    }

    public int lerInt() {
        while (true) {
            try {
                return input.nextInt();
            } catch (Exception e) {
                out.println("Inteiro Inválido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
    }

    public double lerDouble() {
        boolean ok = false;
        double d = 0.0;
        while (!ok) {
            try {
                d = input.nextDouble();
                ok = true;
            } catch (InputMismatchException e) {
                out.println("Valor real Inválido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return d;
    }

    public char lerEscolha() {
        char escolha = 'v';
        String resposta;
        boolean flag = false;

        do {
            resposta = lerString().toLowerCase();
            if (!resposta.isEmpty()) {
                escolha = resposta.charAt(0);
                switch (escolha) {
                    case 's':
                    case 'r':
                    case 'n':
                    case 'v':
                        flag = true;
                        break;
                    default:
                        System.out.println("Escolha inválida. Tente de novo.");
                }
            } else System.out.println("Escolha inválida. Tente de novo.");
        } while (!flag);

        return escolha;
    }

    public float lerFloat() {
        boolean ok = false;
        float f = 0.0f;
        while (!ok) {
            try {
                f = input.nextFloat();
                ok = true;
            } catch (InputMismatchException e) {
                out.println("Valor real Inválido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return f;
    }

    public boolean lerBoolean() {
        boolean ok = false;
        boolean b = false;
        while (!ok) {
            try {
                b = input.nextBoolean();
                ok = true;
            } catch (InputMismatchException e) {
                out.println("Booleano Inválido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return b;
    }

    public short lerShort() {
        boolean ok = false;
        short s = 0;
        while (!ok) {
            try {
                s = input.nextShort();
                ok = true;
            } catch (InputMismatchException e) {
                out.println("Short Inválido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return s;
    }

    public void pauseProg() {
        System.out.println("Carregue em ENTER para continuar\n");
        input.nextLine();
    }

    public Calendar lerData() {
        boolean flag = false;
        Calendar cal = Calendar.getInstance();

        do {
            try {
                System.out.println("Introduza a data da atividade(aaaa/mm/dd):");
                String data = lerString();
                System.out.println("Introduza a hora da atividade(hh:mm:ss) : ");
                String hora = lerString();
                SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
                cal.setTime(sdf.parse(data + " " + hora));
                flag = true;
            } catch (Exception e) {
                System.out.println("Formato de hora errada, tente outra vez.\n");
                flag = false;
            }
        } while (!flag);

        return cal;
    }
}