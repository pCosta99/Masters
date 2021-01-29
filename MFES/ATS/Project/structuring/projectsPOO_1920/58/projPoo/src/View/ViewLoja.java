package View;

import Controller.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class ViewLoja {
    public void ViewLoja(){
    }

    public List<String> viewLoja(String l){
        List<String> ret = new ArrayList<>();

        System.out.println("Menu de Utlizador");
        System.out.println("Email:");
        Scanner n2 = new Scanner(System.in);
        String n1 = n2.nextLine();
        ret.add(n1);
        System.out.println("Password:");
        Scanner p = new Scanner(System.in);
        String p1 = p.nextLine();
        ret.add(p1);

        return ret;
    }

    public int menuL(){
        for (int i = 0; i < 50; ++i) System.out.println();
        System.out.println("Menu de Loja:\n\nAtualizar fila de espera(1)\nVer fila de espera(2)\nSair(0)");
        Scanner p1 = new Scanner(System.in);
        int p = p1.nextInt();
        return p;
    }
}
