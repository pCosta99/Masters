package View;

import Controller.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class ViewUtilizador {
    public void ViewUtilizador(){
    }

    public List<String> viewUtil(String u){
        List<String> ret = new ArrayList<>();

        System.out.println("Menu de Utlizador");
        System.out.println("Email:");
        Scanner n2 = new Scanner(System.in);
        String m = n2.nextLine();
        ret.add(m);
        System.out.println("Password:");
        Scanner p1 = new Scanner(System.in);
        String p = p1.nextLine();
        ret.add(p);

        return ret;
    }

    public int menuU(){
        for (int i = 0; i < 50; ++i) System.out.println();
        System.out.println("Menu de Utlizador:\n\nFazer encomenda(1)\nClassificar entrega(2)\nVer historio de encomendas(3)\nSair(0)");
        Scanner p1 = new Scanner(System.in);
        int p = p1.nextInt();
        return p;
    }
}
