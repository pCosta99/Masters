package View;

import Controller.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class ViewTransportadora {
    public void ViewTransportadora(){
    }

    public List<String> viewTransp(String t){
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

    public int menuT(){
        for (int i = 0; i < 50; ++i) System.out.println();
        System.out.println("Menu de Tranportadora:\n\nTotal faturado(1)\nLista das 10 empresas que mais usam o sistema(2)\nVer o historico de encomendas(3)\nSair(0)");
        Scanner p1 = new Scanner(System.in);
        int p = p1.nextInt();
        return p;
    }
}

