package com.javamarlon;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Menu implements Serializable {

    private List<String> opcoes;
    private int op;

    public Menu(String[] opcoes) {
        this.opcoes = new ArrayList<String>();
        this.opcoes.addAll(Arrays.asList(opcoes));
        this.op = 0;
    }


    public void showMenu() {
        for (String opcao : this.opcoes) System.out.println(opcao);
    }
}