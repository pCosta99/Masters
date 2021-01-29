package view;

import java.util.ArrayList;
import java.util.List;

public class EscolheProduto {
    private final String produto;
    private final int quantidade;


    public EscolheProduto(){
        this.produto = "";
        this.quantidade = 0;

    }


    public EscolheProduto(String prod, int quantidade){
        this.produto = prod;
        this.quantidade = quantidade;
    }


    /*
    public List<String> getProdutos() {
        List<String> clones = new ArrayList<>();
        for(String s : produtos){
            clones.add(s);
        }

        return clones;
    }

     */

    public String getProduto() {
        return produto;
    }

    public int getQuantidade() {
        return quantidade;
    }
}
