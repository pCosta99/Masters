package MVC.Views;

import MVC.IView;

import java.util.List;

public class MenuView implements IView
{
    private List<String> opcoes;

    public MenuView( List<String> opcoes){
        this.opcoes = opcoes;
    }

    public void setOpcoes(List<String> opcoes) {
        this.opcoes = opcoes;
    }

    public void show(){
        System.out.println("******* Menu *******");
        System.out.println("####################");
        for (int i=0; i<this.opcoes.size();i++){
            System.out.print(i+1);
            System.out.print(" - ");
            System.out.println(this.opcoes.get(i));
        }
        System.out.println("0 - Sair");
        System.out.println("####################");
    }

    public void show(Object o){
        System.out.println(o);
    }
}