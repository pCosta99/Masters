package MVC.Views;

import MVC.IView;

import java.util.Arrays;
import java.util.List;

public class MenuView implements IView
{
    private List<String> opcoes;
    //private int op;

    public MenuView(String[] opcoes){
        this.opcoes = Arrays.asList(opcoes);
        //this.op = 0;
    }

    public MenuView( List<String> opcoes){
        this.opcoes = opcoes;
        //this.op = 0;
    }

    // apresenta menu e le opcao
    /*public void executa(){
        do{
            showMenu();
            this.op = lerOpcao();
        }while (this.op == -1);

    }*/

    public void setOpcoes(List<String> opcoes) {
        this.opcoes = opcoes;
    }

    //Apresenta o menu
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

    //le opcao valida
    /*private int lerOpcao(){
        int op;
        Scanner is = new Scanner(System.in);

        System.out.print("Opcao: ");
        try{
            op = is.nextInt();
        }
        catch(InputMismatchException e){
            op = -1;
        }
        if (op<0 || op>this.opcoes.size()){
            System.out.println("opcao invalida!!!");
            op = -1;
        }
        return op;
    }

    public int getOpcao(){
        return this.op;
    }*/
}