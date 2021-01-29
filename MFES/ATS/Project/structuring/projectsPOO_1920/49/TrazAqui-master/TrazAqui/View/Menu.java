package View;

import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class Menu implements Serializable
{
    private int op;
    private List<String>opcoes;



    public Menu(String[] opcoes){
        this.opcoes=new ArrayList<>();
        for(String op: opcoes)
            this.opcoes.add(op);

        this.op=0;

    }

    public void showMenu(){

        System.out.println("0 - Sair");
        for(int i=0;i<this.opcoes.size();i++){
            System.out.print(i+1);
            System.out.print(" - ");
            System.out.println(this.opcoes.get(i));
        }

    }

    public int getOp (){
        return this.op;
    }

    public void executa(){
        do{
            showMenu();
            this.op=escolherOp();

        }
        while(this.op==-1);
    }
    private int escolherOp( )
    {
        int op;


        System.out.println("Opcao:");
        op = Input.lerInt();
        if (op<0||op>this.opcoes.size()) {
            System.out.println("Opção Inválida!");
            op=-1;
        }

        return op;

    }

}