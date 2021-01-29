

import java.io.Serializable;
import java.util.*;

public class Menu implements Serializable
{
    private List<String> opcoes;
    private int op;


    public Menu(String[] opcoes){
        this.opcoes = Arrays.asList(opcoes);//converte o array estatico(ou uma sequencia de elementos) para uma lista
        this.op = 0;
    }

    public Menu(List<String> opcoes){
        this.opcoes = opcoes;//converte o array estatico(ou uma sequencia de elementos) para uma lista
        this.op = 0;
    }

    public void executa(){
        do{
            showMenu();
            this.op = lerOpcao();
        } while(this.op == -1);
    }

    public void showMenu(){
        System.out.println("\n ---------------------------------------");
        System.out.println("|               Traz Aqui!              |");
        System.out.println(" ---------------------------------------\n");
        for(int i=0;i<this.opcoes.size();i++){
            System.out.print(i+1);
            System.out.print(" - ");
            System.out.println(this.opcoes.get(i));
        }
        System.out.println("0 - Sair");
    }

    private int lerOpcao(){
        int op;
        Scanner is = new Scanner(System.in);

        System.out.println("Opçao: ");
        try{
            op = is.nextInt();
        } catch(InputMismatchException e){
            op = -1;
        }

        if(op<0 || op>this.opcoes.size()){

            System.out.println("Opçao Invalida");
            op = -1;
        }
        return op;
    }

    public int getOpcao(){return this.op;}

}