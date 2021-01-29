package view;

import model.GPS;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

public class Menu implements Serializable {
    private List<String> opcoes;
    private int op;

    public Menu(String[] opcoes){
        this.opcoes = Arrays.asList(opcoes);
        this.op = 0;
    }

    public Menu(List<String> opcoes){
        this.opcoes = opcoes;
        this.op = 0;
    }


    public void executa() {
        do {
            showMenu();
            this.op = lerOpcao();
        } while (this.op == -1);
    }

    public void showMenu(){
        System.out.println("\n Selecionar opção:");
        if(this.opcoes!=null) {
            for (int i = 0; i < this.opcoes.size(); i++) {
                System.out.print(i + 1);
                System.out.print(" - ");
                System.out.println(this.opcoes.get(i));
            }
        }
        System.out.println("0 - Sair");
    }

    public int lerOpcao(){
        int op;
        Scanner sc = new Scanner(System.in);
        System.out.print("Opção: ");
        try {
            op = sc.nextInt();
        }
        catch (InputMismatchException e){
            op = -1;
        }
        if (this.opcoes!=null) {
            if (op < 0 || op > this.opcoes.size()) op = -1;
        }
        return op;
    }


    public int getOp(){
        return this.op;
    }

    public double lerDouble(){
        double op = -1;
        Scanner sc = new Scanner(System.in);
        //System.out.print("Input: ");
        try {
            op = sc.nextDouble();
        }
        catch (InputMismatchException e){
            System.out.println("Não foi um double");
        }
        return op;
    }

    public GPS lerLocalizacao(){
        double cx,cy;
        Scanner sc = new Scanner(System.in);
        System.out.println("Inserir (cx , cy): ");
        try{
            System.out.print("Componente cx: ");
            cx = sc.nextDouble();
        }
        catch (InputMismatchException e){
            System.out.println("A componente X não é um double.");
            return null;
        }
        try {
            System.out.print("Componente cy: ");
            cy = sc.nextDouble();
        }
        catch (InputMismatchException e){
            System.out.println("A componente Y não é um double.");
            return null;
        }
        return new GPS(cx,cy);
    }


    public String leString(){
        String op = null;
        Scanner sc = new Scanner(System.in);
        try {
            op = sc.nextLine();
        }
        catch(InputMismatchException e) {
            System.out.println("String Inválida.");
            op = null;
        }
        return op;
    }

    public int leInt(){
        int op;
        Scanner sc = new Scanner(System.in);
        try{
            op = sc.nextInt();
            if(op<0) {
                op = -1;
            }
        }
        catch(InputMismatchException e){
            System.out.println("Não é um número inteiro válido.");
            op = -1;
        }
        return op;
    }

    public String leYesNo(){
        String op;
        Scanner sc = new Scanner(System.in);
        try{
            op = sc.nextLine();
            if(op.equals("y") || op.equals("n")) {
                return op;
            }
            else {
                op = null;
            }
        }
        catch (InputMismatchException e) {
            System.out.println("Não é uma escolha válida.");
            op = null;
        }
        return op;
    }

    public LocalDateTime lerData(){
        int dataD,dataM,dataA;
        do {
            System.out.print("Inserir dia: ");
            dataD = leInt();
        }while(dataD<=0 || dataD > 31);

        do {
            System.out.print("Inserir mês: ");
            dataM = leInt();
        }while(dataM<=0 || dataM > 12);

        do {
            System.out.print("Inserir ano: ");
            dataA = leInt();
        }while(dataA==-1 || dataA > 2020);

        return LocalDateTime.of(dataA,dataM,dataD,0, 0);
    }

    public void setOp(int op){ this.op = op; }

    public String getOpcion(int op){ return this.opcoes.get(op); }

    public void sendMessage(String message){
        System.out.println(message);
    }

    public void sendMessage(String message, double d, String last){
        System.out.print(message);
        System.out.printf("%.2f", d);
        System.out.println(last);
    }

}
