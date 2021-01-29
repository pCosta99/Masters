import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

public class Menu implements Serializable
{
    private List<String> opcoes;
    private int escolha;


    public Menu (String[] lista){
        this.opcoes=Arrays.asList(lista);
        this.escolha=0;
    }

    public Menu (){
        this.opcoes=null;
        this.escolha=0;
    }

    public void executa() {
        do {
            showMenu();
            this.escolha = lerOpcao();
        } while (this.escolha == -1);
    }

    public void showMenu(){
        System.out.println("\n Selecione uma opção:\n");
        for (int i=0; i<this.opcoes.size(); i++){
            System.out.print(i+1);
            System.out.print(" - ");
            System.out.println(this.opcoes.get(i));
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

        if (op < 0 || op > this.opcoes.size()){
            System.out.println("Opção inválida!!!");
            op = -1;
        }
        return op;
    }

    public int getOpcao(){
        return this.escolha;
    }

    public void setOpcao(int op){ 
        this.escolha = op; 
    }

    public String leString(){
        Scanner sc = new Scanner(System.in);
        String input;
        try {
            input = sc.next();
        }
        catch (InputMismatchException e){
            input = "";
        }
        return input;
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

    public double leDouble(){
        double op;
        Scanner sc = new Scanner(System.in);
        try{
            op = sc.nextDouble();
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

    public String leSimNao(){
        String op;
        Scanner sc = new Scanner(System.in);
        do{
            System.out.println("(Sim/Nao)");
            try{
                op = sc.nextLine();
                if(op.equals("Sim") || op.equals("Nao")) {
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
        }while(op==null || !op.equals("Sim") || !op.equals("Nao"));
        return op;
    }

    public LocalDateTime lerData(){
        int dia,mes,ano,hora,minuto,segundo;
        do {
            System.out.print("Inserir ano: ");
            ano = leInt();
        }while(ano <=- 1 || ano > 10000);

        do {
            System.out.print("Inserir mês: ");
            mes = leInt();
        }while(mes <= 0 || mes > 12);

        do {
            System.out.print("Inserir dia: ");
            dia = leInt();
        }while(dia <= 0 || dia > 31);

        do {
            System.out.print("Inserir hora: ");
            hora = leInt();
        }while(hora < 0 || hora >= 24);

        do {
            System.out.print("Inserir minutos: ");
            minuto = leInt();
        }while(minuto < 0 || minuto >= 60);

        do {
            System.out.print("Inserir segundos: ");
            segundo = leInt();
        }while(segundo < 0 || segundo >= 60);

        return LocalDateTime.of(ano,mes,dia,hora,minuto,segundo);
    }
}