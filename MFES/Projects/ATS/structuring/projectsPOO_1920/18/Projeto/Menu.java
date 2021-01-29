import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.lang.String;
import java.util.Scanner;
import java.io.*;

public class Menu{
    
    private List<String> opcoes;
    private int opcao;
    
    public Menu (){
        this.opcoes=new ArrayList<>(); 
    }
    
    public Menu(List<String> op, int o){
        this.opcoes=op;
        this.opcao=o;
    }
    
    public Menu (Menu m){
        this.opcoes=m.getOpcoes();
        this.opcao=m.getOpcao();
    }
    
    public Menu(String[] op){
        this.opcoes=Arrays.asList(op);
        this.opcao=0;
    }
    
    public List<String> getOpcoes(){
        return new ArrayList<>(this.opcoes);
    }
    
    public void setOpcoes(List<String> op){
        this.opcoes=new ArrayList<>(op);
    }
    
    public int getOpcao(){
        return this.opcao;
    }
    
    public void setOpcao(int op){
        this.opcao=op;
    }  
    
    public void executa() throws IOException{
        StringBuilder sb=new StringBuilder();
        for (int i=0;i<getOpcoes().size();i++){
            sb.append(this.opcoes.get(i)).append("\n");
        }
        sb.append("0) Sair");
        System.out.println(sb.toString());
        Scanner input =(new Scanner(System.in));
        try {this.opcao=input.nextInt();}
        catch (Exception exc) {System.out.println("Opcao invalida\n");this.opcao=-1;}
        
    }
    
}



















