import java.util.*;
import java.io.*;
import java.nio.file.*;
import java.util.Scanner;

public class registo
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio

    public registo(){
        Scanner input = new Scanner(System.in);
        String s="=====registo====";
        System.out.println(s);
        s="1 -> Utiluzador"+ "\n" +"2 -> Voluntario"+ "\n"+"3 -> Transportadora"+"\n"+"4 -> Loja";
        System.out.println(s);
        List<String> arr = new ArrayList<String>(2); 
        arr.add("2"); 
        arr.add("1");
        arr.add("3"); 
        arr.add("4");
        String op=input.nextLine();
        while(!arr.contains(op)){
            s="input invalido tente outra vez";
            System.out.println(s);
            op=input.nextLine();
        }
        String tipo="";
        if(op.equals("1")){
            s="Nome:";
            System.out.println(s);
            String nome=input.nextLine();
            s="Latitude:";
            System.out.println(s);
            String lat=input.nextLine();
            s="Longitude:";
            System.out.println(s);
            String lon=input.nextLine();
            s="Email:";
            System.out.println(s);
            String mail=input.nextLine();
            s="Password:";
            System.out.println(s);
            String pass=input.nextLine();
            tipo=("u"+String.valueOf(conta()+100)+","+nome+","+lat+","+lon+","+mail+","+pass);
            escreve("\n"+tipo);
        }
        if(op.equals("2")){
            s="Nome:";
            System.out.println(s);
            String nome=input.nextLine();
            s="Latitude:";
            System.out.println(s);
            String lat=input.nextLine();
            s="Longitude:";
            System.out.println(s);
            String lon=input.nextLine();
            s="raio:";
            System.out.println(s);
            String raio=input.nextLine();
            s="Email:";
            System.out.println(s);
            String mail=input.nextLine();
            s="Password:";
            System.out.println(s);
            String pass=input.nextLine();
            tipo=("v"+String.valueOf(conta()+100)+","+nome+","+lat+","+lon+","+raio+","+mail+","+pass);
            escreve("\n"+tipo);
        }
        if(op.equals("3")){
            s="Nome:";
            System.out.println(s);
            String nome=input.nextLine();
            s="Latitude:";
            System.out.println(s);
            String lat=input.nextLine();
            s="Longitude:";
            System.out.println(s);
            String lon=input.nextLine();
            s="raio:";
            System.out.println(s);
            String raio=input.nextLine();
            s="Preço por Km:";
            System.out.println(s);
            String price=input.nextLine();
            s="Email:";
            System.out.println(s);
            String mail=input.nextLine();
            s="Password:";
            System.out.println(s);
            String pass=input.nextLine();
            tipo=("t"+String.valueOf(conta()+100)+","+nome+","+lat+","+lon+","+raio+","+price+","+mail+","+pass);
            escreve("\n"+tipo);
        }
        if(op.equals("4")){
            s="Nome:";
            System.out.println(s);
            String nome=input.nextLine();
            s="Latitude:";
            System.out.println(s);
            String lat=input.nextLine();
            s="Longitude:";
            System.out.println(s);
            String lon=input.nextLine();
            s="Email:";
            System.out.println(s);
            String mail=input.nextLine();
            s="Password:";
            System.out.println(s);
            String pass=input.nextLine();
            tipo=("l"+String.valueOf(conta()+100)+","+nome+","+lat+","+lon+","+mail+","+pass);
            escreve("\n"+tipo);
        }
        System.out.println("registo concluido");
        new loginUI(tipo);
    }
    public void escreve(String q) 
    {
       try { 
           FileOutputStream contas =new FileOutputStream("contas.txt", true);
           byte b[]=q.getBytes();
           contas.write(b);
           contas.close();
           FileOutputStream log =new FileOutputStream("log.txt", true);
           log.write(b);
           log.close();
       }
        catch (IOException e) {  
        System.out.println("erro 2");
       } 
    
    }
    public int conta(){
        try {
            BufferedReader reader = new BufferedReader(new FileReader("contas.txt"));
            int lines = 0;
            while (reader.readLine() != null) lines++;
            reader.close();
            return(lines);
        }
        catch (IOException e) {
           System.out.println("erro 1");
        }
        return(0);
    }
}
