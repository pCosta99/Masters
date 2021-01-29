import java.util.*;
import java.io.*;
import java.nio.file.*;
import java.util.Scanner;
public class login
{
    private String mail;
    private String pass;
    private String tipo;
    public login() {
        Scanner input = new Scanner(System.in);
        String s="LOGIN:"+ "\n";
        System.out.println(s);
        s="Email:";
        System.out.println(s);
        String mail=input.nextLine();
        this.mail=mail;
        s="Password:";
        System.out.println(s);
        String pass=input.nextLine();
        this.pass=pass;
        while(!isvalid()){
            System.out.println("login errado tente novamente");
            s="Email:";
            System.out.println(s);
            mail=input.nextLine();
            this.mail=mail;
            s="Password:";
            System.out.println(s);
            pass=input.nextLine();
            this.pass=pass;
        }
        if(isvalid()){
                new loginUI(tipo);
        }
    }
    public Boolean isvalid(){
        String file = ler("contas.txt");
        List<String> linhas = new ArrayList(Arrays.asList(file.split("\\r?\\n")));
        Iterator<String> itr = null;
        itr = linhas.iterator(); 
        while (itr.hasNext()){
            String s=itr.next();
            if(s.contains(mail+","+pass)){
                this.tipo=s;
                return true;
            }
        }
        return false;
    }
    public String getmail(){
        return this.mail;
    }
    public String getpass(){
        return this.pass;
    }
    public String ler(String txt) {
       String path = txt;
       try {
            String content = Files.readString(Paths.get(path));
            return (content);
        } 
        catch (IOException e) {
            e.printStackTrace();
        }
        return("");
    }
}
