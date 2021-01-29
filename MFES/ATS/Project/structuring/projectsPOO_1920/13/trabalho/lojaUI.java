import java.io.*;
import java.nio.file.*;
import java.util.*;

public class lojaUI
{
    public lojaUI(String tipo,records rec)
    {
        Scanner input = new Scanner(System.in);
        System.out.println("1 -> Encomendas recebidas");
        List<String> arr = new ArrayList<String>(1); 
        arr.add("1");
        String op=input.nextLine();
        while(!arr.contains(op)){
            System.out.println("input invalido tente outra vez");
            op=input.nextLine();
        }
        if(op.equals("1")){
            confirmar(tipo,rec);
        }

    }
    public void confirmar(String tipo,records rec){
        System.out.println("Encomendas em espera:");
        List<String> l = new ArrayList(Arrays.asList(tipo.split(",")));
        String loja=l.get(0);
        List<String> pend=new ArrayList<String>();
        Map<String, encomenda> enc = rec.getAllenc();
        enc.forEach((key, value) -> {;
                if(value.getloja().equals(loja)) {
                    pend.add(key);
                    System.out.println(key + " --> " + value.getuser());
                } 
            });
        Scanner input = new Scanner(System.in);
        System.out.println("encomenda prontas: (ex:e123,e125)");
        String op=input.nextLine();
        List<String> fin = new ArrayList(Arrays.asList(op.split(",")));
        for(String a:fin){
            escreve(a);
        }
    }
    public void escreve(String q) 
    {
       try { 
           byte b[]=q.getBytes();
           FileOutputStream log =new FileOutputStream("log.txt", true);
           log.write(b);
           log.close();
       }
        catch (IOException e) {  
            System.out.println("erro 2");
       } 
    
    }
}
