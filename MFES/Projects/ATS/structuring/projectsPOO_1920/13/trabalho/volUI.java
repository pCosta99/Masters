import java.io.*;
import java.nio.file.*;
import java.util.*;

public class volUI
{
    public volUI(String tipo,records rec)
    {
        Scanner input = new Scanner(System.in);
        System.out.println("1 -> Entregas disponiveis" );
        List<String> arr = new ArrayList<String>(); 
        arr.add("1");
        arr.add("2");
        List<String> v = new ArrayList(Arrays.asList(tipo.split(",")));
        List<String> a = new ArrayList<>();
        String op=input.nextLine();
        while(!arr.contains(op)){
            System.out.println("input invalido tente outra vez");
            op=input.nextLine();
        }
        if(op.equals("1")){
            a=validos(rec,rec.getaceites(),v);
            System.out.println("Qual encomenda para entreguar:");
            op=input.nextLine();
            while(!a.contains(op)){
            System.out.println("input invalido tente outra vez");
            op=input.nextLine();
           }
           String user=rec.getAllenc().get(op).getuser();
           String loja=rec.getAllenc().get(op).getloja();
           double userlat=rec.getAlluser().get(user).getlat();
           double userlong=rec.getAlluser().get(user).getlong();
           double vollat= Double.parseDouble(v.get(2));
           double vollong= Double.parseDouble(v.get(3));
           double ac = Math.abs(vollong - userlong);
           double cb = Math.abs(vollat - userlat);
           double distu = Math.hypot(ac, cb);
           double lojalat=rec.getAllLojas().get(loja).getlat();
           double lojalong=rec.getAllLojas().get(loja).getlong();
           ac = Math.abs(vollong - lojalong);
           cb = Math.abs(vollat - lojalat);
           double distl = Math.hypot(ac, cb);
           double distlu=Math.hypot(Math.abs(userlong - lojalong), Math.abs(userlat - lojalat));
           escreve(op+","+rec.getAllenc().get(op).getuser()+","+rec.getAllenc().get(op).getloja()+","+rec.getAllenc().get(op).getpeso()+"kg, "+tempo(distl+distu+distlu)+"min, entrege por:"+v.get(0));
           System.out.println("entregue");
        }
        
    }
    public void escreve(String q) 
    {
       try { 
           byte b[]=q.getBytes();
           FileOutputStream log =new FileOutputStream("recibos.txt", true);
           log.write(b);
           log.close();
       }
        catch (IOException e) {  
            System.out.println("erro 2");
       } 
    
    }
    public List<String> validos(records rec,List<String> aceites,List<String> voluntario){
        List<String> fin = new ArrayList<>();
        for(String a:aceites){
            String user=rec.getAllenc().get(a).getuser();
            String loja=rec.getAllenc().get(a).getloja();
            double raio=Double.parseDouble(voluntario.get(4));
            double userlat=rec.getAlluser().get(user).getlat();
            double userlong=rec.getAlluser().get(user).getlong();
            double vollat= Double.parseDouble(voluntario.get(2));
            double vollong= Double.parseDouble(voluntario.get(3));
            double ac = Math.abs(vollong - userlong);
            double cb = Math.abs(vollat - userlat);
            double distu = Math.hypot(ac, cb);
            double lojalat=rec.getAllLojas().get(loja).getlat();
            double lojalong=rec.getAllLojas().get(loja).getlong();
            ac = Math.abs(vollong - lojalong);
            cb = Math.abs(vollat - lojalat);
            double distl = Math.hypot(ac, cb);
            double distlu=Math.hypot(Math.abs(userlong - lojalong), Math.abs(userlat - lojalat));
            
            if(distl<raio && distu<raio){
                System.out.println(a+" com peso de "+String.valueOf(rec.getAllenc().get(a).getpeso())+"kg, e a distacia a percorre é "+String.valueOf(distl+distu+distlu)+"km, o que demorara "+ tempo(distl+distu+distlu)+ "min, devido ao transito");
                fin.add(a);
            }
            
        }
        return fin;
    }
    public String tempo(double dist){
        return String.valueOf(Math.random() * dist);
    }
}
