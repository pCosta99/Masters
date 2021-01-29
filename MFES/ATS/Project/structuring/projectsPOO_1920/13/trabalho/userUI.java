import java.io.*;
import java.nio.file.*;
import java.util.*;
public class userUI
{
    public userUI(String tipo,records rec)
    {
        Scanner input = new Scanner(System.in);
        String s="1 -> Fazer Encomenda"+ "\n" +"2 -> Encomendas recebidas";
        System.out.println(s);
        List<String> arr = new ArrayList<String>(2); 
        arr.add("2"); 
        arr.add("1");
        String op=input.nextLine();
        List<String> l = new ArrayList(Arrays.asList(tipo.split(",")));
        while(!arr.contains(op)){
            s="input invalido tente outra vez";
            System.out.println(s);
            op=input.nextLine();
        }
           if(op.equals("1")){
            compras(tipo,rec);
        }
        if(op.equals("2")){
            recebidas(l.get(0));
        }
    }
    public void recebidas(String user){
        String file = ler("recibos.txt");
        List<String> linhas = new ArrayList(Arrays.asList(file.split("\\r?\\n")));
        Iterator<String> itr = null;
        itr = linhas.iterator(); 
        while (itr.hasNext()){
            String s=itr.next();
            if(s.contains(user)){
                System.out.println(s);
            }
        }
    }
    public void compras(String tipo,records rec){
        Scanner input = new Scanner(System.in);
        Map<String, Loja> lojas = rec.getAllLojas();
        System.out.println("----------------Escolha uma loja:--------------");
        lojas.forEach((key, value) -> System.out.println(key + " --> " + value.getnome()));
        System.out.println("Escreva o codigo da loja:");
        String op=input.nextLine();
        while(!lojas.containsKey(op)){
            System.out.println("input invalido tente outra vez");
            op=input.nextLine();
        }
        lojas.get(op).getAllprodutos().forEach((key, value) -> System.out.println(key + " --> " + value.getdesc()+",   preço:"+value.getpreco()+"$"));
        System.out.println("Escolha o que pertende comprar: (Ex:codigo1,quantidade,codigo2,quantidade)");
        String prods=input.nextLine();
        allocar(tipo,op,prods,lojas.get(op),rec);
        System.out.println("compra efetua, assim que a loja estiver pronta recebera as opçoes de transporte(se existir um volutario disponivel sera autumaticamente atribuido)");
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
    public void allocar(String user,String loja,String prods,Loja l,records rec){
        Map<String, LinhaEncomenda> pord = l.getAllprodutos();
        List<String> p = new ArrayList(Arrays.asList(prods.split(",")));
        Iterator<String> itr = null; 
        itr = p.iterator();
        List<String> u = new ArrayList(Arrays.asList(user.split(",")));
        String s="";
        int sum=0;
        while(itr.hasNext()){
            String i=itr.next();
            if(!pord.containsKey(i)){
                System.out.println("lista invalida tente novamente");
                compras(user,rec);
            }
            else{
                String peso=itr.next();
                sum+=Double.parseDouble(peso);
                s=s+","+i+","+l.getAllprodutos().get(i).getdesc()+","+peso+","+l.getAllprodutos().get(i).getpreco();
            }
        }
        s="\n"+"e"+String.valueOf(conta()+900)+","+u.get(0)+","+loja+","+String.valueOf(sum)+s;
        escreve(s);
    }
    public int conta(){
        try {
            BufferedReader reader = new BufferedReader(new FileReader("log.txt"));
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
