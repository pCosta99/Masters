package View;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class ViewGeral {
    public int n1;
    public ViewGeral(){
    }

    public int viewGeral(){
        System.out.println("\nBem Vindo ao TrazAqui!\n  Login(1)\n  Signup(2)\n  Carregar ficheiro de testo(3)\n  Carregar ficheiro binario(4)\n  Sair(0)");
        Scanner n2 = new Scanner(System.in);
        n1 = n2.nextInt();

        return n1;
    }

    public int login(){
        System.out.println("Iniciar conta como:\n  Utilizador(1)\n  Voluntario(2)\n  Transportadora(3)\n  Loja(4)\n  Sair(0)");
        Scanner n3 = new Scanner(System.in);
        n1 = n3.nextInt();

        return n1;
    }

    public void finish(){
        System.out.println("Obrigado!");
    }

    public void limpa() {
        for (int i = 0; i < 50; ++i) System.out.println();
    }

    public int signup(){
        System.out.println("Criar conta como:\n  Utilizador(1)\n  Voluntario(2)\n  Transportadora(3)\n  Loja(4) \n  Sair(0)");
        Scanner n2 = new Scanner(System.in);
        n1 = n2.nextInt();

        for (int i = 0; i < 50; ++i) System.out.println();
        return n1;
    }

    public List<String> registaUtil(){
        List<String> ret = new ArrayList<>();

        System.out.println("Email:");
        Scanner n2 = new Scanner(System.in);
        String m = n2.nextLine();
        ret.add(m);

        System.out.println("Password:");
        Scanner p1 = new Scanner(System.in);
        String p = p1.nextLine();
        ret.add(p);

        System.out.println("Nome completo:");
        Scanner n6 = new Scanner(System.in);
        String n = n6.nextLine();
        ret.add(n);

        System.out.println("GPS\nCoordenada x:");
        Scanner n1 = new Scanner(System.in);
        double x = n1.nextDouble();
        ret.add(Double.toString(x));

        System.out.println("GPS\nCoordenada y:");
        Scanner n4 = new Scanner(System.in);
        double y = n4.nextDouble();
        ret.add(Double.toString(y));

        return ret;
    }

    public List<String> registaVolun(){
        List<String> ret = new ArrayList<>();

        System.out.println("Email:");
        Scanner n5 = new Scanner(System.in);
        String m = n5.nextLine();
        ret.add(m);

        System.out.println("Password:");
        Scanner p1 = new Scanner(System.in);
        String p = p1.nextLine();
        ret.add(p);

        System.out.println("Nome completo:");
        Scanner n2 = new Scanner(System.in);
        String v = n2.nextLine();
        ret.add(v);

        System.out.println("GPS\nCoordenada x:");
        Scanner n1 = new Scanner(System.in);
        double x = n1.nextDouble();
        ret.add(Double.toString(x));

        System.out.println("GPS\nCoordenada y:");
        Scanner n4 = new Scanner(System.in);
        double y = n4.nextDouble();
        ret.add(Double.toString(y));

        System.out.println("Raio:");
        Scanner r1 = new Scanner(System.in);
        double r = r1.nextDouble();
        ret.add(Double.toString(r));

        System.out.println("Velocidade media:");
        Scanner n12= new Scanner(System.in);
        double vm = n12.nextDouble();
        ret.add(Double.toString(vm));

        System.out.println("Tem certificado médico:\nVerdade(1)\nFalso(0)");
        Scanner e23= new Scanner(System.in);
        double tf = e23.nextDouble();
        ret.add(Double.toString(tf));

        return ret;
    }

    public List<String> registaTransp(){
        List<String> ret = new ArrayList<>();

        System.out.println("Email:");
        Scanner n5 = new Scanner(System.in);
        String m = n5.nextLine();
        ret.add(m);

        System.out.println("Password:");
        Scanner p1 = new Scanner(System.in);
        String p = p1.nextLine();
        ret.add(p);

        System.out.println("Nome completo:");
        Scanner n2 = new Scanner(System.in);
        String t = n2.nextLine();
        ret.add(t);

        System.out.println("GPS\nCoordenada x:");
        Scanner n1 = new Scanner(System.in);
        double x = n1.nextDouble();
        ret.add(Double.toString(x));

        System.out.println("GPS\nCoordenada y:");
        Scanner n4 = new Scanner(System.in);
        double y = n4.nextDouble();
        ret.add(Double.toString(y));

        System.out.println("Raio:");
        Scanner r1 = new Scanner(System.in);
        double r = r1.nextDouble();
        ret.add(Double.toString(r));

        System.out.println("Nif:");
        Scanner nif1 = new Scanner(System.in);
        int nif = nif1.nextInt();
        ret.add(Integer.toString(nif));

        System.out.println("Preço por kilometro:");
        Scanner pkm1 = new Scanner(System.in);
        double pkm = pkm1.nextDouble();
        ret.add(Double.toString(pkm));

        System.out.println("Numero de encomendas que pode transportar por viagem:");
        Scanner n123 = new Scanner(System.in);
        double qtenc = n123.nextDouble();
        ret.add(Double.toString(qtenc));

        System.out.println("Velocidade media:");
        Scanner n12= new Scanner(System.in);
        double vm = n12.nextDouble();
        ret.add(Double.toString(vm));

        System.out.println("Tem certificado médico:\nVerdade(1)\nFalso(0)");
        Scanner e23= new Scanner(System.in);
        double tf = e23.nextDouble();
        ret.add(Double.toString(tf));

        return ret;
    }

    public List<String> registaLoja() {
        List<String> ret = new ArrayList<>();

        System.out.println("Email:");
        Scanner n2 = new Scanner(System.in);
        String m = n2.nextLine();
        ret.add(m);

        System.out.println("Password:");
        Scanner p1 = new Scanner(System.in);
        String p = p1.nextLine();
        ret.add(p);

        System.out.println("Nome completo:");
        Scanner n6 = new Scanner(System.in);
        String n = n6.nextLine();
        ret.add(n);

        System.out.println("GPS\nCoordenada x:");
        Scanner n1 = new Scanner(System.in);
        double x = n1.nextDouble();
        ret.add(Double.toString(x));

        System.out.println("GPS\nCoordenada y:");
        Scanner n4 = new Scanner(System.in);
        double y = n4.nextDouble();
        ret.add(Double.toString(y));

        System.out.println("Tempo médio de atendimento:");
        Scanner n12 = new Scanner(System.in);
        int tp = n12.nextInt();
        ret.add(Integer.toString(tp));

        System.out.println("Tem informaçao sobre a fila de espera?\nSIM(1)\nNAO(2)");
        Scanner n5 = new Scanner(System.in);
        int s = n5.nextInt();
        ret.add(Integer.toString(s));
        if(s==1){
            System.out.println("Fila de espera:");
            Scanner n8 = new Scanner(System.in);
            int esp = n8.nextInt();
            ret.add(Integer.toString(esp));
        }

        return ret;
    }

    public void erroDeIdent(){
        System.out.println("Email ou password incorretos.");
    }

    public int classificaçao(){
        System.out.println("Classifique de 0 a 10");
        Scanner n5 = new Scanner(System.in);
        int s = n5.nextInt();
        if(s<0 || s>10)  System.out.println("Classificacao nao aceite.");
        return s;
    }

    public void printEncomendas(String e){
        System.out.println(e);
    }

    public int atualizafila(){
        System.out.println("Fila de espera:");
        Scanner n5 = new Scanner(System.in);
        int s = n5.nextInt();
        return s;
    }

    public void showFila(int a){
        System.out.println("Fila de espera: "+a);
    }

    public void showS(int a){
        System.out.println(a);
    }

    public void showTop(List<String> ret) {
        int i = 0;
        for(String t : ret){
            i++;
            System.out.println("i: "+ t);
        }
    }

    public int fazEncom(List<String> lojas){
        int a = 1;
        for(String s : lojas){
            System.out.println(s+"(a)");
            a++;
        }
        Scanner n5 = new Scanner(System.in);
        int b = n5.nextInt();
        return b;
    }

    public Double fazEncom1(){
        System.out.println("Peso da encomenda:");
        Scanner n5 = new Scanner(System.in);
        Double b = n5.nextDouble();
        return b;
    }

    public Boolean fazEncom2(){
        System.out.println("É uma encomenda medica:\nSim(1)\nNao(0)");
        Scanner n5 = new Scanner(System.in);
        int b = n5.nextInt();
        if (b==1) return true;
        else return false;
    }

    public List<String> escolherProduto(){
        List<String> ret = new ArrayList<>();
        System.out.println("Codigo do produto:");
        Scanner n5 = new Scanner(System.in);
        String b = n5.nextLine();
        ret.add(b);

        System.out.println("Descriçao do produto:");
        Scanner n6 = new Scanner(System.in);
        b = n6.nextLine();
        ret.add(b);

        System.out.println("Quantidade do produto:");
        Scanner n1 = new Scanner(System.in);
        double a = n1.nextDouble();
        ret.add(Double.toString(a));

        System.out.println("Preco:");
        Scanner n2 = new Scanner(System.in);
        a = n1.nextDouble();
        ret.add(Double.toString(a));

        return ret;
    }
    public int addMaisProduto() {
        System.out.println("Quer adicional mais produtos?\nSim(1)\nNao(0)");
        Scanner n5 = new Scanner(System.in);
        int b = n5.nextInt();
        return b;
    }
}
