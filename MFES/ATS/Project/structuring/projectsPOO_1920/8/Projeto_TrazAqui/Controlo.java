import java.io.Serializable;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.time.LocalDate;

import java.time.format.DateTimeFormatter;
import java.util.*;

public class Controlo implements Serializable {

    public static void menu_registo(TrazAqui t, Menu m){
        Scanner read = new Scanner(System.in);
        String linha;
        int opcao;

        do {
            m.tipo_registo();
            do {
                System.out.println("Escolha uma opção válida: ");
                linha = read.nextLine();
            } while (!Menu.isNumeric(linha) || linha.length() < 1);

            opcao = Integer.parseInt(linha);

            if (opcao != 0) {
                System.out.println("Qual o seu nome: ");
                String nome = read.nextLine();
                String mail = "";
                do {
                    System.out.println("Escreva o seu username: ");
                    mail = read.nextLine();
                }while(t.getutilizadores().containsKey(mail));

                System.out.println("Escreva a password: ");
                String pass = read.nextLine();

                do {
                    System.out.println("Diga a sua coordenada x: ");
                    linha = read.nextLine();
                } while (!Menu.isNumeric(linha) || linha.length() < 1);

                double x = Double.parseDouble(linha);

                do {
                    System.out.println("Diga a sua coordenada y: ");
                    linha = read.nextLine();
                } while (!Menu.isNumeric(linha) || linha.length() < 1);

                double y = Double.parseDouble(linha);

                switch (opcao) {
                    case 1:
                        System.out.println("Diga o seu raio de ação: ");
                        do{
                            linha = read.nextLine();
                        }while (!Menu.isNumeric(linha) || linha.length() < 1);

                        double raio = Double.parseDouble(linha);

                        System.out.println("Faça uma estimativa da velocidade a que se vai deslocar: ");
                        do{
                            linha = read.nextLine();
                        }while (!Menu.isNumeric(linha) || linha.length() < 1);

                        double vel = Double.parseDouble(linha);

                        System.out.println("Tem verificação para transportar encomendas médicas(S ou N)? ");
                        char ch;
                        do{
                            ch = read.next().charAt(0);
                        }while (ch !='S' && ch  != 's' && ch !='N' && ch  != 'n');

                        boolean visto = false;
                        if (ch == 'S' || ch == 's') {
                            visto = true;
                        }
                        Voluntario v = new Voluntario(nome, mail, pass, new Coordenadas(x, y), 0, 0, true, raio, vel, visto, new ArrayList<>());
                        t.addUser(v);
                        break;
                    case 2:
                        Cliente c = new Cliente(nome, mail, pass, new Coordenadas(x, y), new ArrayList<>(),new HashMap<>());
                        t.addUser(c);
                        break;

                    case 3:
                        System.out.println("Diga o nif da empresa: ");
                        String nif = read.nextLine();

                        System.out.println("Taxa de custo por km: ");
                        do{
                            linha = read.nextLine();
                        }while (!Menu.isNumeric(linha) || linha.length() < 1);
                        double taxa = Double.parseDouble(linha);

                        System.out.println("Quantas encomendas pode transportar de uma vez: ");
                        do{
                            linha = read.nextLine();
                        }while (!Menu.isNumeric(linha) || linha.length() < 1);
                        int cap = Integer.parseInt(linha);

                        System.out.println("Faça uma estimativa da velocidade a que se vai deslocar: ");
                        do{
                            linha = read.nextLine();
                        }while (!Menu.isNumeric(linha) || linha.length() < 1);
                        vel = Double.parseDouble(linha);

                        System.out.println("Diga o seu raio de ação: ");
                        do{
                            linha = read.nextLine();
                        }while (!Menu.isNumeric(linha) || linha.length() < 1);
                        raio = Double.parseDouble(linha);

                        System.out.println("Tem verificação para transportar encomendas médicas(S ou N)? ");
                        do{
                            ch = read.next().charAt(0);
                        }while (ch !='S' && ch  != 's' && ch !='N' && ch  != 'n');
                        visto = false;

                        if (ch == 'S' || ch == 's') {
                            visto = true;
                        }

                        Empresaentrega emp = new Empresaentrega(mail, nome, pass, new Coordenadas(x, y),nif, true, taxa, cap, vel, 0, 0, raio,0,visto , new ArrayList<>());
                        t.addUser(emp);
                        break;

                    case 4:
                        System.out.println("Diga o tempo de atendimento esperado: ");
                        do{
                            linha = read.nextLine();
                        }while (!Menu.isNumeric(linha) || linha.length() < 1);
                        double tempo = Double.parseDouble(linha);
                        Loja l = new Loja(nome, mail, pass, new Coordenadas(x, y), new ArrayList<>(), tempo);
                        t.addUser(l);
                        break;

                    default:
                        System.out.println("Opcao Invalida");
                }
            }
        }while(opcao != 0);
    }

    public static void menu_cliente(TrazAqui t,User u,Menu m){
        String linha;
        Scanner read = new Scanner(System.in);
        DateTimeFormatter time = DateTimeFormatter.ofPattern("yyyy/MM/dd");
        int opcao;

        do {
            do {
                m.show_menu_Cliente();
                System.out.println("Escolha uma opção:");
                linha = read.nextLine();
                }while (!Menu.isNumeric(linha) || linha.length() < 1);

                opcao = Integer.parseInt(linha);
                switch (opcao) {
                    case 1:
                        String cod;
                        do{
                            System.out.println("Escreva um codigo para a encomenda(e + digitos): ");
                            linha = read.nextLine();
                        }while(t.getencomendas().containsKey(linha) && linha.charAt(0) != 'e');
                        cod = linha;

                        t.show_Lojas();
                        String userloja;
                        User uti;
                        do{
                            System.out.println("Escreva um codigo da loja de onde quer encomendar: ");
                            linha = read.nextLine();
                            uti = t.getutilizadores().get(linha);
                        }while(!(uti instanceof Loja));
                        userloja = linha;
                        t.show_produtos();
                        List<Produto> lista = new ArrayList<>();
                        do {
                            System.out.println("Escreva um codigo do produto que quer encomendar('exit' para quando não quiser adicionar mais produtos): ");
                            linha = read.nextLine();
                            System.out.println("Insira a quantidade do produto: ");
                            int quantidade = read.nextInt();
                            String nada = read.nextLine();
                            Produto p = t.getProduto(linha);
                            if (p != null){
                                p.setquantidade(quantidade);
                                lista.add(p.clone());
                            }
                        }while(!linha.equals("exit"));
                        Loja l = (Loja) t.getutilizadores().get(userloja).clone();
                        Encomenda nova = new Encomenda(cod,u.getUsername(),userloja,0,LocalDate.now(),5,lista);
                        l.addEncomenda(nova);   //adiciona À fila de espera da loja

                        t.updateUser(l);
                        nova.setPreco(nova.calculaValorLinhaEnc());
                        Map<String,Encomenda> es = t.getencomendas();
                        es.put(nova.getReferencia(),nova.clone());
                        t.setencomendas(es);
                        break;
                    case 2:
                        do{
                            System.out.println("Escreva a data inicial ----> ano/mes/dia");
                            linha = read.nextLine();
                        }while(!isValid(linha));
                        LocalDate data1 = LocalDate.parse(linha,time);

                        do{
                            System.out.println("Escreva a data final ----> ano/mes/dia");
                            linha = read.nextLine();
                        }while(!isValid(linha) && data1.isBefore(LocalDate.parse(linha)));
                        LocalDate data2 = LocalDate.parse(linha,time);
                        if (t.util_Historico((Cliente) u,data1,data2) != null) {
                            for (Encomenda x : t.util_Historico((Cliente) u, data1, data2)) {
                                System.out.println("Encomenda: " + x.getReferencia());
                            }
                        }else{
                            System.out.println("Não tem encomendas nesse período de tempo.");
                        }

                        break;

                    case 3:
                        for(Encomenda x : ((Cliente) u).getHistorico()) {
                            System.out.print("Encomenda: " + x.getReferencia() + " -> ");
                            for (Produto p: x.getLista()){
                                System.out.print('\u250F' + " " + p.getdescricao()  +"\n"
                                                + "                    " + '\u2503' + " Preço" + " -> "+ p.getpreco() + "\n"
                                                + "                    " + '\u2517'+" Quantidade" + " -> "+ p.getquantidade() + "\n"
                                                + "                    ");
                            }
                            System.out.print("\n");
                        }
                       // System.out.println(((Cliente) u).getHistorico());
                        break;

                    case 4:
                        t.show_encomendas_pendentes(u.getUsername());
                        break;

                    case 5:
                        t.show_empresas();
                        System.out.println("Introduza o username da empresa que quer ver:");
                        String caridade = read.nextLine();
                        Empresaentrega e = t.getEmpresa(caridade);
                        while (e == null) {
                            System.out.println("Introduza um username válido: ");
                            caridade = read.nextLine();
                            e = t.getEmpresa(caridade);
                        }
                        System.out.println(e.toString());
                        break;

                    case 6:
                        t.show_voluntarios();
                        System.out.println("Introduza o username do voluntario que quer ver:");
                        caridade = read.nextLine();
                        Voluntario v = t.getVoluntario(caridade);
                        while (v == null) {
                            System.out.println("Introduza um username válido: ");
                            caridade = read.nextLine();
                            v = t.getVoluntario(caridade);
                        }
                        System.out.println(v.toString());
                        break;

                    case 7:
                        List<String> lista2 = t.get_classificoes_pendentes(u.getUsername());
                        if (lista2.size() != 0){
                            System.out.println("Tem as seguintes classificaçoes para dar.");
                            System.out.println(lista2.toString());
                            System.out.println("Escreva o username de quem quer classificar:");
                            do{
                                cod = read.nextLine();
                            }while(!lista2.contains(cod));

                            System.out.println("Escreva a classificacao que quer atribuir:");
                            String aux;
                            boolean isDouble;
                            do{
                                aux = read.nextLine();
                                isDouble = aux.chars().allMatch(Character::isDigit);
                            }while(!isDouble || aux.length() < 1);
                            double c = Double.parseDouble(aux);
                            t.classificar(cod,c);
                        }else{
                            System.out.println("Nao tem classificaçoes para dar.");
                        }
                        break;

                    case 8:
                        Cliente c = (Cliente) u;
                        double valor = 0;
                        Map<String,Encomenda> ep = c.getEncomendas_poraceitar();
                        if(ep.size() != 0) {
                            for (Map.Entry<String, Encomenda> entry : ep.entrySet()) {
                                Empresaentrega et = (Empresaentrega) t.getutilizadores().get(entry.getKey()).clone();
                                valor = t.Def_preco(et, entry.getValue().getReferencia());
                                System.out.println("Empresa: " + et.getUsername() + valor);
                            }

                            do {
                                System.out.println("Escreva o codigo da empresa que quer (ou 'exit' para nao aceitar nenhuma): ");
                                linha = read.nextLine();
                            } while (!c.getEncomendas_poraceitar().containsKey(linha) && !linha.equals("exit"));

                            if (c.getEncomendas_poraceitar().containsKey(linha)) {
                                t.aceita_transportadora(t,u,linha);
                            }
                        }
                        else{
                            System.out.println("Nao tem encomendas por aceitar.");
                        }
                        break;

                    case 0:
                        break;

                    default:
                        System.out.println("Escolha um opção válida");
                        break;
                }
        }while (opcao != 0);
        t.logout();
    }

    public static void menu_voluntario(TrazAqui t, User u, Menu m){
        String linha;
        Scanner read = new Scanner(System.in);
        int opcao = 0;

        do {
            do {
                m.show_menu_Voluntario();
                System.out.println("Escolha uma opção:");
                linha = read.nextLine();
            }while (!Menu.isNumeric(linha) || linha.length() < 1);

            opcao = Integer.parseInt(linha);
            switch (opcao) {
                case 1:
                    t.altera_disp(u);
                    System.out.println("Disponibilidade alterada.");
                    break;

                case 2:
                    t.altera_visto(u) ;
                    System.out.println("Visto alterado.");
                    break;

                case 3:
                    t.mostra_Enc_pendentes();
                    break;

                case 4:
                    t.mostra_Enc_pendentes();
                    String cod;
                    Encomenda e;
                    do{
                        System.out.println("Escreva o codigo da encomenda que quer entregar: ");
                        cod = read.nextLine();
                        e = t.getEncomenda(linha);
                    }while(e == null);
                    t.aceitaEncomenda((Voluntario) u,cod);
                    break;

                case 0:
                    break;

                default:
                    System.out.println("Escolha um opção válida");
                    break;
            }
        }while (opcao != 0);
        t.logout();
    }

    public static void menu_EmpresaEntrega(TrazAqui t, User u, Menu m){
        String linha;
        Scanner read = new Scanner(System.in);
        DateTimeFormatter time = DateTimeFormatter.ofPattern("yyyy/MM/dd");
        int opcao;

        do {
            do {
                m.show_menu_EmpresaEntrega();
                System.out.println("Escolha uma opção:");
                linha = read.nextLine();
            }while (!Menu.isNumeric(linha) || linha.length() < 1);

            opcao = Integer.parseInt(linha);
            switch (opcao) {
                case 1:
                    t.altera_disp(u);
                    break;
                case 2:
                    t.altera_visto(u);
                    break;
                case 3:
                    if(t.show_encomendas(u).size()==0){
                        System.out.println("Não tem encomendas possíveis para determinar.");
                    }else {
                        t.show_encomendas(u);
                        do {
                            System.out.println("Escreva o codigo da encomenda que quer ver o preço: ");
                            linha = read.nextLine();
                        } while (t.getencomendas().containsKey(linha));
                        if (t.Def_preco((Empresaentrega) u,linha) !=0) {
                            System.out.printf("O preço da encomenda %s é: %.2f", linha, t.Def_preco((Empresaentrega) u, linha));
                        }else {
                            System.out.println("Está fora do alcance da transportadora.");
                        }
                    }
                    break;

                case 4:
                    do{
                        System.out.println("Escreva a data inicial ----> ano/mes/dia");
                        linha = read.nextLine();
                    }while(!isValid(linha));
                    LocalDate data1 = LocalDate.parse(linha,time);

                    do{
                        System.out.println("Escreva a data final ----> ano/mes/dia");
                        linha = read.nextLine();
                    }while(!isValid(linha) && data1.isBefore(LocalDate.parse(linha)));
                    LocalDate data2 = LocalDate.parse(linha,time);
                    if(t.total_faturado((Empresaentrega) u , data1, data2)==0) {
                        System.out.printf("O total faturado pela sua empresa no período de tempo especificado foi: %.2f\n", t.total_faturado((Empresaentrega) u, data1, data2));
                    }else{
                        System.out.println("Não faturou nada pois nao foram feitas entregas durante esse período de tempo.");
                    }
                    break;

                case 5:
                    t.mostra_Enc_pendentes();
                    Encomenda e;
                    do{
                        System.out.println("Escreva o codigo da encomenda que quer entregar (ou 'exit' para nao entregar nada): ");
                        linha = read.nextLine();
                        e = t.getEncomenda(linha);
                    }while(e == null && !linha.equals("exit"));
                    int pode_entregar = 0;
                    if(e != null){
                        pode_entregar =t.aceitaenc_empresa(t,u,e);
                    }
                    if (pode_entregar == 1){
                        System.out.println("A entrega está agora à espera de ser aceite pelo cliente....");
                    }

                    break;

                case 0:
                    break;

                default:
                    System.out.println("Escolha um opção válida");
                    break;
            }
        }while (opcao != 0);
        t.logout();
    }



    public static void menu_Loja(TrazAqui t, User u,Menu m) {
        String linha;
        Scanner read = new Scanner(System.in);
        int opcao;
        do {
            do {
                m.show_menu_loja();
                System.out.println("Escolha uma opção:");
                linha = read.nextLine();
            } while (!Menu.isNumeric(linha) || linha.length() < 1);

            opcao = Integer.parseInt(linha);
            switch (opcao) {
                case 1:
                    System.out.println("Tempo de atendimento atual " + ((Loja) u).getTempoAtendimento());
                    System.out.println("Insira o novo tempo de atendimento:");
                    double temp = read.nextDouble();
                    t.altera_atend(u,temp);
                    break;
                case 2:
                    Loja l = (Loja) u;
                    List<Encomenda> espera = l.getFilaespera();
                    if (espera.size() == 0){
                        System.out.println("Nao tem encomendas na fila de espera.");
                    }
                    else {
                        for (Encomenda e : espera) {
                            System.out.println("Encomenda: " + e.getReferencia() + " do Cliente " + e.getReferenciaUti());
                        }
                    }
                    break;

                default:
                    System.out.println("Escolha inválida");
                    break;
            }
        }while (opcao!=0);
    }

    public static boolean isValid(String dateStr) {
        DateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");
        sdf.setLenient(false);
        try {
            sdf.parse(dateStr);
        } catch (ParseException e) {
            return false;
        }
        return true;
    }
}