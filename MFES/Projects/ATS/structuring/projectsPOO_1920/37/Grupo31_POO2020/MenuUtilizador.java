import java.util.Scanner;
import java.util.Random;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.io.*;

public class MenuUtilizador extends Menu implements Serializable{
    // apenas um scanner para todas as operações
    static final Scanner in = new Scanner(System.in);

    // variável privada para o código de utilizador. É usado quando é efectuado um login para guardar o utilizador
    private String codUtilizador;

    // variável privada para ter o código de quem irá distribuir a encomenda
    private String distribuidor;

    // variável para ter o custo total (transporte + da encomenda)
    private double custoTotal = 0.0;

    // construtor vazio
    public MenuUtilizador(){}
    
    // menu inicial
    public void menuInicial(GestaoTotal gt){
        String s;
        System.out.println("--------------------------------------");
        System.out.println("Escolha uma das seguintes opções:");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Registo");
        System.out.println("\n[2] Login");
        System.out.println("\n[3] Voltar Menu Inicial");
        
        
        s = in.nextLine();
    
        switch(s){
            case "0": System.exit(0);
            case "1": registo(gt); break;
            case "2": int i = login(gt);
            
            // switch para o login
                switch(i){
                    case 0: System.out.println("Inválido."); break;
                    case 1: acoesUtilizador(gt); break;
                    default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); break;
                }
                break;
            //
            case "3" : super.menuInicial(gt); break;
            
            
            
        
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); break;
        }

        this.menuInicial(gt);

    }
    
  

    // Fazer Login
    public int login(GestaoTotal gt) {
        int i[] = {0};
        // String email
        String e;
        System.out.println("\n >Insira aqui o seu email: ");
        e = in.nextLine();
        
        // string password
        String p;
        System.out.println("\n >Insira aqui a sua password :");
        p = in.nextLine();

        gt.getGU().getUtilizadores().values().stream()
            .forEach(u -> {
                            if ((u.getUser()).equals(e) && u.getPass().equals(p)) {
                                    i[0] = 1; 
                                    // altera a variável codUtilizador para o codigo respetivo do email
                                    codUtilizador = u.getCodU();
                                }
                            }
              );
        return i[0];
    }


    // registar um Utilizador
    public void registo(GestaoTotal gt) {
        System.out.println("-----------Registe-se aqui.--------------");
        String s;
        System.out.println("\n >Insira aqui o seu email:");
        s = in.nextLine();
        while(s.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            s = in.nextLine();
        }
        
        
        while (gt.existeEmail(s)){
            System.out.println("\n !!!! O email que inseriu já existe, coloque outro:");
            s = in.nextLine();
            while(s.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            s = in.nextLine();
            }
          }
        
       
        String q;
        System.out.println("\n >Insira aqui a sua password:");
        q = in.nextLine();
        while(q.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            q = in.nextLine();
        }
        
        
        String d;
        System.out.println("\n >Insira aqui o seu código de Utilizador :");
        d = in.nextLine();
        while(d.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            d = in.nextLine();
        }
        
        while (gt.existeUtilizador(d)){
            System.out.println("\n !!!! O codigo de utilizador que inseriu já existe, coloque outro:");
            d = in.nextLine();
            while(d.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            d = in.nextLine();
            }
          }
        
        
        String v;
        System.out.println("\n >Insira aqui o seu Nome (1º e último):");
        v = in.nextLine();
        while(v.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            v = in.nextLine();
        }
        
        
        // coordenadas
        String x,y;
        double gpsx = 0.0;
        double gpsy = 0.0;


        // latitude
        System.out.println("\n >Insira aqui as suas coordenadas(Latitude) :");
        x = in.nextLine();
        while(x.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            x = in.nextLine();
        }
        // longitude
        System.out.println("\n >Insira aqui as suas coordenadas(Longitude) :");
        y = in.nextLine();
        while(y.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            y = in.nextLine();
        }
        // tenta fazer o parse.
        do{
            try{
                gpsx = Double.parseDouble(x);
                gpsy = Double.parseDouble(y);

                // cria uma localização se tudo estiver ok
                Localizacao local = new Localizacao(gpsx, gpsy);
          
                gt.adicionaUtilizador(new Utilizador(s,q,d,local,v));
                System.out.println("\n  O seu registo foi efetuado.\n");
            }
            catch(NumberFormatException e){
                System.out.println("!!!! Input inválido. Tente novamente."); registo(gt);
                break;
            }
        }
        while(false);

    }

    // acoes de um utilizador
    public void acoesUtilizador(GestaoTotal gt){
        String s;
        System.out.println("-------------Bem Vindo----------------");
        System.out.println("\nEscolha uma das seguintes opções:");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Fazer uma encomenda");
        System.out.println("\n[2] Histórico de Encomendas");
        System.out.println("\n[3] Classificar uma entrega");
        System.out.println("\n[4] Terminar Sessão");
        s = in.nextLine();
        switch(s){
            case "0": System.exit(0);
            case "1": fazerEncomenda(gt); break;
            case "2": System.out.println(gt.getGE().printEncUtilizador(codUtilizador)); acoesUtilizador(gt); break;
            case "3": this.classificarEntrega(gt); acoesUtilizador(gt); break;
            case "4": this.menuInicial(gt); break;
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); acoesUtilizador(gt); break;
        }
    }

    // escolha de um tipo de encomenda
    public void fazerEncomenda(GestaoTotal gt){
        String s;
        System.out.println("\nEscolha uma das seguintes opções:");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Voltar atrás.");
        System.out.println("\n[2] Encomenda Normal");
        System.out.println("\n[3] Encomenda Médica");
        s = in.nextLine();
        //
        switch(s){
            case "0": System.exit(0);
            case "1": acoesUtilizador(gt); break;
            case "2": // passa ao seguinte
            case "3": escolheTransporte(gt,s); gerarEncomenda(gt,s); break;
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); acoesUtilizador(gt); break;
        }
    }
    
    // escolha de um transporte para a encomenda
    public void escolheTransporte(GestaoTotal gt, String t){
        String s;
        System.out.println("--------------------------------------");
        System.out.println("\nDeseja que a encomenda seja transportada por um voluntário ou por uma empresa transportadora?:");
        System.out.println("\n[1] Voluntário");
        System.out.println("\n --info-- Não terá custos adicionais");
        System.out.println("\n[2] Empresa Transportadora");
        System.out.println("\n--info-- Terá uma taxa definida por cada empresa");
        System.out.println("\n[0] Voltar atrás");
        s = in.nextLine();
        switch(s){
            case "0": fazerEncomenda(gt); break;
                        
            case "1": 
                    if(encontraVoluntario(gt,t).equals("Não foi encontrado nenhum voluntário")){
                        System.out.println("Por favor selecione uma empresa transportadora ou tente mais tarde.");
                        escolheTransporte(gt,t);
                    }
                    else{
                        System.out.println("O seu voluntário é : " + encontraVoluntario(gt,t));
                        distribuidor = encontraVoluntario(gt,t);
                    }
                    break;
            case "2": 
                if(encontraEmpresa(gt,t).equals("Não foi encontrado nenhuma empresa transportadora no seu raio")){
                    System.out.println("Não existe nenhuma empresa transportadora no seu raio. Por favor tente mais tarde.");
                    escolheTransporte(gt,t);
                }
                else{
                    System.out.println(("\nO custo de transporte da empresa ")+ encontraEmpresa(gt,t) + " é " + gt.custoTransporteEmp(codUtilizador, encontraEmpresa(gt,t)));
                    String v;
                    System.out.println("Deseja continuar ou escolher voluntário?");
                    System.out.println("\n[1] Continuar");
                    System.out.println("\n[2] Voluntário");
                    v=in.nextLine();
                    switch(v){
                        case "1":
                            System.out.println("A sua empresa transportadora é : " + encontraEmpresa(gt,t));
                            // adiciona o valor do transporte ao custo total
                            custoTotal += gt.custoTransporteEmp(codUtilizador, encontraEmpresa(gt,t));
                            distribuidor = encontraEmpresa(gt,t);
                            break;
                            
                        case "2": 
                            if(encontraVoluntario(gt,t).equals("Não foi encontrado nenhum voluntário")){
                                System.out.println("Por favor selecione uma empresa transportadora ou tente mais tarde.");
                                escolheTransporte(gt,t);
                            }
                            else{
                                System.out.println("O seu voluntário é : " + encontraVoluntario(gt,t));
                                distribuidor = encontraVoluntario(gt,t);
                            }
                            break;
                        default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); escolheTransporte(gt,t); break;
                    }
                }
                break;
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); escolheTransporte(gt,t); break;
        }
    }

    // efectuar uma encomenda
    public void gerarEncomenda(GestaoTotal gt, String s){
        // código da loja
        String v;
        System.out.println("\n >Insira aqui o código da Loja onde pretende fazer a encomenda:");
        System.out.println("\n" + gt.getGL());
        v = in.nextLine();
        while (!gt.existeLoja(v)){
            System.out.println("\n >Essa loja não existe. Por favor escreva um código da Loja possível:");
            v = in.nextLine();
        }
        
        // gerar um código de encomenda
        String d;
        System.out.println("\n >O código da sua encomenda é :");
        Random r = new Random();
        d = "e"+r.nextInt(9999);
        while (gt.existeEncomenda(d)){
            d = "e"+r.nextInt(9999);
        }
        System.out.println(d);
        
        // data
        LocalDate t;
        t = LocalDate.now();
        
        // gera um valor aleatório para o peso
        Random gerador = new Random();
        double p = gerador.nextDouble()*10;
        
        // lista de produtos de uma encomenda
        ArrayList<LinhaEncomenda> n = new ArrayList<LinhaEncomenda>();
        fazerLinhaEncomenda(gt,n);

        // cria uma encomenda
        switch(s){
            case "2": 
                Encomenda encomendaN = new Encomenda(codUtilizador,d,v,t,p,n,null);
                custoTotal += encomendaN.calculaValorTotal();
                System.out.println("O custo total da encomenda (transporte + valor da encomenda) é: " +custoTotal);
                gt.adicionaEncomenda(encomendaN);
                gt.adicionaEncomendaAceite(d, distribuidor);
                custoTotal = 0.0;
                break;
            case "3": 
                Medicas encomendaM = new Medicas(codUtilizador,d,v,t,p,n,null);
                custoTotal += encomendaM.calculaValorTotal();
                System.out.println("O custo total da encomenda (transporte + valor da encomenda) é: " +custoTotal);
                gt.adicionaEncomenda(encomendaM);
                gt.adicionaEncomendaAceite(d, distribuidor);
                custoTotal = 0.0;
                break;
            default : System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); fazerEncomenda(gt); break;
        }

        // opção de adicionar outra encomenda
        System.out.println("\n >Deseja adicionar outra encomenda? :");
        System.out.println("\n >[1] Sim! ");
        System.out.println("\n >[2] Não! ");
        String outra;
        outra = in.nextLine();
        
        switch(outra){
            case "1": fazerEncomenda(gt); break;
            case "2": acoesUtilizador(gt); break;
            default: System.out.println("Opção inválida."); acoesUtilizador(gt);break;
        }   
    }
    
    // criar as linhas de encomendas de uma encomenda
    public void fazerLinhaEncomenda(GestaoTotal gt, ArrayList<LinhaEncomenda> n){
        String codP;
        System.out.println("\n >O código do seu produto é :");
        Random r = new Random();
        codP = "p"+r.nextInt(9999);

        // fazer um ciclo para ver se o produto existe nas linhas de encomenda do arraylist n
        for(int i = 0; i < n.size(); i++){
            // verifica pelo arraylist se existe o produto
            if (n.get(i).getCodProduto().equals(codP)){
                // se existe, avisa que já existe um código de produto na sua encomenda e reencaminha para introduzir outro produto
                fazerLinhaEncomenda(gt, n);
                break;
            }
        }
        
        System.out.println(codP);
        
        // descrição do produto
        String v;
        System.out.println("\n >Insira aqui a descrição do produto:");
        v = in.nextLine();
        while(v.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            v = in.nextLine();
        }
        
        // preco aleatório
        Random gerador = new Random();
        double preco = gerador.nextDouble()*10;
        
        // quantidade
        String  q;
        double quantidade = 0.0;
        System.out.println("\n >Insira aqui a quantidade que pretende encomendar");
        q = in.nextLine();
        while(q.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            q = in.nextLine();
        }

        // tenta fazer o parse.
        do{
            try{
                quantidade = Double.parseDouble(q);
                // taxa
                double i;
                i = 0.23;
        
                // desconto
                double desconto = 0;
        
                // adiciona o produto à lista de produtos comprados naquela encomenda
                n.add(new LinhaEncomenda (codP,v,preco,quantidade,i,desconto));
                System.out.println("\n !!!! O seu Produto foi adicionado com sucesso! :");
        
                // verifica se quer adicionar outro produto
                System.out.println("\n >Deseja adicionar outro produto? :");
                System.out.println("\n >[1] Sim! ");
                System.out.println("\n >[2] Não! ");
                String outra;
                outra = in.nextLine();
        
                switch(outra){
                    case "1": fazerLinhaEncomenda(gt,n); break;
                    case "2": break;
                    default: System.out.println("Opção inválida."); acoesUtilizador(gt);break;
                }
            }
            catch(NumberFormatException e){
                System.out.println("!!!! Input inválido. Tente novamente."); fazerLinhaEncomenda(gt,n);
                break;
            }
        }
        while(false);
    }

    // classificar uma entrega
    private void classificarEntrega(GestaoTotal gt) {
        String s;
        System.out.println("--------------------------------------");
        System.out.println("\nDeseja classificar:");
        System.out.println("\n[0] Voltar atrás");
        System.out.println("\n[1] Um Voluntário");
        System.out.println("\n[2] Uma Empresa Transportadora");
        s = in.nextLine();
        switch(s){
            case "0": acoesUtilizador(gt); break;
            case "1": 
            case "2":
                // double com a classifacao
                double cl;
                // string com o input da classificacao
                String classificacao;
                // string com o input do codigo da encomenda
                String codEncomenda;
                // string com o input do codigo da loja onde foi feita a encomenda
                String codLoja;
                //
                System.out.println("\nIndique o código da encomenda");
                codEncomenda = in.nextLine();
                while(codEncomenda.equals("")){
                    System.out.println("!!!! Input inválido. Tente novamente.");
                    codEncomenda = in.nextLine();
                }
                // 
                System.out.println("\nIndique o código da loja onde a encomenda foi efetuada");
                codLoja = in.nextLine();
                while(codLoja.equals("")){
                    System.out.println("!!!! Input inválido. Tente novamente.");
                    codLoja = in.nextLine();
                }
                // nova encomenda para ter os dados dela
                Encomenda nova = gt.getGE().getEncomenda(codEncomenda).clone();
                // 
                if(nova == null){
                    System.out.println("!!!! Input inválido. Tente novamente.");
                    classificarEntrega(gt);
                }
                System.out.println("\nIndique a classificação");
                classificacao = in.nextLine();
                try{
                    cl = Double.parseDouble(classificacao);
                    switch(s){
                        case "1":
                            if(!gt.existeVoluntario(nova.getdistribuidor())){
                                System.out.println("!!!! Input inválido. Tente novamente.");
                            }else{
                                gt.getGV().getVoluntario(nova.getdistribuidor()).classificar(cl);
                                System.out.println("Obrigado pela sua classificação!");
                            }
                            break;
                        case "2":
                            if(!gt.existeEmpresa(nova.getdistribuidor())){
                                System.out.println("Obrigado pela sua classificação!");
                            }else{
                                gt.getGEMP().getEmpresa(nova.getdistribuidor()).classificar(cl);
                            }
                            break;
                    }
                }
                catch(NumberFormatException e){
                    System.out.println("!!!! Input inválido. Tente novamente."); classificarEntrega(gt);
                    break;
                }
                break;
            default: System.out.println("!!!! Input inválido. Tente novamente."); classificarEntrega(gt); break;
        }
    }
    
    // dado um codigo de utilizador e um codigo de voluntario, verifica se eles se interferem
    public boolean verificaRaioVol(GestaoTotal gt, String codUtilizador,String codVolu){
        double raioV = gt.getGV().getVoluntarios().get(codVolu).getRaio();
        double lxV= gt.getGV().getVoluntarios().get(codVolu).getLocalizacao().getX();
        double lyV= gt.getGV().getVoluntarios().get(codVolu).getLocalizacao().getY();
        double lxU= gt.getGU().getUtilizadores().get(codUtilizador).getLocalizacao().getX();
        double lyU= gt.getGU().getUtilizadores().get(codUtilizador).getLocalizacao().getY();
        
        if((lxV+raioV)>= lxU && (lxV-raioV)<= lxU && (lyV+raioV)>= lyU && (lyV-raioV)<= lyU) {
            return true;
        }
        return false;
    }
    
    // dado o codigo de utilizador, encontra o primeiro voluntário no seu raio
    public String encontraVoluntario(GestaoTotal gt, String t){
        for(Voluntario v : gt.getGV().getVoluntarios().values()){
            // verifica o raio do voluntario
            if(verificaRaioVol(gt, codUtilizador,v.getCodV())){
                // caso a opção tenha sido "médicas", verifica se é certificado
                if(t.equals("2") || (t.equals("3") && v.getCertificado())){
                    return v.getCodV();
                }
            }
        }
        return "Não foi encontrado nenhum voluntário";
    } 

     // dado um codigo de utilizador e um codigo de empresa, verifica se eles se interferem
    public boolean verificaRaioEmpr(GestaoTotal gt, String codUtilizador,String codEmp){
        double raioE = gt.getGEMP().getEmpresasTransp().get(codEmp).getRaio();
        double lxE= gt.getGEMP().getEmpresasTransp().get(codEmp).getLocalizacao().getX();
        double lyE= gt.getGEMP().getEmpresasTransp().get(codEmp).getLocalizacao().getY();
        double lxU= gt.getGU().getUtilizadores().get(codUtilizador).getLocalizacao().getX();
        double lyU= gt.getGU().getUtilizadores().get(codUtilizador).getLocalizacao().getY();
        
        if((lxE+raioE)>= lxU && (lxE-raioE)<= lxU && (lyE+raioE)>= lyU && (lyE-raioE)<= lyU) {
            return true;
        }
        return false;
    }
    
    // dado o codigo de utilizador, encontra o primeiro voluntário no seu raio
    public String encontraEmpresa(GestaoTotal gt, String t){
        for(EmpresaTransportadora e : gt.getGEMP().getEmpresasTransp().values()){
            // verifica o raio do voluntario
            if(verificaRaioEmpr(gt, codUtilizador,e.getCodEmp())){
                // caso a opção tenha sido "médicas", verifica se é certificado
                if(t.equals("2") || (t.equals("3") && e.getCertificado())){
                    return e.getCodEmp();
                }
            }
        }
        return "Não foi encontrado nenhuma empresa transportadora no seu raio";
    }
}