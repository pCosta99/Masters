import java.util.*;
import static java.lang.System.in;
import static java.lang.System.out;

public class Menu {
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_RESET = "\u001B[0m";
    //Variáveis de Instancia
    Scanner ler;

    String top = new String(ANSI_YELLOW +"############# !!! Titulo !!! #############" + ANSI_RESET);
    public void bottom(String top){
        int i=0;
        while (i<top.length()-10){out.print(ANSI_YELLOW +"#");
        i++;}
        out.println("#"+ ANSI_RESET);}

    public Menu(){
        ler=new Scanner(in);
    }

    public String lerString(String s) {
        out.println(s);
        String str=ler.nextLine();
        while (str.isEmpty()) str=ler.nextLine();
        return str;
    }

    public StringBuffer lerStringBuffer(String s) {
        out.println(s);
        StringBuffer str = new StringBuffer();
        str.append(ler.next());
        return str;
    }

    public int lerInt(String s){
        out.println(s);
        return ler.nextInt();
    }

    public double lerDouble(String s){
        out.println(s);
        return ler.nextDouble();
    }

    public boolean lerBoolean(String s){
        String str = lerString(s);
        if(str.startsWith("S") || str.equals("s")) str="true";
        else str = "false";
        return Boolean.parseBoolean(str);
    }

    //Menu Inicial - é aqui onde o utilizador pode fazer Login, Registar uma conta e terminatar a aplicação
    public int InicioApp(){
        out.println(top.replace("Titulo", "Traz Aqui"));
        out.println("1) Autenticação");
        out.println("2) Registar");
        out.println("0) Sair");
        String bottom = top.replace("Titulo", "Traz Aqui");
        bottom(bottom);
        int e = ler.nextInt();
        return e;
    }

    //Menu Inicial - onde o utilizador entra nos vários serviços
    public int menuInicial(){
        out.println(top.replace("Titulo", "Menu Principal"));
        out.println("1) Loja");
        out.println("2) Transportador");
        out.println("3) Voluntário");
        out.println("4) Cliente");
        out.println("0) Voltar ao Ecrã Inicial");
        String bottom = top.replace("Titulo", "Menu Principal");
        bottom(bottom);
        int e = -1;
        while (e<0 || e>4) { e = ler.nextInt(); }
        return e;
    }

    //Ecrãs de Login nas contas dos respectivos serviços
    public String LoginL(){
        out.println(top.replace("Titulo", "Loja"));
        out.println("\nInsira os dados pedidos para confirmar a sua Loja.\nSe quiser retroceder escreva: <b.\n");
        String SLN;     //Shop Login Name
        out.println("Insira o seu nome:");
        SLN = ler.nextLine();
        while (SLN.isEmpty()){SLN = ler.nextLine();}
        if (SLN.equals("<b")) {
            return "<b";     //este valor faz com que o Controller retroceda o programa para o menu inicial
        }
        String SLC;     //Shop Login Code
        out.println("\nInsira o seu códdigo:");
        SLC = ler.nextLine();
        while (SLC.isEmpty()){SLC = ler.nextLine();}
        if (SLC.equals("<b")) {     //elimina o email insirido pelo utilizador
            return "<b";     //este valor faz com que o Controller retroceda o programa para o menu inicial
        }
        return SLN + "#" + SLC;
    }

    public String LoginT(){
        out.println(top.replace("Titulo", "Transportadora"));
        out.println("\nInsira os dados pedidos para confirmar a sua Empresa.\nSe quiser retroceder escreva: <b.\n");
        String SLN;
        out.println("Insira o seu nome:");
        SLN = ler.nextLine();
        while (SLN.isEmpty()){SLN = ler.nextLine();}
        if (SLN.equals("<b")) {
            return "<b";     //este valor faz com que o Controller retroceda o programa para o menu inicial
        }
        String SLC;
        out.println("\nInsira o seu códdigo:");
        SLC = ler.nextLine();
        while (SLC.isEmpty()){SLC = ler.nextLine();}
        if (SLC.equals("<b")) {     //elimina o email insirido pelo utilizador
            return "<b";     //este valor faz com que o Controller retroceda o programa para o menu inicial
        }
        return SLN + "#" + SLC;
    }

    public String LoginV(){
        out.println(top.replace("Titulo", "Voluntário"));
        out.println("\nInsira os dados pedidos para confirmar a sua conta de Voluntário.\nSe quiser retroceder escreva: <b.\n");
        String SLN;     //Shop Login Name
        out.println("Insira o seu nome:");
        SLN = ler.nextLine();
        while (SLN.isEmpty()){SLN = ler.nextLine();}
        if (SLN.equals("<b")) {
            return "<b";     //este valor faz com que o Controller retroceda o programa para o menu inicial
        }
        String SLC;     //Shop Login Code
        out.println("\nInsira o seu códdigo:");
        SLC = ler.nextLine();
        while (SLC.isEmpty()){SLC = ler.nextLine();}
        if (SLC.equals("<b")) {
            //elimina o email insirido pelo utilizador
            return "<b";     //este valor faz com que o Controller retroceda o programa para o menu inicial
        }
        return SLN + "#" + SLC;
    }

    public String LoginC(){
        out.println(top.replace("Titulo", "Cliente"));
        out.println("\nInsira os dados pedidos para confirmar a sua Conta de Utilizador.\nSe quiser retroceder escreva: <b.\n");
        String SLN;     //Shop Login Name
        out.println("Insira o seu nickname:");
        SLN = ler.nextLine();
        while (SLN.isEmpty()){SLN = ler.nextLine();}
        if (SLN.equals("<b")) {
            return "<b";     //este valor faz com que o Controller retroceda o programa para o menu inicial
        }
        String SLC;     //Shop Login Code
        out.println("\nInsira o seu código:");
        SLC = ler.nextLine();
        while (SLC.isEmpty()){SLC = ler.nextLine();}
        if (SLC.equals("<b")) {     //elimina o email insirido pelo utilizador
            return "<b";     //este valor faz com que o Controller retroceda o programa para o menu inicial
        }
        return SLN + "#" + SLC;
    }

    //Registo de contas
    public int menuRegistoGeral() {
        out.println(top.replace("Titulo", "Registo"));
        out.println("1) Loja");
        out.println("2) Transportador");
        out.println("3) Voluntário");
        out.println("4) Cliente");
        out.println("0) Voltar ao Menu Principal");
        String bottom = top.replace("Titulo", "Registo");
        bottom(bottom);

        String e = ler.nextLine();
        while (e.isEmpty()){
            e = ler.nextLine();
        }
        int r = Integer.parseInt(e);
        return r;
    }

    public String RegL(){
        String str;    //strings a devolver

        String nome, proDis,avgT, queue, coor;

        out.print("\nIndique os dados pedidos.\nSe quiser retroceder escreva: <b.\n");

        nome = lerString("\nNome da Loja:");
        if (nome.equals("<b")) {
            return "<b";
        }

        queue = lerString("\nTamanho da fila de espera (0->Nenhum; 1->Pouco; 2-> Médio; 3->Elevado):");
        if (queue.equals("<b")) {
            return "<b";
        }

        avgT = lerString("\nTempo de Espera Médio (em minutos):");
        if (avgT.equals("<b")) {
            return "<b";
        }

        coor = lerString("\nCoordenadas da Loja, em Latitude e Longitude - no formato x,y (ex.:90,50):");
        if (coor.equals("<b")) {
            return "<b";
        }

        proDis = lerString("\nLista de produtos disponiveis para compra (no seguinte formato - papel higiénico-peso-preço,desinfetante-peso-preço):");
        if (proDis.equals("<b")) {
            return "<b";
        }

        str = nome +  "," + queue + "," + avgT + "," + coor + "," + proDis ;
        return str;
    }

    public String RegT(){
        String str;

        String nome, cem, disp, tipo;

        String coor, raio, velo, preço, NIF, numE;

        out.print("\nIndique os dados pedidos.\nSe quiser retroceder escreva: <b.\n");

        nome = lerString("\nNome da Empresa:");
        if (nome.equals("<b")) {
            return "<b";
        }

        coor = lerString("\nCoordenadas da sua Transportadora, em Latitude e Longitude (no formato x,y - ex.:90,50):");
        if (coor.equals("<b")) {
            return "<b";
        }

        raio = lerString("\nRaio de Ação, em km (tamanho da área que os seus serviços podem ''cobrir''):");
        if (raio.equals("<b")) {
            return "<b";
        }

        disp = lerString("\nDisponibilidade (se a empresa encontra-se disponível para realizar qualquer serviços - basta um sim ou não):");
        if (disp.equals("<b")) {
            return "<b";
        }
        else{
            if(disp.equals("Sim") || disp.equals("sim")) disp="true";
            else disp = "false";
        }

        tipo = lerString("\nDiga qual o tipo de entregas que o seu serviço efetua ( 1-> Comida; 2->Eletrodomésticos; 3->Outros):");
        if (tipo.equals("<b")) {
            return "<b";
        }
        velo = lerString("\nVelocidade Média:");
        if (velo.equals("<b")) {
            return "<b";
        }

        cem = lerString("\nO seus seviços tem certificado de entrega médica? Sim ou Não:");
        if (cem.equals("<b")) {
            return "<b";
        }
        else{
            if(cem.equals("Sim") || cem.equals("sim")) cem="true";
            else cem = "false";
        }

        preço = lerString("\nQuanto custa (em euros) os seus seviços?");
        if (preço.equals("<b")) {
            return "<b";
        }

        NIF = lerString("Por favor introduza o seu Número de Identificação Fiscal, ou NIF (9 dígitos):\n");
        if (NIF.equals("<b")) {
            return "<b";
        }
        numE = lerString("Finalmente, qual o número de ''estafetas'' que a sua empresa atualmente emprega?");
        if (numE.equals("<b")) {
            return "<b";
        }
        str = nome + "," + coor + "," + raio + "," + disp + "," + tipo + "," + velo + "," + cem + "," + preço + "," + NIF + "," + numE;
        return str;
    }

    public String RegV(){
        String str;

        String nome, cem, disp, tipo;

        String coor, raio, velo;

        out.print("\nIndique os dados pedidos.\nSe quiser retroceder escreva: <b.\n");

        nome = lerString("\nNome:");
        if (nome.equals("<b")) {
            return "<b";
        }

        coor = lerString("\nAs suas coordenadas, em Latitude e Longitude (no formato x,y - ex.:90,50):");
        if (coor.equals("<b")) {
            return "<b";
        }

        raio = lerString("\nRaio de Ação, em km (tamanho da área que os seus serviços podem ''cobrir''):");
        if (raio.equals("<b")) {
            return "<b";
        }

        disp = lerString("\nDisponibilidade (se te encontras disponível para realizar qualquer serviços - basta um sim ou não):");
        if (disp.equals("<b")) {
            return "<b";
        }
        else{
            if(disp.equals("Sim") || disp.equals("sim")) disp="true";
            else disp = "false";
        }

        tipo = lerString("\nDiga qual o tipo de entregas que o seu serviço efetua ( 1-> Comida; 2->Eletrodomésticos; 3->Outros):");
        if (tipo.equals("<b")) {
            return "<b";
        }

        velo = lerString("\nVelocidade Média:");
        if (velo.equals("<b")) {
            return "<b";
        }

        cem = lerString("\nO seus seviços tem certificado de entrega médica? Sim ou Não:");
        if (cem.equals("<b")) {
            return "<b";
        }
        else{
            if(cem.equals("Sim") || cem.equals("sim")) cem="true";
            else cem = "false";
        }

        str = nome + "," + coor + "," + raio + "," + disp + "," + tipo + "," + velo + "," + cem;
        return str;
    }

    public String RegC(){
        String str;

        String nome, coor;

        out.print("\nIndique os dados pedidos.\nSe quiser retroceder escreva: <b.\n");

        nome = lerString("\nNickname:");
        if (nome.equals("<b")) {
            return "<b";
        }

        coor = lerString("\nAs suas coordenadas, em Latitude e Longitude (no formato x,y - ex.:90,50):");
        if (coor.equals("<b")) {
            return "<b";
        }

        str = nome + ","  + coor;
        return str;
    }

    //Menus do User.
    public int menuGeralUser(String nome){
        int choice=-1;
        out.println(top.replace("Titulo", nome));
        out.println("1) Efetuar Encomenda");
        out.println("2) Ver Encomendas Passadas.");
        out.println("3) 10 Empresas que mais utilizam o sistema.");
        out.println("4) 10 Utilizadores que mais utilizam o sistema.");
        out.println("5) Classificar Entregador.");
        out.println("0) Log-Out.");
        String bottom = top.replace("Titulo", nome);
        bottom(bottom);
        while (choice<0 || choice>5) { choice = ler.nextInt(); }
        return choice;
    }

    public String showStores(List<Store> lojas){
        String code="codigo da loja selecionada";
        int i=0;
        Map<Integer,Store> integerStoreMap = new HashMap<>();
        out.println(top.replace("Titulo", "Lojas Disponiveis"));
        out.println("Selecione Loja pretendida. Para Retroceder escreva \"<b\" ");
        out.println("Nome");
        for(Store l : lojas){
            integerStoreMap.put(i,l.clone());
            out.println(i + ") " + l.getName());
            i++;
        }
        String bottom = top.replace("Titulo", "Lojas Disponiveis");
        bottom(bottom);
        code=ler.next();
        if(!code.equals("<b")){ code = integerStoreMap.get(Integer.parseInt(code)).getCod();}
        return code;
    }

    public List<String> showProducts(Set<Product> produtos){
      List<Product> productList = List.copyOf(produtos);
      List<String> pos = new ArrayList<>();
      int i=0; boolean flag =true; int j=0;
      while (flag){
          j+=10;
          out.println(top.replace("Titulo", "Produtos Disponiveis"));
          for(;i<j && i<productList.size();i++){
              Product p = productList.get(i);
              out.println(i+") " + p.getName() + " - " + p.getPrice());
          }

          try {
              if(productList.get(i)!=null) out.println(i+") " +"Proxima Pagina.");
          }catch (Exception e){out.println(i+") " +"Primeira pagina");}
          String bottom = top.replace("Titulo", "Produtos Disponiveis");
          bottom(bottom);
          String[] code =ler.next().split(",");
          pos.addAll(Arrays.asList(code));
          if(i!=productList.size()){
              if(pos.contains(String.valueOf(i))){
                  pos.remove(String.valueOf(i));}
              else{flag=false;}
          }
          else if(pos.contains(String.valueOf(i))){pos.remove(String.valueOf(i)); i=0;j=0;}
          if(pos.remove("<b")){flag=false;}
      }
        List<String> codigo = new ArrayList<>();
      for(String p : pos){
          codigo.add(productList.get(Integer.parseInt(p)).getCode());
      }
      out.println(codigo.toString());
      return codigo;
    }


    public void showPastOrders(Set<Order> orders, Map<String,Store> stores){
        out.println(top.replace("Titulo", "Encomendas Anteriores"));
        out.println("Codigo - Nome da Loja - Peso - Preço - Preço de Transporte - {Produtos Por Nome}");
        if(orders.isEmpty()) out.println("Não existem Encomendas a apresentar.");
        else for(Order l : orders){
            out.println(l.getCode() + " - " + stores.get(l.getSellerCode()).getName() + " - " + l.getOrderPrice()+ " - "
            +l.getDeliveryPrice() );
        }
        String bottom = top.replace("Titulo", "Encomendas Anteriores");
        bottom(bottom);
    }

    public void showTop10Transporters(Set<Transporter> transporterSet){
        out.println(top.replace("Titulo", "Top10Empresas"));
        if(transporterSet.isEmpty()) out.println("Não existem informação a apresentar.");
        else transporterSet.forEach(e->out.println("Codigo:" + e.getCode() + " Nome:" + e.getName()));
        String bottom = top.replace("Titulo", "Top10Empresas");
        bottom(bottom);
    }

    public void showTop10Users(Set<User> userSet){
        out.println(top.replace("Titulo", "Top10Utilizadores"));
        if(userSet.isEmpty()) out.println("Não existem informação a apresentar.");
        else userSet.forEach(e->out.println("Codigo:" + e.getCod() + " Nome:" + e.getName()));
        String bottom = top.replace("Titulo", "Top10Utilizadores");
        bottom(bottom);
    }

    //Menus da Loja
    public int menuGeralLoja(String nome){
        int choice=-1;
        out.println(top.replace("Titulo", nome));
        out.println("1) Alterar Fila de Espera.");
        out.println("2) Alterar Tempo Médio de Espera.");
        out.println("3) Processar Encomendas.");
        out.println("4) Adicionar Produto(s).");
        out.println("5) Remover Produto(s).");
        out.println("6) Ver Encomendas Passadas.");
        out.println("7) 10 Empresas que mais utilizam o sistema.");
        out.println("8) 10 Utilizadores que mais utilizam o sistema.");
        out.println("0) Log-Out.");
        String bottom = top.replace("Titulo", nome);
        bottom(bottom);
        while (choice<0 || choice>8) { choice = ler.nextInt(); }

        return choice;
    }

    public List<String> showOrders(Set<Order> ordersToSetReady,String title){
        List<String> list = new ArrayList<>();
        out.println(top.replace("Titulo", title));
        out.println("Selecione as Encomendas pretendidos indicando os seus códigos separados por virgulas. Para Retroceder escreva \"<b\" ");
        for(Order order : ordersToSetReady){out.println("Codigo: " + order.getCode() + order.getProducts());}
        String bottom = top.replace("Titulo", title);
        bottom(bottom);
        String[] codigos = ler.next().split(","); int i =0;
        while (i<codigos.length){list.add(codigos[i]); i++;}
        return list;
    }

    public String showOrdersString(Set<Order> ordersToSetReady,String title){
        List<String> list = new ArrayList<>();
        out.println(top.replace("Titulo", title));
        out.println("Selecione a Encomenda pretendida indicando o seu códigos. Para Retroceder escreva \"<b\" ");
        for(Order order : ordersToSetReady){out.println("Codigo: " + order.getCode());}
        String bottom = top.replace("Titulo", title);
        bottom(bottom);
        return ler.next();
    }


    //Menus Voluntario
    public int menuGeralVoluntario(String nome){
        int choice=-1;
        out.println(top.replace("Titulo", nome));
        out.println("1) Alterar Disponibilidade.");
        out.println("2) Registar Encomenda como Efetuada.");
        out.println("3) Ver minha classificação");
        out.println("4) Ver Encomendas Passadas.");
        out.println("5) 10 Empresas que mais utilizam o sistema.");
        out.println("6) 10 Utilizadores que mais utilizam o sistema.");
        out.println("0) Log-Out.");
        String bottom = top.replace("Titulo", nome);
        bottom(bottom);
        while (choice<0 || choice>6) { choice = ler.nextInt(); }

        return choice;
    }

    public int menuGeralEmpresa(String nome){
        int choice=-1;
        out.println(top.replace("Titulo", nome));
        out.println("1) Alterar Disponibilidade.");
        out.println("2) Atualizar numero de estafetas.");
        out.println("3) Registar Encomenda como Efetuada.");
        out.println("4) Ver minha classificação");
        out.println("5) Ver Encomendas Passadas.");
        out.println("6) 10 Empresas que mais utilizam o sistema.");
        out.println("7) 10 Utilizadores que mais utilizam o sistema.");
        out.println("8) Total Faturado.");
        out.println("0) Log-Out.");
        String bottom = top.replace("Titulo", nome);
        bottom(bottom);
        while (choice<0 || choice>8) { choice = ler.nextInt(); }

        return choice;
    }




}
