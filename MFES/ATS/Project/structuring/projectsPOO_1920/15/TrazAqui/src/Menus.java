import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.*;

public class Menus{
    Scanner scan = new Scanner(System.in);

    public int menuRegisto(){
        System.out.println("-------------------------------");
        System.out.println("---------REGISTO----------");
        System.out.println("1 - Utilizador");
        System.out.println("2 - Voluntário");
        System.out.println("3 - Empresa");
        System.out.println("4 - Transportadora");
        System.out.println("5 - Loja");
        System.out.println("");
        System.out.println("0 - Sair");
        int a = scan.nextInt();
        return a;
    }


    // Menu Utilizador
    public void menuUtilizador(Utilizador mu, Parse p){
        int m;
        Scanner scan = new Scanner(System.in);
        do {
            System.out.println("******** Menu Utilizador ********");
            m = menuUtilizador();
            switch (m){
                case 1:
                    List<LinhaEncomenda> produtosSelecionados = new ArrayList<>();
                    Scanner read = new Scanner(System.in);
                    int linha = 0;
                    String y;
                    float quantidade = 0;

                    for (Loja l : p.getLojas().values()){
                        System.out.println(l.getCodUtilizador() + ": " +  l.getNome());
                    }
                    do {
                        System.out.println("Escolha a loja:");
                        y = read.nextLine();
                    }while(!p.getLojas().containsKey(y));
                    String s;
                    do {
                        System.out.println("Escolha um codigo para a encomenda: ");
                        s = read.nextLine();
                    }while(p.getEncomendas().containsKey(s));
                    String seleciona = "";
                    while (!seleciona.equals("exit")) {
                        for (LinhaEncomenda g : p.getProdutos()) {
                            System.out.print("[" + (g.getCodProduto()) + "]" + g.getDescricao());
                            int espaco = 30;
                            espaco = espaco - g.getDescricao().length();
                            for (int i = 0; i < espaco; i++) {
                                System.out.print(" ");
                            }
                            linha++;
                            if ((linha % 6) == 0) {
                                System.out.println();
                            }
                        }
                        System.out.println("0 - exit");
                        System.out.println("Escolha os produtos:");
                        seleciona = scan.nextLine();
                        if(!seleciona.equals("exit")){
                            if(p.contem(seleciona)){
                                System.out.println("Quantidade desejada:");
                                quantidade = scan.nextFloat();
                                String nada = scan.nextLine();
                                p.descProd(seleciona).setQuantidade(quantidade);
                                produtosSelecionados.add(p.descProd(seleciona));
                            }
                        }
                        linha = 0;
                    }
                    Encomenda enc = new Encomenda(s, mu.getCodUtilizador(), y, 0, produtosSelecionados);
                    Map<String, Encomenda> enc_efetuada = p.getEncomendas();
                    enc_efetuada.put(enc.getCodEncomenda(), enc.clone());
                    p.setEncomendas(enc_efetuada);
                    System.out.println("Encomenda efetuada.");
                    break;
                case 2:
                    List<Encomenda> aux1 = p.encPorAceitar(mu);
                    String empresa;
                    String encomenda;
                    if(aux1 == null){
                        System.out.println("Não tem encomendas por aceitar!");
                    }else{
                        for (Encomenda e : aux1){
                            System.out.println(e.getCodEncomenda());
                        }
                        do {
                            System.out.println("Escolha a encomenda: ");
                            encomenda = scan.nextLine();
                        }while (!aux1.contains(p.getEncomendas().get(encomenda)));
                        for (String u : p.getPendentes().get(encomenda)){
                            System.out.println(u + p.precoEnc(p.getTransportadoras().get(u), p.getEncomendas().get(encomenda)));
                        }do {
                            System.out.println("Escolha a transportadora que efetue a encomenda");
                            empresa = scan.nextLine();
                        }while (!p.getPendentes().get(encomenda).contains(empresa));
                    mu.adicionaEnc(p.getEncomendas().get(encomenda));
                    p.getTransportadoras().get(empresa).adicionaEnc(p.getEncomendas().get(encomenda));
                    Map<String, List<String>> tmp = new HashMap<>(p.getPendentes());
                    tmp.remove(encomenda);
                    p.setPendentes(tmp);
                    Map<String, Encomenda> aceites = new HashMap<>(p.getAceite());
                    aceites.put(encomenda,p.getEncomendas().get(encomenda));
                    p.setAceite(aceites);
                    }
                    break;
                case 3:
                    System.out.println("3 - Aceder a encomendas efetuadas");
                    p.encomendasEfetuadas(mu.getCodUtilizador());
                    break;
                case 4:
                    List<String> aux = p.classifica(mu);
                    if (aux.size() == 0){
                        System.out.println("Não tem encomendas por classificar\n");
                        break;
                    } else{
                        System.out.println("Escolha a Tranportadora ou Voluntário que deseja classificar ");
                        System.out.println(aux.toString());
                    }
                    String cod = scan.nextLine();
                    double classi;
                    do {
                        System.out.println("Classifique entre 1 e 10");
                        classi = scan.nextDouble();
                    } while (classi < 1 || classi > 10);
                    String nada = scan.nextLine();
                    if (cod.charAt(0) == 't'){
                        p.getTransportadoras().get(cod).atualiza_Class(classi);
                        p.encomendas_Classificadas(mu, cod);
                    } else{
                        p.getVoluntarios().get(cod).atualiza_Class(classi);
                        p.encomendas_Classificadas(mu, cod);
                    }
                    break;
                case 0:
                    System.out.println("SAIU!");
                    break;
                default:
                    System.out.println("Opcao errada");
            }
        } while (m != 0);
    }


    public void menuVoluntario(Voluntario mv, Parse p){
        int m;
        Scanner read = new Scanner(System.in);
        do {
            System.out.println("******** Menu Voluntário ********");
            m = menuVoluntario();
            switch (m){
                case 1:
                    p.alteraDisponibilidade_V(mv);
                    System.out.println("Alterou a sua disponibilidade para:\n" + mv.getDisponibilidade());
                    break;
                case 2:
                    System.out.println("Escolha a encomenda que pretende entregar ou exit se não quiser nenhuma:"); //falta caso n queira entregar
                    if(mv.getDisponibilidade() == true){
                        p.recolherEncomenda(mv);
                        String s;
                        s = read.nextLine();
                        p.encAceite(s);
                        Map<String, Encomenda> aceites = new HashMap<>(p.getAceite());
                        aceites.put(s,p.getEncomendas().get(s));
                        p.setAceite(aceites);
                        mv.adicionaEnc(p.getEncomendas().get(s));
                        p.getVoluntarios().get(s).adicionaEnc(p.getEncomendas().get(s));
                    }else System.out.println("Tem de alterar a disponibilidade para efetuar a encomenda");
                    break;
                case 3:
                    p.aceitoTransporteMedicamentos_V(mv);
                    System.out.println("Alterou o estado de transporte de medicamentos para:\n" + mv.getAceitaMedicamentos());
                    break;
                case 0:
                    System.out.println("SAIU!");
                    break;
                default:
                    System.out.println("Opcao errada");
            }
        }
        while (m!= 0);
    }

    public void menuTransportadora(Transportadora mt, Parse p) {
        Scanner scan = new Scanner(System.in);
        int m;
        do {
            System.out.println("******** Menu Transportadora ********");
            m = menuTransportadora();
            switch (m) {
                case 1:
                    p.alteraDisponibilidade_T(mt);
                    System.out.println("Alterou a sua disponibilidade para:\n" + mt.getDisponibilidade());
                    break;
                case 2:
                    LocalDateTime inicio = ler_data_inicial();
                    LocalDateTime fim = ler_data_final();
                    System.out.println(p.totalFaturado(mt,inicio,fim));
                    break;
                case 3:
                    System.out.println("Escolha a encomenda que pretende entregar:"); //falta caso n queira entregar
                    if(mt.getDisponibilidade() == true){
                        p.recolherEncomenda(mt);
                        String s;
                        s = scan.nextLine();
                        p.addPendentes(mt,s);
                    }else System.out.println("Tem de estar disponivel para efetuar entrega");
                    break;
                case 4:
                    p.aceitoTransporteMedicamentos_T(mt);
                    System.out.println("Alterou o estado de transporte de medicamentos para:\n" + mt.getAceitoTransporteMedicamentos());
                    break;
                case 0:
                    System.out.println("SAIU!");
                    break;
                default:
                    System.out.println("Opcao errada");
            }
        } while (m != 0);
    }


    public void menuLoja(Loja ml, Parse p){
        Scanner read = new Scanner(System.in);
        int m;
        do {
            System.out.println("******** Menu Loja ********");
            m = menuLoja();
            switch (m){
                case 1:
                    System.out.println("Ver histórico de encomendas");
                    p.lojaEncomendas(ml);
                    break;
                case 2:
                    System.out.println(p.defPeso(ml));
                    String i;
                    do {
                        System.out.println("Escreva o código da encomenda para definir o peso: ");
                        i = read.nextLine();
                    }while(!p.defPeso(ml).contains(i));
                    System.out.println("Defina o peso: ");
                    double defpeso;
                    defpeso = read.nextDouble();
                    String nada = read.nextLine();
                    p.alteraPeso(i, defpeso);
                    break;
                case 0:
                    System.out.println("SAIU!");
                    break;
                default:
                    System.out.println("Opcao errada");
            }
        } while (m != 0);
    }



    public void registoUtilizador(Parse p){
        System.out.println("Introduza o seu username: ");
        String codUtilizador = scan.next();

        System.out.println("Introduza o nome: ");
        String nome = scan.next();

        System.out.println("Introduza a sua latitude: ");
        double gpsx = Double.parseDouble(scan.next());

        System.out.println("Introduza a sua longitude: ");
        double gpsy = Double.parseDouble(scan.next());

        System.out.println("Introduza a sua password: ");
        String password = scan.next();
        try{
            Utilizador u = new Utilizador(codUtilizador, nome, gpsx, gpsy, password);;
            p.addUser(u);
        } catch (utilizadorJaExiste e){
            System.out.println(e.getMessage());
        }

    }

    public void registoVoluntario(Parse p) {
        System.out.println("Intruduza o seu username: ");
        String codUtilizador = scan.next();

        System.out.println("Introduza o nome: ");
        String nome = scan.next();

        System.out.println("Introduza a sua latitude: ");
        double gpsx = Double.parseDouble(scan.next());

        System.out.println("Introduza a sua longitude: ");
        double gpsy = Double.parseDouble(scan.next());

        System.out.println("Introduza a sua password: ");
        String password = scan.next();

        System.out.println("Intoduza o seu raio: ");
        double raio = Double.parseDouble(scan.next());

        try {
            Voluntario v =  new Voluntario(codUtilizador, nome, gpsx, gpsy, password, raio, true, 0,0, new ArrayList<>(), false);
            p.addUser(v);
        } catch (utilizadorJaExiste e){
            System.out.println(e.getMessage());
        }


    }


    public void registoLoja(Parse p){
        System.out.println("Intruduza o seu username: ");
        String codUtilizador = scan.next();

        System.out.println("Introduza o nome: ");
        String nome = scan.next();

        System.out.println("Introduza a sua latitude: ");
        double gpsx = Double.parseDouble(scan.next());

        System.out.println("Introduza a sua longitude: ");
        double gpsy = Double.parseDouble(scan.next());

        System.out.println("Introduza a sua password: ");
        String password = scan.next();

        try{
            Loja l = new Loja(codUtilizador, nome, gpsx, gpsy, password, new ArrayList<>());
            p.addUser(l);
        }   catch (utilizadorJaExiste e){
            System.out.println(e.getMessage());
        }

    }

    public void registoTransportadora(Parse p){
        System.out.println("Intruduza o seu username: ");
        String codUtilizador = scan.next();

        System.out.println("Introduza o nome: ");
        String nome = scan.next();

        System.out.println("Introduza a sua latitude: ");
        double gpsx = Double.parseDouble(scan.next());

        System.out.println("Introduza a sua longitude: ");
        double gpsy = Double.parseDouble(scan.next());

        System.out.println("Introduza a sua password: ");
        String password = scan.next();

        System.out.println("Introduza o seu nif: ");
        String nif = scan.next();

        System.out.println("Intoduza o seu raio: ");
        double raio = Double.parseDouble(scan.next());

        System.out.println("Introduza o seu Preço por KM: ");
        double precoKm = Double.parseDouble(scan.next());
        try{
            Transportadora t = new Transportadora(codUtilizador, nome, gpsx, gpsy, password, nif, raio, precoKm, true, 0, 0, new ArrayList<>(), 0, false);
            p.addUser(t);
        } catch (utilizadorJaExiste e){
            System.out.println(e.getMessage());
        }

    }


    public int menuUtilizador(){
        System.out.println("1 - Efectuar encomenda");
        System.out.println("2 - Aceitar serviço entrega");
        System.out.println("3 - Aceder a encomendas efetuadas");
        System.out.println("4 - Classificar entrega");
        System.out.println("\n0 - Sair");
        int a = scan.nextInt();
        return a;
    }

    public int menuVoluntario(){
        System.out.println("1 - Alterar disponilidade");
        System.out.println("2 - Selecionar encomenda a efetuar");
        System.out.println("3 - Transporte de medicamentos");
        System.out.println("\n0 - Sair");
        int a = scan.nextInt();
        return a;
    }

    public int menuTransportadora(){
        System.out.println("1 - Alterar disponibilidade");
        System.out.println("2 - Total faturado por uma Transportadora");
        System.out.println("3 - Selecionar encomenda a efetuar");
        System.out.println("4 - Transporte de medicamentos");
        System.out.println("\n0 - Sair");
        int a = scan.nextInt();
        return a;
    }

    public int menuLoja(){
        System.out.println("1 - Ver histórico");
        System.out.println("2 - Definir peso da encomenda");
        System.out.println("\n0 - Sair");
        int a = scan.nextInt();
        return a;
    }



    /**
     * Método que verifica se uma data é válida.
     * @param inDate
     * @return
     */
    public boolean isValidDate(String inDate)
    {
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
        dateFormat.setLenient(false);
        try{
            dateFormat.parse(inDate.trim());
        }catch (ParseException e){
            return false;
        }
        return true;
    }
    /**
     * Método que converte um objeto do tipo Date num objeto do tipo LocalDate.
     * @param dateToConvert
     * @return LocalDate
     */
    public LocalDate convertToLocalDate(Date dateToConvert)
    {
        return dateToConvert.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    }


    public LocalDateTime ler_data_inicial() {
        Date date1;
        LocalDateTime inicio = LocalDateTime.now();
        LocalTime lti = LocalTime.of(0, 0, 0);
        String data;
        boolean b;
        Scanner read = new Scanner(System.in);
        try {
            System.out.print("Introduza a data inicial (Nota: Use o formato dd/mm/aaaa): ");
            data = read.nextLine();
            date1 = new SimpleDateFormat("dd/MM/yyyy").parse(data);
            b = isValidDate(data);
            if (b == false) {
                System.out.print("\nErro: Inseriu um data inválida!\n");
                read.nextLine();
                return LocalDateTime.MAX;
            }
            LocalDate ld1 = convertToLocalDate(date1);
            inicio = LocalDateTime.of(ld1, lti);
            return inicio;
        } catch (Exception e) {
            System.out.print("\nErro: Formato da data inserido. Utilize o formato indicado!\n");
            read.nextLine();
            return LocalDateTime.MAX;
        }
    }

    /** Método que lê adequadamente uma data final através de um Scanner.
     * @param
     * @return data final
     */
    public LocalDateTime ler_data_final() {
        Date date2;
        LocalDateTime fim = LocalDateTime.now();
        LocalTime ltf = LocalTime.of(23, 59, 59);
        String data1;
        boolean a;
        Scanner scan = new Scanner(System.in);
        try {
            System.out.print("Introduza a data final (Nota: Use o formato dd/mm/aaaa): ");
            data1 = scan.nextLine();
            date2 = new SimpleDateFormat("dd/MM/yyyy").parse(data1);
            a = isValidDate(data1);
            if (a == false) {
                System.out.print("\nErro: Inseriu um data inválida!\n");
                scan.nextLine();
                return LocalDateTime.MAX;
            }
            LocalDate ld2 = convertToLocalDate(date2);
            fim = LocalDateTime.of(ld2, ltf);
            return fim;
        } catch (Exception e) {
            System.out.print("\nErro: Formato da data inserido. Utilize o formato indicado!\n");
            scan.nextLine();
            return LocalDateTime.MAX;

        }

    }

}
