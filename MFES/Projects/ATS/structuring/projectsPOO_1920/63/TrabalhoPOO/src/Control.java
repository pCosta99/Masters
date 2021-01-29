import java.util.*;
import java.util.stream.Collectors;

public class Control {
    /** variaveis de instancia */
    View view;
    Model model;

    /** constructores de classe */
    /** vazio */

    /** parametrico */
    public Control(View view, Model model){
        this.view = view;
        this.model = model;
    }

    /** copia */

    /** gets/sets das variaveis de instancia */

    /** metodos override */

    /** metodos especificos */
    //----------------------------- NIVEL 0 -----------------------//
    /**
     * Inicializa a aplicação
     */
    public void inicio(){
        char option;
        String buffer;
        Scanner input = new Scanner(System.in);

        do{
            this.view.inicio();

            try {
                buffer = input.nextLine();
            }catch (NoSuchElementException e){
                buffer = "";
            }
            if(buffer.length() > 0){
                option = buffer.charAt(0);
            }else{
                option = '0';
            }

            switch (option){
                case '1' : option = this.entrarOUregistar1(0); break;
                case '2' : option = this.entrarOUregistar1(1); break;
                case '3' : option = this.sobre(); break;
                case '4' : option = this.estatisticas(); break;
                default: break;
            }
        }while (option != '0');

        input.close();
    }

    //----------------------------- NIVEL 1 -----------------------//
    /**
     * Gere a opção Entrar (i=0) ou Registar (i=1) no nivel 1
     */
    public char entrarOUregistar1(int i){
        char option;
        String buffer;
        Scanner input = new Scanner(System.in);

        do{
            this.view.entrarOUregistar1(i);

            try {
                buffer = input.nextLine();
            }catch (NoSuchElementException e){
                buffer = "";
            }
            if(buffer.length() > 0){
                option = buffer.charAt(0);
            }else{
                option = '0';
            }

            if(option == '1' || option == '2' || option == '3' || option == '4'){
                entrarOUregistar2(i, option);
            }

        }while (option != '0');

        input.close();

        return '0';
    }

    /**
     * Gere a opção Sobre
     */
    public char sobre(){
        char option;
        String buffer;
        Scanner input = new Scanner(System.in);

        do{
            this.view.sobre();

            try {
                buffer = input.nextLine();
            }catch (NoSuchElementException e){
                buffer = "";
            }
            if(buffer.length() > 0){
                option = buffer.charAt(0);
            }else{
                option = '0';
            }

        }while (option != '0');

        input.close();

        return '0';
    }

    /**
     * Gere a opção Estatisticas
     */
    public char estatisticas(){
        Scanner input = new Scanner(System.in);
        char option;
        String buffer;

        do {
            this.view.janelaEstatisticas();
            try {
                buffer = input.nextLine();
            }catch (NoSuchElementException e){
                buffer = "";
            }
            if(buffer.length() > 0){
                option = buffer.charAt(0);
            }else{
                option = '0';
            }
            switch (option) {
                case '1':
                    String transportadora;
                    System.out.println("Escreva o codigo da transportadora");
                    transportadora = input.nextLine();
                    double custoTotal = 0;
                    for (DadosEntrega de : this.model.devolveTransportadora(transportadora).getHistorico()) {
                        custoTotal += de.getCusto();
                    }
                    System.out.println(custoTotal);
                    break;
                case '2':
                    List<Transportadora> aux = new ArrayList<>(this.model.getTransportadoras());
                    aux.sort(new ComparatorTransKms());
                    aux.stream().limit(10).collect(Collectors.toList());
                    for (Transportadora t : aux) {
                        System.out.println("Transportadora " + t.getNome() + " - " + t.totalKms() + " kms");
                    }
                    break;
            }
        }while (option != '0');
        input.close();
        return '0';
    }

    //----------------------------- NIVEL 2 -----------------------//
    /**
     * Gere a opção Entrar (i=0) ou Registar (i=1) no nivel 2
     */
    public void entrarOUregistar2(int i, char tipoUsuario){
        String email;
        String pass;
        String codUsuario;
        boolean flag = false;

        Scanner input = new Scanner(System.in);

        do{
            //leitura do email
            System.out.print("Email: ");
            email = input.nextLine();

            //leitura da password
            System.out.print("Password: ");
            pass = input.nextLine();

            if(i==0) {
                codUsuario = this.model.validaLogin(email, pass, tipoUsuario);
            }else {
                codUsuario = this.model.validaRegisto(email, pass, tipoUsuario);
            }

            if(codUsuario.length() != 0) {
                flag = true;
            }else{
                View.print("Email ou password incorretos!\n");
            }
        }while (!flag);

        switch (codUsuario.charAt(0)){
            case 'u':
                Utilizador u = this.model.devolveUtilizador(codUsuario);
                menuUtilizador(u);
                break;
            case 'v':
                Voluntario v = this.model.devolveVoluntario(codUsuario);
                menuVoluntario(codUsuario);
                break;
            case 't':
                Transportadora t= this.model.devolveTransportadora(codUsuario);
                menuTransportadora(codUsuario);
                break;
            case 'l':
                Loja l = this.model.devolveLoja(codUsuario);
                menuLoja();
                break;
            default: break;
        }

        input.close();
    }

    //----------------------------- NIVEL 3 -----------------------//
    /**
     * Gere o menu do utlizador
     */
    public void menuUtilizador(Utilizador u){
        char option;
        String buffer;
        Scanner input = new Scanner(System.in);

        do{
            this.view.janelaUtilizador(u.getCodUtilizador());

            try {
                buffer = input.nextLine();
            }catch (NoSuchElementException e){
                buffer = "";
            }
            if(buffer.length() > 0){
                option = buffer.charAt(0);
            }else{
                option = '0';
            }

            switch (option){
                case '1' : this.registarEncomenda(u); break;
                case '2' : this.estadoEntrega(u); break;
                case '3' : option = this.sobre(); break;
                case '4' : option = this.estatisticas(); break;
                default: break;
            }
        }while (option != '0');

        input.close();
    }

    /**
     * Gere o menu do voluntario
     */
    public void menuVoluntario(String codVoluntario){
        char option;
        String buffer;
        Scanner input = new Scanner(System.in);
        Voluntario vol = this.model.devolveVoluntario(codVoluntario);
        Encomenda atual = null;
        double temp;

        do{
            this.view.janelaVoluntario();

            try {
                buffer = input.nextLine();
            }catch (NoSuchElementException e){
                buffer = "";
            }
            if(buffer.length() > 0){
                option = buffer.charAt(0);
            }else{
                option = '0';
            }

            switch (option){
                case '1' :
                        this.model.voluntarioPodeEntregar(codVoluntario);
                        View.print("Sinalizou que pode entregar\n");
                        break;
                case '2' :
                        atual = this.model.voluntarioVaiALoja(codVoluntario);
                        if (atual!= null) View.print("Vai a loja buscar a encomenda com codigo: " + atual.getCodEncomenda() + "\n");
                        else View.print("Nao ha lojas por perto com encomendas disponiveis\n");
                        break;
                case '3' :
                        if (atual != null) {
                            temp = this.model.entregarEncomenda(codVoluntario, atual.getCodEncomenda());
                            View.print("Entregou a encomenda em "+ temp + "unidades de tempo\n");
                        }
                        else View.print("O voluntario nao tem emcomenda, vá levantar uma a uma loja\n");
                        break;
                default: break;
            }
        }while (option != '0');

        input.close();
    }

    /**
     * Gere o menu do transportadora
     */
    public void menuTransportadora(String codTransportadora) {
        Scanner input = new Scanner(System.in);
        char option;
        String buffer;
        do {
            this.view.janelaTransportadora();

            try {
                buffer = input.nextLine();
            }catch (NoSuchElementException e){
                buffer = "";
            }
            if (buffer.length() > 0) {
                option = buffer.charAt(0);
            } else {
                option = '0';
            }

            switch (option) {
                case '1':
                    List<Encomenda> res = this.model.verEncomendasValidas(
                            this.model.devolveTransportadora(codTransportadora).getCodTransportadora());
                    if (res.size() > 0) {
                        for (Encomenda e : res) {
                            System.out.println(e.getCodEncomenda());
                        }
                    }
                    break;
                case '2':
                    String encomenda;
                    System.out.println("Escreva o codigo da encomenda");
                    encomenda = input.nextLine();
                    this.model.proporEntregaT(encomenda, this.model.devolveTransportadora(codTransportadora));
                    break;

                case '3':
                    for (DadosEntrega de : this.model.devolveTransportadora(codTransportadora).getHistorico()) {
                        System.out.println(de.toString());
                    }
                    break;
                default:
                    break;
            }

        }while (option != '0');

        input.close();

    }


    /**
     * Gere o menu do loja
     */
    public void menuLoja(){
        Scanner input = new Scanner(System.in);
        String opcao = "n/d", encomenda;

        while(!opcao.equals("0")) {

            this.view.janelaLoja();
            opcao = input.nextLine();

            switch (opcao) {
                case "1":
                    View.print("Insira qual o codigo da encomenda pronta a ser levantada\n");
                    encomenda = input.nextLine();
                    this.model.sinalizaPodeSerLevantada(encomenda);
                    break;
                case "0":
                    opcao = "0";
                    break;
                default:
                    break;
            }
        }
    }

    //----------------------------- NIVEL 4 -----------------------//
    /**
     * Gere a opção do utlizador: 1 - Registar Nova Entrega
     * @param utilizador que vai inserir encomenda
     */
    public void registarEncomenda(Utilizador utilizador){
        char option;
        boolean flag = false;
        Loja l = new Loja();
        int quantidade = 0;
        double peso = 0.0;

        String buffer;
        Scanner input = new Scanner(System.in);

        Encomenda encomenda = new Encomenda();
        encomenda.setUtilizador(utilizador);

        View.print("Insira os dados da nova encomenda:\n");

        do {
            View.print("Loja: ");
            try {
                buffer = input.nextLine();
            }catch (NoSuchElementException e){
                buffer = "";
            }
            if (buffer.length() >0) {
                if (!this.model.lojaExiste(buffer))
                    this.model.insereLoja(new Loja(buffer));
                l = this.model.devolveLoja(buffer);
            }
        }while(buffer.length() == 0);

        encomenda.setLoja(l);

        do {
            View.print("Quantidade de produtos na encomenda: ");
            buffer = input.nextLine();
            try{
                quantidade = Integer.parseInt(buffer);
                flag = true;
            }catch (NumberFormatException e){
                View.print("Número inserido é inválido!");
            }
        }while(!flag);
        flag = false;

        ArrayList<LinhaEncomenda> linhasEnc = new ArrayList<>();

        for(int i=0 ; i<quantidade ; i++){
            String newCodProduto;
            String newDescricao;
            double newPrecoUnitario = 0.0;
            double newQuantidade = 0.0;

            do {
                View.print("\nInsira o código do produto " + i + " [ex P1]: ");
                newCodProduto = input.nextLine();
            }while(newCodProduto.length()==0);

            do {
                View.print("\nInsira a descrição do produto " + i + " [ex. Queijo da serra]: ");
                newDescricao = input.nextLine();
            }while (newDescricao.length() == 0);

            do {
                View.print("\nInsira o preço unitário do produto " + i + " [ex. 1.0]: ");
                buffer = input.nextLine();
                try{
                    newPrecoUnitario = Double.parseDouble(buffer);
                    flag = true;
                }catch (NumberFormatException e){
                    View.print("\nNúmero inserido é inválido!");
                }
            }while(!flag && buffer.length() == 0);
            flag = false;

            do {
                View.print("\nInsira a quantidade do produto " + i + " [ex. 1.0]: ");
                buffer = input.nextLine();
                try{
                    newQuantidade = Double.parseDouble(buffer);
                    flag = true;
                }catch (NumberFormatException e){
                    View.print("\nNúmero inserido é inválido!");
                }
            }while(!flag && buffer.length() == 0);
            flag = false;

            LinhaEncomenda le = new LinhaEncomenda(newCodProduto, newDescricao, newPrecoUnitario,
            newQuantidade);

            linhasEnc.add(le);

            peso += newPrecoUnitario * newQuantidade;
        }

        encomenda.setPeso(peso);

        encomenda.setLinhasEncomenda(linhasEnc);

        this.model.insereEncomenda(encomenda);

        input.close();
    }

    public void gravarLog(){
        try {
            this.model.gravarLog();
        }catch (Exception e){
            System.out.println(e.getMessage());
        }
    }

    /**
     * Gere a opção do utlizador: 2 - Estado Entrega
     * @param utilizador que vai ver o estado
     */
    public void estadoEntrega(Utilizador utilizador) {
        int totalPendentes=0;
        List<DadosEntrega> de = this.model.verEncomendasPendentes(utilizador);

        if(de.size() > 0) {
            View.print("Tem as seguintes encomendas pendentes:\n");
            for (DadosEntrega d : de) {
                totalPendentes++;
                View.print(totalPendentes + ":\n\t" + de.toString() + "\n");
            }

            Scanner input = new Scanner(System.in);
            char option;
            int nEnc = 0;
            boolean flag = false;
            String buffer;
            do {
                View.print("Indique o número da encomenda que pretende alterar estado: ");
                buffer = input.nextLine();
                try{
                    nEnc = Integer.parseInt(buffer);
                    flag = true;
                }catch (NumberFormatException e){
                    View.print("\nNúmero inserido é inválido!");
                }
            }while(!flag && buffer.length() == 0 && nEnc > totalPendentes );

            int resposta = -1;
            do {
                View.print("Pretende aceitar(1) ou rejeitar(1) a oferta de entrega " + nEnc + " ? ");
                buffer = input.nextLine();
                try{
                    resposta = Integer.parseInt(buffer);
                    if(resposta==0 || resposta==1){
                        flag = true;
                    }
                }catch (NumberFormatException e){
                    View.print("\nNúmero inserido é inválido!");
                }
            }while(!flag && buffer.length() == 0);

            if(resposta == 1){
                this.model.devolveTransportadora(de.get(nEnc-1)
                        .getCodTransportadora()).insereHistorico(de.get(nEnc-1));
                this.model.insereAceite(de.get(nEnc-1).getCodEntrega());
            }else{
                this.model.removeEntregaPendente(de.get(nEnc-1).getCodEntrega());
            }

            input.close();
        }else{
            View.print("Não tem encomendas pendentes!\n");
        }
    }





}
