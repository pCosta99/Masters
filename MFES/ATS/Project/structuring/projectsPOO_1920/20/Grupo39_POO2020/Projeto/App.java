import java.io.*;
import static java.lang.System.out;
import java.util.Scanner;
import java.util.Comparator;
import java.util.ArrayList;
import java.io.Serializable;
import java.util.stream.Collectors;
import java.util.List;
public class App implements Serializable
{
    //Model
    private Model model;

    //View(s)
    private Menu menuNotLogged, menuUtilizador, menuVoluntario, menuEmpresa, menuLoja, menuCriarConta;

    //Variables
    boolean loggedIn;
    Object atual;
    /**
     * Constructor for objects of class App
     */
    public App()
    {
        // Criar o menu
        String[] opcoesNotLogged = {"Nada", "Login","Criar Conta", "Gravar em binario", "Carregar de binario", "Carregar de ficheiro de logs", "Mostrar top 10 utilizadores","Terminar app"};
        String[] opcoesUtilizador = {"Logout", "Traz Aqui","Historico", "Avaliar", "Criar encomenda"};
        String[] opcoesVoluntario = {"Logout", "Sinalizar"};
        String[] opcoesEmpresa = {"Logout", "Sinalizar", "Mostrar lucro"};
        String[] opcoesLoja = {"Logout", "Alterar tempo de espera", "Adicionar Encomenda pronta a ser entregue", "Adicionar produto disponivel"};
        String[] opcoesCriarConta = {"Utilizador", "Voluntario", "Empresa", "Loja"};        
        
        this.menuNotLogged = new Menu(opcoesNotLogged);
        this.menuUtilizador = new Menu(opcoesUtilizador);
        this.menuVoluntario = new Menu(opcoesVoluntario);
        this.menuEmpresa = new Menu(opcoesEmpresa);
        this.menuLoja = new Menu(opcoesLoja);
        this.menuCriarConta = new Menu(opcoesCriarConta);
       

        // Criar o model
        this.model = new Model();
        //variaveis
        this.loggedIn = false;
        this.atual = null;
    }

    /**
     * O método main cria a aplicação e invoca o método run()
     */
    //public static void main(String[] args) {
    //    new App().run();
    //}

    
    
    /**
     * Executa o menu principal e invoca o método correspondente à opção seleccionada.
     */
    public void run() {
        Pair loginInfo;
        int r;
        Scanner scanner;
        String codLido;
        String command;
        String input;
        double preco;
        double avaliacao;
        boolean exit = false;
        do {
            //Escolher outra opcao faz com que o ciclo continue
            if (this.model.getQueueSize()>0){
                processQueueFirst();
            }

            if (this.atual != null){
                if (this.atual.getClass() == Utilizador.class){
                    Utilizador utilizadorAtual = (Utilizador) this.atual;
                    if (this.model.inPendentes(utilizadorAtual)){
                        //envia propostas para as encomendas deste utilizador
                        processPendentes();
                    }
                    out.println("Ola Utilizador " + utilizadorAtual.getNome() + " (" + utilizadorAtual.getCod() + ")");
                    menuUtilizador.executa();
                    switch (menuUtilizador.getOpcao()) {
                        case 0:
                            this.loggedIn = false;
                            this.atual = null;
                            break;
                        case 1:
                            out.println("Introduza o codigo da Encomenda, sff");
                            scanner = new Scanner(System.in);
                            codLido = scanner.nextLine();

                            if (this.model.inLoja(codLido) && !this.model.inQueue(codLido)){
                                this.model.toQueue(codLido);
                            }
                            else{
                                out.println("Esse codigo nao esta na nossa base de dados");
                            }
                            break;
                        case 2:
                            out.println("Deseja filtrar por transportador?\nSim: 1\nNao: 0");
                            scanner = new Scanner(System.in);
                            command = scanner.nextLine();
                            if (command.equals("1")){
                              //Mostrar historico filtrado
                              out.println("Insira o codigo do Transportador");
                              scanner = new Scanner(System.in);
                              codLido = scanner.nextLine();
                              
                              try{
                                  String historicoString = utilizadorAtual.getHistoricoFilteredString(codLido);

                                  if (historicoString.equals("")){
                                      out.println("Nao existem encomendas para serem mostradas");
                                    }
                                    else{
                                        out.println(historicoString);
                                    }
                                }
                              catch(NullPointerException e){
                                  out.println("Não existem encomendas para serem mostradas");
                                }
                            }
                            else{
                              //Mostrar historico
                              String historicoString = utilizadorAtual.getHistoricoString();
                              if (historicoString.equals("")){
                                  out.println("Nao existem encomendas para serem mostradas");
                              }
                              else{
                                  out.println(historicoString);
                                }
                            }
                            break;
                        case 3:
                            out.println("Introduza o cod do transportador que pretende avaliar");
                            scanner = new Scanner(System.in);
                            codLido = scanner.nextLine();

                            out.println("Introduza a avaliacao");
                            scanner = new Scanner(System.in);
                            avaliacao = Double.parseDouble(scanner.nextLine());

                            this.model.getTransportador(codLido).avaliar(avaliacao);
                            break;
                        case 4:
                            fazerEncomenda();
                            break;                         
                    }
                }
                else if(this.atual.getClass() == Voluntario.class){
                    Voluntario voluntarioAtual = (Voluntario) this.atual;
                    out.println("Ola Voluntario " + voluntarioAtual.getNome() + " (" + voluntarioAtual.getCod() + ")");
                    menuVoluntario.executa();
                    switch (menuVoluntario.getOpcao()) {
                        case 0:
                            this.loggedIn = false;
                            this.atual = null;
                            break;
                        case 1:
                            this.model.addDisponivel(voluntarioAtual.getCod());
                            out.println("Sinalizado");
                            break;
                    }
                }
                else if(atual.getClass() == Empresa.class){
                    Empresa empresaAtual = (Empresa) atual;
                    out.println("Ola Empresa " + empresaAtual.getNome() + " (" + empresaAtual.getCod() + ")");
                    menuEmpresa.executa();
                    switch(menuEmpresa.getOpcao()){
                        case 0:
                            this.loggedIn = false;
                            this.atual = null;
                            break;
                        case 1:
                            this.model.addDisponivel(empresaAtual.getCod());
                            out.println("Sinalizado");
                            break;
                        case 2:
                            out.println("A Empresa teve " + empresaAtual.getLucro() + " de lucro\n");
                            break;
                    }
                }
                else if(atual.getClass() == Loja.class){
                    Loja lojaAtual = (Loja) atual;
                    out.println("Ola Loja " + lojaAtual.getNome() + " (" + lojaAtual.getCod() + ")");
                    menuLoja.executa();
                    switch(menuLoja.getOpcao()){
                        case 0:
                            this.loggedIn = false;
                            this.atual = null;
                            break;
                        case 1:
                            out.println("Insira o tempo de espera estimado (1.5 => uma hora e meia (30 minutos)): ");
                            scanner = new Scanner(System.in);
                            input = scanner.nextLine();

                            lojaAtual.setTempo(Double.parseDouble(input));
                            break;
                        case 2:
                            adicionarEncomenda();
                            break;
                        case 3:
                            adicionarProdutoDisponivel();
                            break;
                    }
                }
            }
            else{
                menuNotLogged.executa();
                switch (menuNotLogged.getOpcao()) {
                    case 1:
                        if (loggedIn){
                            out.println("Ja esta logado");
                        }
                        else{
                            login();
                        }
                        break;
                    case 2:
                        criarConta();
                        break;

                    case 3:
                        out.println ("Insira nome do ficheiro para guardar");
                        scanner = new Scanner(System.in);
                        input = scanner.nextLine().toLowerCase();
                        try{
                            this.model.gravar(input);
                        }
                        catch (IOException e){
                            out.println(e.getMessage());
                        }
                        break;
                    case 4:
                        out.println ("Insira nome do ficheiro a carregar");
                        scanner = new Scanner(System.in);
                        input = scanner.nextLine().toLowerCase();

                        try{
                            this.model.carregar(input);
                        }
                        catch(IOException io){
                            out.println(io.getMessage());
                        }
                        catch(ClassNotFoundException e){
                            out.println(e.getMessage());
                        }
                        break;
                    case 5:
                        out.println ("Insira nome do ficheiro a carregar");
                        scanner = new Scanner(System.in);
                        input = scanner.nextLine().toLowerCase();

                        try{
                            this.model.carregarLogs(input);
                            out.println("done!");
                        }
                        catch(IOException io){
                            out.println(io.getMessage());
                        }
                        catch(ClassNotFoundException e){
                            out.println(e.getMessage());
                        }
                        catch(ExceptionEncomendaNaoEncontrada e){
                            out.println(e.getMessage());
                        }
                        break;
                    case 6:
                        out.println(this.model.getTopTenUtil());
                        break;
                    case 7:
                        exit = true;
                        break;
                        
                }
            }
        } while (exit == false);
    }

    //métodos auxiliares
    private void processQueueFirst() {
        for (int i = 0; i< this.model.getQueueSize(); i++){
            //processa encomendas
            String proximaCod = this.model.getQueue(i);
    
    
            BaseDeDados dados = this.model.getDados();
            Loja loja = this.model.getLoja(dados.qualLoja(proximaCod));
            Encomenda proxima = loja.getEncomenda(proximaCod);
    
            Object temp = this.model.getUtilizador(proxima.getCodCliente());
            Utilizador dest = (Utilizador) temp;
    
            //transportador que oferece o melhor preco
            if (this.model.getDisponiveis().size() != 0){
                Transportador transportador = null;
                try {
                    transportador = (Transportador) this.model.getDisponiveis().stream().map(key->this.model.getTransportador(key)).filter(key -> key.disponivel(loja.getLocal(),dest.getLocal())).sorted(Comparator.comparingDouble(transp->transp.getPreco(transp.getLocal().dist(loja.getLocal()) + loja.getLocal().dist(dest.getLocal()), proxima))).toArray()[0];
                }
                catch (ArrayIndexOutOfBoundsException e){
                    //nenhum capaz de entregar a encomenda;
                    out.println("Nenhum capaz de entregar a encomenda");
                    continue;
                }
                catch (NullPointerException e)
                {
                    //nenhum capaz de entregar a encomenda;
                    out.println("Nenhum capaz de entregar a encomenda");
                    continue;
                }
                double preco = loja.getTempo()*5 + transportador.getPreco(transportador.getLocal().dist(loja.getLocal()) + loja.getLocal().dist(dest.getLocal()), proxima);
        
                if (preco == 0 || dest.equals(this.atual)){
                    //só corre aceita se preco != 0 e dest.equals(this.atual)
                    if (preco == 0 || aceita(proximaCod, "" + preco,transportador)){
                        if (this.model.inDisponiveis(transportador.getCod())){
                          transportador.transportar(proxima.getCod());
                          //remove da queue e da loja
                          this.model.removeQueue(proximaCod);
                          loja.remove(proximaCod);
                          //uma vez que a encomenda é imediatamente entregue:
                          dest.addHistorico(transportador.getCod(), proximaCod, "" + preco);
                          dados.addEntregue(proxima);
                          dest.addNEnc();
                        }
                    }
                }
                else{
                    this.model.removeQueue(proximaCod);
                    this.model.addPendentes(dest.getCod(), proximaCod);
                }
            }
            break;
        }
    }

    private void processPendentes(){
        Utilizador utilizadorAtual = (Utilizador) this.atual;
        for (String c: this.model.getPendentes(utilizadorAtual.getCod())){
            BaseDeDados dados = this.model.getDados();

            Loja loja = this.model.getLoja(dados.qualLoja(c));
            Utilizador dest = utilizadorAtual;
            Encomenda proxima = loja.getEncomenda(c);
            String proximaCod = proxima.getCod();
            //transportador que oferece o melhor preco
            Transportador transportador = null;
            try {
                transportador = (Transportador) this.model.getDisponiveis().stream().map(key->this.model.getTransportador(key)).filter(key -> key.disponivel(loja.getLocal(),dest.getLocal())).sorted(Comparator.comparingDouble(transp->transp.getPreco(transp.getLocal().dist(loja.getLocal()) + loja.getLocal().dist(dest.getLocal()), proxima))).toArray()[0];
            }
            catch (ArrayIndexOutOfBoundsException e){
                //nenhum capaz de entregar a encomenda;
                out.println("Nenhum capaz de entregar a encomenda");
                continue;
            }
            catch (NullPointerException e)
            {
                //nenhum capaz de entregar a encomenda;
                out.println("Nenhum capaz de entregar a encomenda");
                continue;
            }
            
            double preco = loja.getTempo()*5 + transportador.getPreco(transportador.getLocal().dist(loja.getLocal()) + loja.getLocal().dist(dest.getLocal()), proxima);

            if (preco == 0 || aceita(proximaCod, "" + preco)){
                if (this.model.inDisponiveis(transportador.getCod())){
                    transportador.transportar(proxima.getCod());
                    //remove encomenda da queue e da loja
                    this.model.removeQueue(proximaCod);
                    loja.remove(proximaCod);

                    utilizadorAtual.addHistorico(transportador.getCod(), proximaCod, "" + preco);
                    dados.addEntregue(proxima);
                    dest.addNEnc();
                    this.model.removePendentes(dest.getCod());
                  }
           }        
            else{
                out.println("Tem encomendas pendentes mas nao existem transportadores disponiveis");
            }
        }

    }

    private boolean aceita(String enc, String preco){
        out.println("Aceita a entrega da encomenda " + enc + " por " + preco + "?\nSim: 1\nNao: 0\n");

        //Menu improvisado
        Scanner input = new Scanner(System.in);
        String command = input.nextLine();

        if (command.equals("1")){
            return true;
            
        }
        else{
            return false;
        }
    }
    
        private boolean aceita(String enc, String preco,Transportador transportador){
        out.println("Aceita a entrega da encomenda " + enc + " por " + preco + "?\nSim: 1\nNao: 0\n");

        //Menu improvisado
        Scanner input = new Scanner(System.in);
        String command = input.nextLine();
        
        Empresa transportado = (Empresa) transportador;

        if (command.equals("1")){
            transportado.addLucro(preco);
            return true;
            
        }
        else{
            return false;
        }
    }

    private void login(){
        Scanner scanner;
        Pair loginInfo;
        BaseDeDados dados = this.model.getDados();

        out.println ("Login");
        out.println ("Insira Username");
        scanner = new Scanner(System.in);
        String username = scanner.nextLine().toLowerCase();

        out.println ("Insira Senha");
        scanner = new Scanner(System.in);
        String senha = scanner.nextLine();

        loginInfo = new Pair (username, senha);
        if (dados.getLogin(loginInfo) != null){
            Object temp = this.model.login(scanner, dados.getLogin(loginInfo));
            this.atual = temp.getClass().cast(temp);  //transformar atual para class correspondente;

            loggedIn = true;

        }

        else {
            out.println ("Informação inválida");
        }

    }

    private void criarConta(){
        Scanner scanner;
        out.println("Criar conta");
        out.println ("Insira Username");
        scanner = new Scanner(System.in);
        String username = scanner.nextLine().toLowerCase();

        if (this.model.usernameExistente(username)){
            out.println ("Username já existente");
        }
        else {
            out.println ("Insira Senha");
            String senha = scanner.nextLine();
            Pair username_senha = new Pair(username, senha);
            out.println("Que tipo de conta pretende criar?");
                menuCriarConta.executa();
                switch (menuCriarConta.getOpcao()){
                    case 0:
                        criarUtilizador(username_senha);
                        break;
                    case 1:
                        criarVoluntario(username_senha);
                        break;
                    case 2:
                        criarEmpresa(username_senha);
                        break;
                    case 3:
                        criarLoja(username_senha);
                        break;
                }
        }
    }

    private void criarUtilizador(Pair username_senha){
        out.println ("Insira o seu Nome Pŕoprio");
        Scanner scanner = new Scanner(System.in);
        String nome = scanner.nextLine();

        out.println ("Insira o sua Localizacao");
        scanner = new Scanner(System.in);
        GPS local = new GPS(scanner.nextLine());

        int r = this.model.criarUtilizador(username_senha, nome, local);
        if (r == 0){
            out.println("Conta criada com sucesso");
        }
        else{
            out.println("Erro na criacao de conta");
        }
    }

    private void criarVoluntario(Pair username_senha){
        out.println ("Insira o seu Nome Pŕoprio");
        Scanner scanner = new Scanner(System.in);
        String nome = scanner.nextLine();

        out.println ("Insira o sua Localizacao");
        scanner = new Scanner(System.in);
        GPS local = new GPS(scanner.nextLine());

        out.println ("Insira o seu raio");
        scanner = new Scanner(System.in);
        double raio = Double.parseDouble(scanner.nextLine());

        int r = this.model.criarVoluntario(username_senha, nome, local, raio);
        if (r == 0){
            out.println("Conta criada com sucesso");
        }
        else{
            out.println("Erro na criacao de conta");
        }
    }

    private void criarEmpresa(Pair username_senha){
        out.println ("Insira o Nome");
        Scanner scanner = new Scanner(System.in);
        String nome = scanner.nextLine();

        out.println ("Insira a Localizacao");
        scanner = new Scanner(System.in);
        GPS local = new GPS(scanner.nextLine());

        out.println ("Insira o raio");
        scanner = new Scanner(System.in);
        double raio = Double.parseDouble(scanner.nextLine());

        out.println ("Insira o nif");
        scanner = new Scanner(System.in);
        String nif = scanner.nextLine();

        out.println ("Insira o preco");
        scanner = new Scanner(System.in);
        double preco = Double.parseDouble(scanner.nextLine());

        int r = this.model.criarEmpresa(username_senha, nome, local, raio, nif, preco);
        if (r == 0){
            out.println("Conta criada com sucesso");
        }
        else{
            out.println("Erro na criacao de conta");
        }
    }

    private void criarLoja(Pair username_senha){
        out.println ("Insira o Nome");
        Scanner scanner = new Scanner(System.in);
        String nome = scanner.nextLine();

        out.println ("Insira a Localizacao");
        scanner = new Scanner(System.in);
        GPS local = new GPS(scanner.nextLine());

        int r = this.model.criarLoja(username_senha, nome, local);
        if (r == 0){
            out.println("Conta criada com sucesso");
        }
        else{
            out.println("Erro na criacao de conta");
        }
    }
    
    private void adicionarEncomenda(){
        Loja atualLoja = (Loja) atual;
        
        boolean b = true;
        Scanner scanner;
        String codLoja = atualLoja.getCod();
        String novoProduto;
        ArrayList<Pair> produtos;
        String codUtilizador;
        String quantidade;
        double peso;
        String nif;
        String lojasString;
        
        while (b){
            b = false;
            out.println("Insira o codigo do Utilizador (cliente) para quem se dirige a encomenda");
                               
            scanner = new Scanner(System.in);
            codUtilizador = scanner.nextLine();
            produtos = new ArrayList<Pair>();
            
            do{
                out.println("Escolha um produto, pressione 0 para terminar, ou pressione 1 para terminar e mudar de loja:");
                out.println(this.model.getLojaProdutos(codLoja));
                
                scanner = new Scanner(System.in);
                novoProduto = scanner.nextLine();              
            
                if(novoProduto.equals("0")){
                    if(produtos.isEmpty()){
                        continue;
                    }
                    out.println("Introduza, por favor:");
                    out.println("Um peso estimado da encomenda");
                    scanner = new Scanner(System.in);
                    peso = Double.parseDouble(scanner.nextLine());
                    
                    out.println("Insira nif do cliente");
                    scanner = new Scanner(System.in);
                    nif = scanner.nextLine();
                    
                    out.println("O codigo da encomenda e: " + this.model.criarEncomenda(peso, nif, produtos, codUtilizador, codLoja));
                }
                else if(novoProduto.equals("1")){
                    if(produtos.isEmpty()){
                        b=true;
                        continue;
                    }
                    out.println("Introduza, por favor:");
                    out.println("Um peso estimado da encomenda");
                    scanner = new Scanner(System.in);
                    peso = Double.parseDouble(scanner.nextLine());
                    
                    out.println("Insira nif do cliente");
                    scanner = new Scanner(System.in);
                    nif = scanner.nextLine();
                    
                    out.println("O codigo da encomenda e: " + this.model.criarEncomenda(peso, nif, produtos, codUtilizador, codLoja));
                    b = true;
                }
                else{  
                    if (!this.model.produtoDisponivel(atualLoja.getCod(), novoProduto)){
                        out.println("Produto Nao Disponivel");
                        break;    
                    }
                    out.println("Introduza a quantidade");
                    scanner = new Scanner(System.in);
                    quantidade = scanner.nextLine();
                                        
                    produtos.add(new Pair(novoProduto, quantidade)); 
                    out.println(novoProduto + " adicionado com sucesso"); 
                }
            }while(!novoProduto.equals("0") && !novoProduto.equals("1"));
        }
    }
    
    private void fazerEncomenda(){         
        Utilizador atualUtilizador = (Utilizador) atual;
        
        boolean b = true;
        String quantidade;
        String novoProduto;
        ArrayList<Pair> produtos = new ArrayList<Pair>();
        double peso;
        String nif;
        String lojasString;
        while (b){
            b = false;
            lojasString = this.model.getLojasString();
            if (lojasString.equals("")){
                out.println("Nenhuma Loja disponivel");
                return;
            }    
            out.println("Insira o codigo da loja onde fazer a encomenda");
            out.println(this.model.getLojasString());
            
            Scanner scanner;
            scanner = new Scanner(System.in);
            String codLoja = scanner.nextLine();
            produtos = new ArrayList<Pair>();
            
            do{
                out.println("Escolha um produto, pressione 0 para terminar, ou pressione 1 para terminar e mudar de loja:");
                out.println(this.model.getLojaProdutos(codLoja));
                
                scanner = new Scanner(System.in);
                novoProduto = scanner.nextLine();
                
            
                if (novoProduto.equals("0")){
                    if(produtos.isEmpty()){
                        continue;
                    }
                    out.println("Introduza, por favor:");
                    out.println("Um peso estimado da encomenda");
                    scanner = new Scanner(System.in);
                    peso = Double.parseDouble(scanner.nextLine());
                    
                    out.println("Insira nif do cliente");
                    scanner = new Scanner(System.in);
                    nif = scanner.nextLine();
                    out.println("O codigo da encomenda e: " + this.model.criarEncomenda(peso, nif, produtos, atualUtilizador.getCod(), codLoja));
                }
                else if (novoProduto.equals("1")){
                    if(produtos.isEmpty()){
                        b=true;
                        continue;
                    }
                    out.println("Introduza, por favor:");
                    out.println("Um peso estimado da encomenda");
                    scanner = new Scanner(System.in);
                    peso = Double.parseDouble(scanner.nextLine());
                    
                    out.println("Insira nif do cliente");
                    scanner = new Scanner(System.in);
                    nif = scanner.nextLine();
                    
                    out.println("O codigo da encomenda e: " + this.model.criarEncomenda(peso, nif, produtos, atualUtilizador.getCod(), codLoja));
                    b = true;
                }
                else{ 
                    if (!this.model.produtoDisponivel(codLoja, novoProduto)){
                        out.println("Produto Nao Disponivel");
                        continue;    
                    }
                    out.println("Introduza a quantidade");
                    scanner = new Scanner(System.in);
                    quantidade = scanner.nextLine();
                                        
                    produtos.add(new Pair(novoProduto, quantidade)); 
                    out.println(novoProduto + " adicionado com sucesso"); 
                }
                
            }while(!novoProduto.equals("0") && !novoProduto.equals("1"));
        }
    }
    
    private void adicionarProdutoDisponivel(){
        Loja atualLoja = (Loja) atual;
        Scanner scanner;
        
        out.println("Insira:");
        out.println("Referencia:");
        
        scanner = new Scanner(System.in);
        String ref = scanner.nextLine();
        
        out.println("Descricao:");
        
        scanner = new Scanner(System.in);
        String descricao = scanner.nextLine();
        
        out.println("Preco:");
        
        scanner = new Scanner(System.in);
        double preco = Double.parseDouble(scanner.nextLine());
        
        
        int sucesso = this.model.addProdutoDisponivel(atualLoja.getCod(), ref, descricao, preco);
        if (sucesso == 0){
            out.println("Adicionado com sucesso");
        }
    }
    
}

