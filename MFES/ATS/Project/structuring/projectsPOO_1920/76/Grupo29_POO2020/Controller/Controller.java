package Controller;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import Exceptions.InvalidInputException;
import Exceptions.NoEntityException;
import Model.Conta;
import Model.IConta;
import Model.IEncomenda;
import Model.Gerador;
import Model.IGestao;
import Model.Leitura;
import Model.Gestao;
import Model.LinhaEncomenda;
import Utilities.Ponto;
import View.View;

public class Controller implements IController {
    private static final Pattern p_email = Pattern.compile("[^@]+@[^\\.]+\\..+"); 
    private static final Pattern p_entityCode = Pattern.compile("[altuvp]\\d+");
    private static final Pattern p_double = Pattern.compile("\\d+\\.\\d+"); 
    private static final Pattern p_int = Pattern.compile("\\d+"); 
    private static final Pattern p_string = Pattern.compile(".+");
    private static final Pattern p_bigdecimal = Pattern.compile("\\d+(\\.\\d+)?");
    private static final Pattern p_nif = Pattern.compile("\\d{9}");
    private static final Pattern p_classificacao = Pattern.compile("(\\d|10)");
    private static final Pattern p_encomenda = Pattern.compile("e\\d+");
    private static final Pattern p_datetime = Pattern.compile("\\d{4}\\-\\d{2}\\-\\d{2}T\\d{2}\\:\\d{2}\\:\\d{2}");

    private IGestao gest;
    private BufferedReader input; 
    private IConta user;

    public Controller(){
        this.gest = new Gestao();
        this.gest.addConta(new Conta("a0", "admin@gmail.com", "123", new ArrayList<>()));
        this.gest.addConta(new Conta("u0", "user@gmail.com", "123", new ArrayList<>()));
        this.gest.addConta(new Conta("t0", "transportadora@gmail.com", "123", new ArrayList<>()));
        input =  new BufferedReader(new InputStreamReader(System.in));
    }


    private void pressEnterToContinue() throws IOException{
            View.pressEnterToContinue();
            input.readLine();
    }

    private boolean paraCancelar(){
        View.paraCancelar();
        try{
            String inp = input.readLine();
            if(inp != null && inp.charAt(0) == '0'){
                View.cancelar();
                input.readLine();
                return true;
            }
            return false;
        }catch(Exception e){
            return false;
        }
    }

    private void error(String e) throws IOException{
        View.error(e);
        pressEnterToContinue();
    }

    private String insira(String insira, Pattern p) throws IOException{
        String res;
        do{
            View.insira(insira);
            res = input.readLine(); 
        } while (!p.matcher(res).find());
        return res;
    }

    private boolean pretende(String s) throws IOException{
        View.pretende(s);

        return input.readLine().charAt(0) == 's';
    }

    private void navegador(List<String> l){
        int i = 0, auxInt;
        String auxString = "";
        boolean r = true, searchMode = false;
        List<String> temp = null;
        while(r){
            View.clear();
            View.out(l.get(i));
            View.navegador(searchMode);
            try{
                if((auxString = input.readLine()) != null)
                    switch(auxString.charAt(0)){
                        case 'n': if(i >= l.size() - 1) error("página seguinte não existe."); else i++; break;
                        case 'p': if(i > 0) i--; else error("página anterior não existe."); break;
                        case 'c': r = false; break;
                        case 'f': i = 0; break;
                        case 'l': i = l.size() - 1; break;
                        case 'g': if((auxInt = Integer.parseInt(insira("o número da página", p_int))) >= l.size()) error("página inexistente."); else i = auxInt - 1; break;
                        case 's': View.insira("o que prentende pesquisar");
                                  String p = input.readLine();
                                  if(searchMode){
                                    searchMode = false;
                                    i = 0;
                                    l = temp;
                                  }
                                  List<String> search = l.stream().filter(x -> x.contains(p)).collect(Collectors.toList());
                                  if(search.size() > 0){
                                      searchMode = true;
                                      i = 0;
                                      temp = l;
                                      l = search;
                                  }
                                  else error("não foram encontrados resultados.");
                                  break;
                        case 'i': if(searchMode){l = temp; searchMode = false;} else error("modo pesquisa não ativo."); break;
                        default: error("opção inválida."); break;
                    } 
                else
                    error("opção inválida");
            }catch(Exception e){
                View.error(e.getMessage());
            }
        }
    }

    /**
     * Gere o início de sessão.
     * @return Conta lida.
     * @throws IOException
     */
    private IConta iniciarSessao() throws IOException{
        boolean r = true;
        IConta user = null;
        while (r) {
            View.clear();
            View.intro();
            View.login();
            View.prompt();
            
            char in = 0;
            String inp = input.readLine();
            if (inp.length() > 0)
            in = inp.charAt(0);
        
            switch (in) {
                case '1': 
                    user = autenticacao();
                    r = user == null;
                    break;
                case '2':
                    if ((user = registo()) != null)
                        r = false;
                    else 
                        View.loginResultado(3); //email já existente
                    break;
                default:
                        error("opção inválida");
                        break;      
            }   
        }

        return user;
    }


    /**
     * Gere a autenticação do programa.
     * @return Conta lida.
     * @throws IOException
     */
    private IConta autenticacao() throws IOException{
        IConta res = null;
        int code = 0;
        String email = insira("o seu email", p_email);
        View.insira("a sua senha");
        String senha = String.valueOf(input.readLine());
        
        res = gest.getBaseDeDados().getConta(email);

        if (res == null)
            code = 1; //email inexistente
        else if (!res.getPassword().equals(senha))
            code = 2; //senha incorreta


        View.loginResultado(code);
        pressEnterToContinue();

        return res;
    }

    /**
     * Gere o registo de uma entidade no programa.
     * @return Conta registada.
     * @throws IOException
     */
    private IConta registo() throws IOException{
        IConta res = null;
        if(paraCancelar()) return res;
        String id, email, senha;

        id = insira("o seu ID", p_entityCode);
        email = insira("o seu email", p_email);
        View.insira("a sua senha");
        senha = String.valueOf(input.readLine());


        if (!gest.getBaseDeDados().existeConta(email)) { 
            res = new Conta(id, email, senha, new ArrayList<>());
            gest.addConta(res); 
        }
        error("já existe uma conta com o email\'" + email + "\'");        

        return res;
    }

    /**
     * Insere um pedido de encomenda a uma loja, por parte de um utilizador.
     * @throws IOException
     */
    private void inserirPedidoEncomenda() throws IOException, InvalidInputException, NoEntityException{
        if(paraCancelar()) return;

        Set<String> encs = gest.getCodEnc();
        
        String codigo = Gerador.gerarCodigoEncomenda(encs);
        View.out("o código da encomenda é " + codigo);
        String codLoja = insira("o código da loja", p_entityCode);
        int nLinhas = Integer.parseInt(insira("o número de produtos na encomenda", p_int));
        HashSet<LinhaEncomenda> linhas = new HashSet<>(nLinhas);

        for (int i = 0; i < nLinhas; i++) {
            View.insira("os dados referentes ao produto " + (i+1));
            String cod = insira("o código", p_entityCode);
            String desc = insira("a descrição", p_string);
            int quant = Integer.parseInt(insira("a quantidade", p_int));
            linhas.add(new LinhaEncomenda(cod, desc, quant, BigDecimal.ZERO));
        }

        View.out("\nÉ encomenda médica?");
        View.out("(s) - Sim\t(n) - Não");
        Boolean medica = input.readLine().equals("s");


        gest.registaNovaEncomenda(codigo, user.getId(), codLoja, -1.d, linhas, LocalDateTime.now(), null, medica);
    }


    private IEncomenda concluirPedidoEncomenda(List<IEncomenda> le) throws IOException, InvalidInputException, NoEntityException{
        if(le.size() <= 0) throw new NoEntityException("encomenda inexistente.");

        IEncomenda e = le.get(0);
        View.out(e.toString());
        double peso = e.getPeso();
        if (peso < 0) {
            peso = Double.parseDouble(insira("o peso total da encomenda", p_double));
            e.setPeso(peso);
        }
        Set<String> produtos = e.getProdutosDasLinhas();
        BigDecimal valorUnit;
        for(String p : produtos){
            valorUnit = new BigDecimal(insira("o preço unitário do produto " + p, p_bigdecimal));
            e.setValorUnitn(p, valorUnit);
        }

        return e;
    }


    /**
     * Regista uma loja, por parte de um admin.
     * @throws IOException
     */
    private void registarLoja() throws IOException, InvalidInputException{ 
        if(paraCancelar()) return;
        String id = Gerador.gerarCodigoEntidade(gest.getIDsContas(), 'l');
        System.out.println("Código aleatório gerado: " + id);
        String nome =  insira("o nome da loja", p_string);
        System.out.println("Informações sobre o local da loja");
        double lat = Double.parseDouble(insira("latitude", p_double));
        double lon = Double.parseDouble(insira("longitude", p_double));

        gest.registaLoja(id, nome, new Ponto(lat, lon));
    }

    /**
     * Regista uma transportadora, por parte de um admin.
     * @throws IOException
     */
    private void registarTransportadora() throws IOException, InvalidInputException { 
        if(paraCancelar()) return;

        String id = Gerador.gerarCodigoEntidade(gest.getIDsContas(), 't');
        System.out.println("Código aleatório gerado: " + id);
        String nome = insira("o nome da transportadora", p_string);
        String nif = insira("o nif", p_nif);
        System.out.println("-Informações da posição da transportadora-");
        double lat = Double.parseDouble(insira("a latitude", p_double));
        double lon = Double.parseDouble(insira("a longitude", p_double));
        double raio = Double.parseDouble(insira("o raio", p_double));
        BigDecimal ppkm = new BigDecimal(insira("o preço por kilometro", p_bigdecimal));
    
        gest.registaTransportadora(id, nome, nif, new Ponto(lat, lon), raio, ppkm);
    }

    /**
     * Regista um utilizador, por parte de um admin/utilizador.
     * @throws IOException
     */
    private void registarUtilizador() throws IOException, InvalidInputException {
        if(paraCancelar()) return;
        String id = Gerador.gerarCodigoEntidade(gest.getIDsContas(), 'u');
        System.out.println("Código aleatório gerado: " + id);
        String nome =  insira("o nome", p_string);
        System.out.println("Informações sobre a posição do utilizador");
        double lat = Double.parseDouble(insira("latitude", p_double));
        double lon = Double.parseDouble(insira("longitude", p_double));

        gest.registaUtilizador(id, nome, new Ponto(lat, lon));
    }

    /**
     * Regista um voluntário, por parte de um admin.
     * @throws IOException
     */
    private void registarVoluntario() throws IOException, InvalidInputException { 
        if(paraCancelar()) return;
        String id = Gerador.gerarCodigoEntidade(gest.getIDsContas(), 'v');
        System.out.println("Código aleatório gerado: " + id);
        String nome =  insira("o nome", p_string);
        System.out.println("-Informações da posição do voluntário-");
        double lat = Double.parseDouble(insira("latitude", p_double));
        double lon = Double.parseDouble(insira("longitude", p_double));
        double raio = Double.parseDouble(insira("o raio", p_double));

        gest.registaVoluntario(id, nome, new Ponto(lat, lon),raio);
    }

    /**
     * Regista uma entidade a designar.
     * @throws IOException
     */
    private void registarEntidade() throws IOException, InvalidInputException {
        if(paraCancelar()) return;
        View.opcoesRegistoEntidade();
        String inp = input.readLine();
        if(inp != null) 
            switch(inp.charAt(0)){
                case 'l': registarLoja();           break;
                case 't': registarTransportadora();  break;
                case 'u': registarUtilizador();     break;
                case 'v': registarVoluntario();     break; 
        }

    }

    /**
     * Classifica um distribuidor por parte de um utilizador.
     * @throws IOException
     */
    private void classificarDistribuidor() throws IOException, InvalidInputException, NoEntityException{
        String cod;
        int classificacao;
        if(paraCancelar()) return;
        cod = insira("o id do distribuidor a classificar", p_entityCode);
        classificacao = Integer.parseInt(insira("a classificação (0 a 10)", p_classificacao));
        gest.query7(user.getId(), classificacao, cod);
    }

    /**
     * Informa que uma encomenda está pronta por parte de uma loja.
     * @throws IOException
     */
    private void encomendaPronta() throws IOException, InvalidInputException, NoEntityException {
        if(paraCancelar()) return;
        List<IEncomenda> le = gest.encomendasPendentesALoja(user.getId());
        View.emFila(le);
        if(le.size() > 0){
            String cod = insira("o código da encomenda", p_encomenda);
            gest.pendenteAtualizadaParaProntas(concluirPedidoEncomenda(le.stream().filter(e -> e.getCodigo().equals(cod)).collect(Collectors.toList())));
        }
    }

    /**
     * Mostra as encomendas pendentes para um distirbuidor.
     * @throws IOException
     */
    private void verEncomendasPendentes() throws NoEntityException, InvalidInputException, IOException {
        List<IEncomenda> encs = gest.encomendasDisponiveisParaEntrega(user.getId());
        if(encs == null || encs.size() == 0)
            View.error("não foram encontradas encomendas.");
        else{
            navegador(encs.stream().map(e -> e.toString()).collect(Collectors.toList()));
            if(pretende("transportar alguma encomenda"))
                if(gest.aceitaEntregar(user.getId(), insira("o código da encomenda", p_encomenda)))
                   View.out("Encomenda atribuída ao distribuidor \'" + user.getId() + "\' com sucesso.");
                else
                    View.error("Distribuidor \'" + user.getId() + "\' não disponível para entregar."); 
        }
    }

    /**
     * Mostra as encomendas feitas por um Distribuidor.
     * @throws IOException
     */
    private void verEncsDistribuidor() throws IOException, NoEntityException, InvalidInputException {
        String cod = insira("o código do distribuidor", p_entityCode);
        Set<String> encs = gest.getEncsDistribuidor(cod);
        if(encs == null || encs.size() == 0)
            System.out.println("Sem encomendas.");
        else
            navegador(encs.stream().collect(Collectors.toList()));
    }

    public void verEncsEntidade()throws IOException, NoEntityException, InvalidInputException {
        String ent = insira("o código da entidade", p_entityCode);
        List<Map.Entry<IEncomenda,String>> encomendas = gest.query8(ent);
        Set<String> aux = encomendas.stream().map(x -> x.getKey().getCodigo()).collect(Collectors.toSet());
        View.query8(encomendas, gest.dataPrevistaEntregas(aux));
    }

    /**
     * Mostra o total por um Distribuidor, num determinado período de tempo.
     * @throws IOException
     */
    private void totalFaturado() throws IOException, NoEntityException, InvalidInputException {
        String cod = insira("o código do distribuidor", p_entityCode);
        LocalDateTime inicio = LocalDateTime.parse(insira("data inicial <AAAA-MM-DD\'T\'HH:MM:SS>", p_datetime));
        LocalDateTime fim = LocalDateTime.parse(insira("data final <AAAA-MM-DD\'T\'HH:MM:SS>", p_datetime));

        View.out("Total faturado: " + gest.query9(cod, inicio, fim));
    }

    /**
     * Mostra os 10 utilizadores que mais utilizaram o sistema.
     * @throws IOException
     */
    private void top10Utilizadores(){
        View.query10(gest.query10());
    }

    /**
     * Mostra as 10 tranportadoras que mais utilizaram o sistema.
     * @throws IOException
     */
    private void top10Transportadoras(){
        View.query11(gest.query11());
    }

    private void encomendaEntregue() throws IOException, NoEntityException, InvalidInputException{
        if(paraCancelar()) return;

        String codEnc = insira("o código da encomenda", p_encomenda);
        LocalDateTime tempoEntrega = LocalDateTime.parse(insira("data de entrega <AAAA-MM-DD\'T\'HH:MM:SS>", p_datetime));
        double kms = Double.parseDouble(insira("kilometros percorridos", p_double));

        this.gest.entregue(user.getId(), codEnc, tempoEntrega, kms);
    }

    private void emFila() throws IOException, NoEntityException, InvalidInputException{
        List<IEncomenda> le = gest.encomendasPendentesALoja(user.getId());
        View.emFila(le);
        if(le.size() > 0 && pretende("ver detalhes de uma encomenda")){
            String enc = insira("o código da encomenda", p_encomenda);
            int size = le.size(), found = 0, i;
            for(i = 0; i < size; i++)
                if(le.get(i).getCodigo().equals(enc)){
                    View.out(le.get(i).toString());
                    found++;
                }
            if(found == 0)  throw new NoEntityException("encomenda \'" + enc +"\' inexistente.");
        }
    }

    private void transpMedicamentos() throws IOException, NoEntityException{
        gest.transpMedicamentos(user.getId(), pretende("transportar medicamentos"));
    }



    private void inputAdmin(char in) throws IOException, NoEntityException, InvalidInputException {
        switch (in) {
            case '1': registarEntidade(); break;
            case '2': verEncsDistribuidor(); break;
            case '3': totalFaturado(); break;
            case '4': top10Utilizadores(); break;
            case '5': top10Transportadoras(); break;
            case '6': verEncsEntidade(); break;
        }
    }

    private void inputLoja(char in) throws IOException, NoEntityException, InvalidInputException {
        switch (in) {
            case '1': encomendaPronta(); break;
            case '2': emFila(); break;
        }
    }

    private void inputTransportadora(char in) throws IOException, NoEntityException, InvalidInputException{
        switch (in) {
            case '1': verEncomendasPendentes(); break;
            case '2': encomendaEntregue(); break;
        }
    }

    private void inputUtilizador(char in) throws IOException, NoEntityException, InvalidInputException{
        switch (in) {
            case '1': inserirPedidoEncomenda(); break;
            case '2': classificarDistribuidor(); break;
        }
    }

    private void inputVoluntario(char in) throws IOException, NoEntityException, InvalidInputException{
        switch (in) {
            case '1': verEncomendasPendentes(); break;
            case '2': encomendaEntregue(); break;
            case '3': transpMedicamentos(); break;
        }
    }


    /**
     * Remete um input de uma dada conta para a função correta.
     * 
     * @param in    Opção escolhida pelo utilizador.
     * @throws IOException
     */
    private void inputEntidade(char in) throws IOException, NoEntityException, InvalidInputException{
        View.clear();
        switch (user.getTipo()) {
            case 'a': inputAdmin(in);          break;
            case 'l': inputLoja(in);           break;
            case 't': inputTransportadora(in); break;
            case 'u': inputUtilizador(in);     break;
            case 'v': inputVoluntario(in);     break;
        }
        pressEnterToContinue();
    }

    /**
     * Remete um input de uma dada conta para a função correta.
     * 
     * @throws IOException
     */
    private void gravarEstado() throws IOException {
        View.clear();
        View.gravarEstado();
        String op = this.input.readLine(); 
        if(op != null)
            switch(op){
                case "s" : gest.save();
                           break;
                case "n" : View.insira("o caminho");
                           gest.save(input.readLine());
                           break;
                case "c" : return;
            }
        View.out("\n\nEstado gravado com sucesso.");
        pressEnterToContinue();
    }

    /**
     * Remete um input de uma dada conta para a função correta.
     * 
     * @throws IOException
     */
    private void carregarEstado() throws IOException, InvalidInputException, NoEntityException, ClassNotFoundException{
        View.clear();
        View.carregarEstado();
        String op = this.input.readLine(); 
        if(op != null)
            switch(op){
                case "l" : if(pretende("carregar o ficheiro padrão"))
                                gest = Leitura.readStandardPath();
                            else {
                                View.insira("o caminho do ficheiro");
                                gest = Leitura.readFromFilePath(this.input.readLine());
                            }
                            break;
                case "t" : if(pretende("carregar o ficheiro padrão"))
                                gest.load();
                           else {
                                View.insira("o caminho do ficheiro");
                                gest.load(this.input.readLine());
                                 }
                            break;
                case "c" : return;
            }
        View.out("\n\nEstado carregado com sucesso.");
        pressEnterToContinue();
    }

    /**
     * Mostra as notificações para um user.
     * @throws IOException
     */
    private void verNotificacoes() throws IOException{
        View.clear();
        List<String> noti = user.getNotificacoes();
        if(noti == null || noti.size() == 0)
            System.out.println("\nSem notificações.");
        else
            navegador(noti);

        pressEnterToContinue();
    }

    private void verPerfil() throws IOException, NoEntityException, InvalidInputException{
        View.clear();
        View.out("Id: "+ user.getId());
        View.out("Email: " + user.getEmail());
        char tipo = user.getTipo();

        if (tipo == 'v' || tipo == 't')
            View.out(String.format("Classificação: %.1f/10", gest.getDistRating(user.getId()).getMean()));
        
        if(tipo != 'a'){
            List<Map.Entry<IEncomenda,String>> encomendas = gest.query8(user.getId());
            Set<String> aux = encomendas.stream().map(x -> x.getKey().getCodigo()).collect(Collectors.toSet());
            View.query8(encomendas, gest.dataPrevistaEntregas(aux));
        }
        if(tipo == 'u'){
            int n;
            boolean r = true;
            List<Map.Entry<String, IEncomenda>> l_ts;
           do{
                l_ts = gest.nEncomendasPorAceitarDoUtilizador(user.getId());
                n = l_ts.size();
                if(n > 0){
                    View.aviso("tem " + n + " pedidos de entregas de transportadoras por aceitar");
                    pressEnterToContinue();
                    View.transpPorDecidir(l_ts);
                    if(r = pretende("responder a algum pedido")){
                        String t = insira("o código da transportadora", p_entityCode);
                        gest.utilizadorAceitaTranportadora(pretende("aceitar"), user.getId(), t);
                    }
                }
            }while(r && n > 0);
        }
        pressEnterToContinue();
    }

    private void criarEstado() throws IOException{
        View.clear();
        if(gest != null)
            View.aviso("tem um estado em execução");
            if(pretende("gravar o estado atual"))
                gravarEstado();
            gest = new Gestao();
    }

    /**
     * Menu do programa.
     * 
     * @throws IOException
     */
    public void menu() throws IOException{
        boolean r = true; 
        while(user == null){
            try{
                View.clear();
                user = iniciarSessao();
            }catch(Exception e){
                error(e.getMessage());
            }
        }
        char tipo = user.getTipo();
        String id = user.getId();
        
        char in;
        r = true;
        while (r) {
            try{
                View.clear();
                View.intro();
                View.greeting(id);
                switch (tipo) {
                    case 'a': View.opcoesAdmin();          break;
                    case 'l': View.opcoesLoja();           break;
                    case 't': View.opcoesTransportadora(); break;
                    case 'u': View.opcoesUtilizador();     break;
                    case 'v': View.opcoesVoluntario();     break;
                }
                View.opcoesGeral();


                View.prompt();
                in = 0;
                String inp = input.readLine();
                if (inp.length() > 0)
                    in = inp.charAt(0);

            
                switch (in) {
                    case '1': case '2': case '3': case '4': case '5': case '6': inputEntidade(in); break;
                    case 'c': criarEstado(); user = iniciarSessao(); tipo = user.getTipo(); id = user.getId(); break;
                    case 'g': gravarEstado(); break;
                    case 'l': carregarEstado(); user = iniciarSessao(); tipo = user.getTipo(); id = user.getId(); break;
                    case 'm': user = iniciarSessao(); tipo = user.getTipo(); id = user.getId(); break;
                    case 'n': verNotificacoes(); break;
                    case 'p': verPerfil(); break;
                    case 's': r = false; break;
                }
            }
            catch(Exception e){
                View.clear();
                error(e.getMessage());
                e.printStackTrace();
                pressEnterToContinue();
            }
        }

        View.outro();
        input.close();
    }
}