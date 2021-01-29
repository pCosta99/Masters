
import java.lang.System;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.io.Console;
import java.io.FileNotFoundException;
import java.io.IOException;


public class Controller {


    private static User.Meteorologia met;
    private static User.EstadoCovid estadoCovid;
    /**
     * metodo main que inicia o programa e que permite realizar as operaçoes disponiveis
     * no inicio da execuçao gera uma metereologia e um estado covid aleatorio 
     */
    public static void main(String[] args) {

        View view = new View();
        TrazAqui ta =new TrazAqui();

        try{
            ta = ta.lerFicheiro ("save");
        }
        catch(FileNotFoundException e){
            view.print("Ficheiro Invalido:" + e.getMessage());
        }
        catch(IOException e){
            view.print("Erro:" + e.getMessage());
        }
        catch (ClassNotFoundException e) {
            view.print("Classe nao encontrada:" + e.getMessage());
        }

        String password1 = "";
        boolean quit = false;


        Random randomInt = new Random();
        int randomIntGerado = randomInt.nextInt(4);
        switch(randomIntGerado){
            case 0:
                met = User.Meteorologia.Sol;
                break;
            case 1:
                met = User.Meteorologia.Nevoeiro;
                break;
            case 2:
                met = User.Meteorologia.Chuva;
                break;
            case 3:
                met = User.Meteorologia.Neve;
                break;
            default:
                met = User.Meteorologia.Sol;
                break;
        }
        

        int randomIntGerado2 = randomInt.nextInt(3);
        switch(randomIntGerado2){
            case 0:
                estadoCovid = User.EstadoCovid.Normalidade;
                break;
            case 1:
                estadoCovid = User.EstadoCovid.Precaucao;
                break;
            case 2:
                estadoCovid = User.EstadoCovid.Emergencia;
                break;
            default:
                estadoCovid = User.EstadoCovid.Precaucao;
                break;
        }

        view.inicio(met, estadoCovid);
        while (!quit) {
            switch (view.menu()) {

                case 1:
                    //Registar-me
                    String nome;
                    
                    nome=view.registoInicialnome();
                    try {
                        if (ta.existeNome(nome)){
                        throw new UserJaRegistadoException ("Nome de Utilizador ja usado, porfavor escolha outro");
                    }
                    } catch (Exception e) {
                        while(ta.existeNome(nome)){
                            e.getMessage();
                            nome=view.registoInicialnome();
                        }
                    }
                    

                    
                    password1 = view.registoInicialpassword();


                    switch (view.registoInicialOpcoes()) {

                        case 1:
                            //Utilizador
                            User adicionadoUtilizador = view.registoUtiliazador(password1);
                            ta.addUser ( adicionadoUtilizador,nome);
                            break;

                        case 2:
                            //Voluntario
                            User adicionadoVoluntario = view.registoVoluntario(password1);
                            ta.addUser ( adicionadoVoluntario, nome);
                            break;
                        
                        case 3:
                            //Loja
                            User adicionadoLoja = view.registoLoja(password1);
                            ta.addUser ( adicionadoLoja,nome);
                            break;
                        
                        case 4:
                            //Transportadoras
                            User adicionadoTransportadora = view.registoTransportadora(password1);
                            ta.addUser ( adicionadoTransportadora,nome);
                            break;

                        default : 
                            break;
                    }
                    
                    break;

                
                case 2:
                    //login
                    view.print("Digite o seu nome");
                    String nomeCod = view.scan();
                    
                    Console console = System.console();
                    char[] passwordArray = console.readPassword("Digite a sua palavra-passe:\n");

                    password1 = "";
                    for (char i : passwordArray){
                        password1 = password1 + i;
                    }

                    
                    if (!ta.existeNome(nomeCod)) {
                        view.print("Utilizador nao registado");
                        break;  
                    }
                    nomeCod=ta.nomeCodigo(nomeCod);
                    
                    boolean sair = false;
                    User u =ta.getUser(nomeCod);
                    while (!u.getPassword().equals(password1) && !sair){
                        view.print("\nQuer tentar de novo? S/N");
                        String resposta = view.scan(); 
                        if (resposta.equals("N")){
                            sair = true;
                        }
                        else if(resposta.equals("S"))
                        {
                            passwordArray = console.readPassword("Introduza a sua palavra-passe:\n");

                            password1 = "";
                            for (char i : passwordArray)
                            {
                                password1 = password1 + i;
                            }
                        }
                        else
                        {
                            view.print("\nResposta incorrecta, escolha S/N");
                        }
                    }
                    if (u.getPassword().equals(password1) && !sair){
                                String tipo = u.getClass().getName();
                                switch(tipo) {
                                    case "Transportadoras":
                                        executaTransportadoras( nomeCod, ta, view);
                                        break;
                                    case "Utilizador":
                                        executaUtilizador( nomeCod, ta, view);
                                        break;
                                    case "Voluntarios":
                                        executaVoluntario( nomeCod, ta, view);
                                        break;
                                    case "Lojas":
                                        executaLojas( nomeCod , ta, view);
                                        break;
                                    case "LojaComFila":
                                        executaLojas( nomeCod , ta, view);
                                        break;
                                    }
                                
                            }
  
                    break;

                case 3:
                    //Carregar de Ficheiro (texto)
                    try {
                        ta.lerFicheiroCSV(view.nomeFicheiro());
                    }
                    catch (Exception  e){
                        view.print("Oops, nao consegui ler o ficheiro :(\n" + e.getMessage());
                    }
                    break;
                
                case 4:
                    //imprimir texto
                    try{
                        ta.paraFicheiroCSV(view.nomeFicheiro());
                    }
                    catch(FileNotFoundException e){
                        view.print("Ficheiro Invalido" + e.getMessage());
                    }
                    break;

                case 5:
                    //ler binario
                    try{
                        ta = ta.lerFicheiro (view.nomeFicheiro());
                    }
                    catch(FileNotFoundException e){
                        view.print("Ficheiro Invalido:" + e.getMessage());
                    }
                    catch(IOException e){
                        view.print("Erro:" + e.getMessage());
                    }
                    catch (ClassNotFoundException e) {
                        view.print("Classe nao encontrada:" + e.getMessage());
                    }
                    break;
                    
                case 6:
                    //imprimir binario
                    try{
                        ta.guardaFicheiro (view.nomeFicheiro());
                    }
                    catch(FileNotFoundException e){
                        view.print("Ficheiro Invalido:" + e.getMessage());}
                    catch(IOException e){
                        view.print("Erro:" + e.getMessage());
                    }
                    break;

                case 7 : 
                    //Quit
                    try{
                        ta.guardaFicheiro ("save");
                    }
                    catch(FileNotFoundException e){
                        view.print("Ficheiro Invalido:" + e.getMessage());}
                    catch(IOException e){
                        view.print("Erro:" + e.getMessage());
                    }
                    view.print("Programa terminado com sucesso e seu estado guardado!");
                    quit = true;
                    break;

                default:

                    break;
            }
        }
        view.close();
    }

    /**
     * metodo que permite as Transportadoras realizarem as operaçoes para si disponiveis
     */
    private static void executaTransportadoras(String nomeCod ,TrazAqui ta,View view)
            
    {
        boolean sair = false;
        while (!sair){
            switch(view.menuTransportadora()){
                case 1:
                    //muda disponibilidade
                    
                    Boolean b = view.pergunta("Qual a sua disponibilidade atual?\nEsta disponivel (true/false)");
                    try {
                        view.print(ta.alteraDisponibilidade(nomeCod ,b));
                    } catch (Exception e) {
                        view.printErro(e.getMessage());
                    }
                    
                    break;
                                                
                case 2:
                    //Aceitar entrega de certa encomenda no seu raio

                    try{

                        if (ta.getDisponibilidade(nomeCod) == false){
                            throw new MudancaDisponibilidadeException ("Mude a sua disponibilidade antes");
                        }

                        if (ta.getJaFoiBuscar(nomeCod).size() > ta.getCapacidade(nomeCod)){
                            view.print("Excedeu a sua capacidade de Encomendas, entregue as atuais antes de pegar em mais");
                        }

                        //Aceitar certa encomenda
                        
                        view.print("Qual a encomenda que pretende aceitar? escreva o codigo dela");
                        view.apresentaEncomendasParaEntregar ( ta.escolherEncomendaAEntregar(nomeCod));

                        String escolhaEncTransportadora = view.scan();

                        if (ta.getEncomendaQuePrecisaSerEntregue(escolhaEncTransportadora).getEMedica() && !(ta.aceitoTransporteMedicamentos(nomeCod))){
                            view.print("Voce nao pode transportar encomendas medicas!");
                            break;
                        }

                        ta.adicionaTransportadoraACorrespondenciaDaEncomendaEscolhida (nomeCod , escolhaEncTransportadora);
                    }

                    catch (MudancaDisponibilidadeException m) {
                        view.printErro(m.getMessage());
                    }
                    catch (ListaVaziaException e){
                        view.printErro(e.getMessage());
                    }

                    break;

                case 3:
                    //Ir buscar certa encomenda a uma loja
                    try {
                        view.print("Qual a encomenda que pretende ir buscar? (exx)");
                        view.print(ta.getParaLevar(nomeCod));
                        String escolhaEncParalevarTransportadora = view.scan();
                        if (!ta.getParaLevar(nomeCod).contains(escolhaEncParalevarTransportadora)){
                            throw new EncomendaInvalidaException("Encomenda invalida, porfavor escolha uma da lista fornecida");
                        }

                        ta.addJaFoiBuscar(nomeCod , escolhaEncParalevarTransportadora);
                        ta.removeParaLevar(nomeCod ,escolhaEncParalevarTransportadora);
                        ta.removeDaFila ( nomeCod , escolhaEncParalevarTransportadora);
                    }
                    catch (ListaVaziaException e){
                        view.printErro(e.getMessage());
                    }
                    catch(EncomendaInvalidaException e){
                        view.printErro(e.getMessage());
                    }


                    break;

                case 4:
                    //transportar encomenda
                    view.print("Qual a encomenda que pretende Transportar?");
                    try {
                        view.print (ta.getJaFoiBuscar(nomeCod).toString());
                        String escolhaEncParaTransportar = view.scan();
                        if (!ta.getJaFoiBuscar(nomeCod).contains(escolhaEncParaTransportar)){
                            throw new EncomendaInvalidaException("Encomenda invalida, porfavor escolha uma da lista fornecida");
                        }
                        ta.removeJaFoiBuscar(nomeCod , escolhaEncParaTransportar);

                        Encomenda encSendoEntregue = ta.getEncomendaQuePrecisaSerEntregue(escolhaEncParaTransportar);
                        ta.removeEncomendaQuePrecisaSerEntregue(encSendoEntregue);

                        Utilizador utilizadorQuePediuEncomenda = ta.getUtilizadorQuePediuEncomenda (encSendoEntregue);
                        Lojas lojaQuePreparouEncomenda = ta.getLojaQuePreparouEncomenda(encSendoEntregue);



                        double taxaDeEntrega = ta.custoEntrega(nomeCod , lojaQuePreparouEncomenda.getCoordenadas(), utilizadorQuePediuEncomenda.getCoordenadas(),encSendoEntregue.getPeso());
                        double kmsPercorridos1 = ta.kmsPercorridos(nomeCod , utilizadorQuePediuEncomenda.getCoordenadas() , lojaQuePreparouEncomenda.getCoordenadas());
                        double tempoDemorado1 = 0;

                        if (lojaQuePreparouEncomenda.getClass().getName().equals("LojaComFila")){
                            LojaComFila lojaComFilaAtual = (LojaComFila) lojaQuePreparouEncomenda;
                            tempoDemorado1 += ta.tempoRecolherEntrega(nomeCod, lojaComFilaAtual , met, estadoCovid);
                            lojaComFilaAtual.addTempoEspera(estadoCovid);
                        }

                        tempoDemorado1 += ta.tempoChegadaLoja(nomeCod , lojaQuePreparouEncomenda,met);
                        tempoDemorado1 += ta.tempoChegadaUtilizador(nomeCod , lojaQuePreparouEncomenda, utilizadorQuePediuEncomenda, met);
                        

                        ta.addToRegistoUser(utilizadorQuePediuEncomenda.getId() , nomeCod , encSendoEntregue.clone(), 0, LocalDate.now(),kmsPercorridos1,taxaDeEntrega, tempoDemorado1);
                        ta.addToRegistoUser(nomeCod ,nomeCod , encSendoEntregue.clone(), 0, LocalDate.now(), kmsPercorridos1,taxaDeEntrega, tempoDemorado1);
                        ta.addToRegistoUser(lojaQuePreparouEncomenda.getId() ,nomeCod , encSendoEntregue.clone(), 0, LocalDate.now(), kmsPercorridos1,taxaDeEntrega,tempoDemorado1);
                                                    
                                                    
                        view.print("Encomenda Entregue!");
                    }catch (EncomendaInvalidaException e2){
                        view.printErro(e2.getMessage());
                    }

                        break;
                                                
                                                
                case 5:
                    //"5 - consultas avançadas"
                    
                    switch(view.consultarDadosTransportadora())
                    {
                        case 1:
                            //Ver o meu perfil
                            Transportadoras t = (Transportadoras) ta.getUser(nomeCod);
                            view.print(t.toString());
                            break;
                        case 2:
                            //- Ver total faturado num periodo de tempo
                            view.print(ta.totalFaturadoTaxas(nomeCod));
                            break;
                        case 3:
                            //ver meus registos 
                            view.print(ta.registoGeral(nomeCod));
                            break;

                        case 4: 
                            //Ver registos num dado periodo de tempo
                           view.print("Verificacao dos extratos de viagem num determinado periodo de tempo\n");
                           LocalDate dataI = view.scanDataInicial();
                           LocalDate dataF = view.scanDataFinal();
                           view.print(ta.verRegistosEntrePeriodoDeTempo(nomeCod,dataI,dataF));
                           break;

                        case 5:
                            // - Ver as minhas classificacoes
                            view.print(ta.transportadorClassificacoes(nomeCod));
                            break;
        
                        case 6:
                            //Ver top 'x'
                            criteriosdeComparacao (nomeCod , ta, view );
                            break;

                        case 7:
                            //Voltar Atraz
                            break;

                        default:
                            break;
                    }
                    break;
                case 6:
                    //Voltar ao menu inicial
                    sair = true;
                    break;
            }
        }
    }




    /**
     * metodo que permite ao Utilizador realizarem as operaçoes para si disponiveis
     */
    private static void executaUtilizador(String nomeCod ,TrazAqui ta,View view)
    {
        boolean sair = false;
        while (!sair){
            switch(view.menuUtilizador()){
                case 1:
                    //Criar encomenda
                    view.print("Qual a Loja a qual quer encomendar?\n");

                    List<User> contas =ta.getContasList();
                    for (User u : contas){
                        if (u.getClass().getName().equals("Lojas") || u.getClass().getName().equals("LojaComFila"))
                            view.print("Nome da loja : " + u.getNome() + " -> " + u.getId());
                        if (u.getClass().getName().equals("LojaComFila")){
                            LojaComFila lojaFilaAtual = (LojaComFila) u;
                            if (lojaFilaAtual.tempoMedioEspera() == -1){
                                view.print("Tempo Medio de espera : ainda nao determinado");
                            }
                            else{
                                view.print("Tempo Medio de espera : " + lojaFilaAtual.tempoMedioEspera());
                            }
                        }
                    }
                    
                    String lojaEscolhida = view.scan();
                    try{
                        Encomenda novaEncomenda = view.novaEncomenda(nomeCod, lojaEscolhida,ta.getcontadorEnc());
                    
                        User uu = ta.getUser(lojaEscolhida);
                        if (uu.getClass().getName().equals("LojaComFila")){
                            LojaComFila l = (LojaComFila) uu;
                            l.adicionaAFila(novaEncomenda);
                        }

                        ta.adicionaEncomenda(nomeCod,novaEncomenda);
                        ta.adicionaParaPreparacao(novaEncomenda, lojaEscolhida);
                    }catch(Exception e){
                        view.printErro(e.getMessage());
                    }

                    break;
                case 2:
                    //Pedir entrega de uma encomenda
                        
                    try { 
                        view.print("Digite o codigo da encomenda que deseja ser transportada\n\n" + ta.utilizadorEncomendasParaEntrege(nomeCod));
                    
                        String encomendaEscolhida = view.scan();
                        if (!ta.utilizadorEncomendasParaEntrege(nomeCod).contains(encomendaEscolhida)){
                            throw new EncomendaInvalidaException ("Encomenda invalida, porfavor escolha uma da lista fornecida");
                        }
                        
                        ta.solicitarEncomenda(nomeCod,encomendaEscolhida);

                    } catch (ListaVaziaException e){
                        view.printErro(e.getMessage());
                    }catch (EncomendaInvalidaException e2){
                        view.printErro(e2.getMessage());
                    } 

                    break;
                case 3:

                    //Aceitar Transporte
                    try {
                    view.print("Qual a encomenda que quer aceitar?\n" + ta.utilizadorEncomendasPorAceitar(nomeCod));
                    String codEncAceitar = view.scan();

                    if (!ta.utilizadorEncomendasPorAceitar(nomeCod).contains(codEncAceitar)){
                        throw new EncomendaInvalidaException("Encomenda invalida, porfavor escolha uma da lista fornecida");
                    }


                    List<Transportadoras> todos = ta.getTransportadorasEncomenda(codEncAceitar);
                    if (todos.size() == 0){
                        throw new ListaVaziaException("Ainda nenhum transportador demonstrou interesse");
                    }
                    ta.utilizadorRemoveEncomendaPorAceitar(nomeCod,codEncAceitar);
                    Encomenda encomendaAceitar = ta.getEncomendaParaEntrega(codEncAceitar);
                    Map<String,User> contas2 = ta.getContasMap();
                    for (Transportadoras t: todos){
                        view.print("\nTransportadora:  " + t.getId() + "  " + t.getNome() + "\nPreco:  " + 
                        ta.custoEntrega(t.getId(),contas2.get(encomendaAceitar.getCodLoja()).getCoordenadas(), 
                        contas2.get(encomendaAceitar.getCodUtilizador()).getCoordenadas(), encomendaAceitar.getPeso())
                        + "\nTempo ate a loja:  " + ta.tempoChegadaLoja(t.getId() ,(Lojas)contas2.get(encomendaAceitar.getCodLoja()),met)
                        + "\nClassificacao media da transportadora:  " + t.verClassificacoesMedia());
                        
                        
                        if (contas2.get(encomendaAceitar.getCodLoja()).getClass().getName().equals("LojaComFila")){
                            LojaComFila lojaComFilaAtual = (LojaComFila) contas2.get(encomendaAceitar.getCodLoja());
                            double tempoDemoradoFilaLoja = lojaComFilaAtual.tempoMedioEspera();
                            view.print("Tempo medio da Fila da Loja :" + tempoDemoradoFilaLoja );
                        }
                    }
                    view.print("Insira o codigo da transportadora escohida (txx) ou 0 para recusar todas");
                    
                    String respostasAceitar = view.scan();

                    if (respostasAceitar != "0"){
                        ta.aceitarEncomenda(respostasAceitar,codEncAceitar);
                    }
                    } catch (ListaVaziaException e){
                        view.printErro(e.getMessage());
                    }
                    catch (EncomendaInvalidaException e2){
                        view.printErro(e2.getMessage());
                    }

                    break;

                case 4:
                    //Classificar alguem
                    try {
                        view.print("Qual quer classificar? Coloque o codigo de encomenda\n" + ta.escolheClassificar(nomeCod));
                        String codEnc = view.scan();
                        view.print("Qual a classificacao que quer dar? (de 1-5)\n");
                        int classificacao = view.scanInt();
                        ta.classificarEntrega(nomeCod,codEnc, classificacao);
                        view.print("Classificado com sucesso!\n");
                    } catch (ListaVaziaException e){
                        view.printErro(e.getMessage());
                    }
                    break;

                case 5:
                    //Consultas avancadas
                    switch(view.consultarDadosUtilizador()){
                        case 1:
                            //Ver o meu perfil
                            Utilizador u = (Utilizador) ta.getUser(nomeCod);
                            view.print(u.toString());
                            break;
                        case 2:
                            //ver registos encomendas pedidas
                            Utilizador atualUtilizador1 =(Utilizador) ta.getUser(nomeCod);
                            view.print(atualUtilizador1.apresentarListaencomendasJaPreparadas());
                            try{
                            view.print(atualUtilizador1.apresentarListaEncomendasPorAceitar());
                            }catch(ListaVaziaException e){
                                view.printErro(e.getMessage());
                            }
                            break;

                        case 3:
                            //Ver registo encomendas entregues
                            view.print(ta.registoGeral(nomeCod));
                            break;

                        case 4:
                            //ver registos entre certas datas
                            //ver entregas efetuados por tal voluntario ou tranportadora
                            String entregador = view.perguntaString ("Quem foi o entregador?\n Diga o seu código.\n");
                            LocalDate dI= view.scanDataInicial();
                            LocalDate dF= view.scanDataFinal();
                            view.print(ta.verRegistos(nomeCod,entregador,dI,dF));  //codigo data data
                            break;


                        case 5:
                            //Ver top 'x'
                            criteriosdeComparacao (nomeCod , ta, view );
                            break;
                        
                        case 6:
                            // voltar atraz
                            break;

                        default:
                            break;
                    }
                
                
                
                case 6:
                    //Voltar ao menu inicial
                    sair = true;
                    break;

                default:
                    break;
            
            }
        }
    }


    /**
     * metodo que permite aos Voluntarios realizarem as operaçoes para si disponiveis
     */
    private static void executaVoluntario(String nomeCod ,TrazAqui ta,View view)
            
    {
        boolean sair = false;
        while (!sair){
            switch(view.menuVoluntario()){
                case 1:
                    //Sinalizar disponibilidade de entrega
                    Boolean b = view.pergunta("Qual a sua disponibilidade atual?\nEstá disponivel (true/false)");
                    try {
                        view.print(ta.alteraDisponibilidade(nomeCod ,b));
                    } catch (Exception e) {
                        view.printErro(e.getMessage());
                    }
                    break;

                case 2:
                    //Ir buscar certa encomenda a uma loja dentro do seu raio
                    try {
                    if (ta.getDisponibilidade(nomeCod) == false){
                        throw new MudancaDisponibilidadeException ("Mude a sua disponibilidade antes");
                    }
                    }
                    catch (MudancaDisponibilidadeException m) {
                        view.printErro(m.getMessage());
                        break;
                    }



                    if (ta.getJaFoiBuscar(nomeCod).size() > 1){
                        view.print("Excedeu a sua capacidade de Encomendas, entregue as atuais antes de pegar em mais");
                    }


                    //Aceitar certa encomenda
                    try {
                        view.print("Qual a encomenda que pretende aceitar? escreva o codigo dela");
                        view.apresentaEncomendasParaEntregar ( ta.escolherEncomendaAEntregar(nomeCod));
                        String escolhaEncVoluntario = view.scan();

                        if (!ta.escolherEncomendaAEntregar(nomeCod).contains(ta.getEncomendaQuePrecisaSerEntregue(escolhaEncVoluntario))){
                            throw new EncomendaInvalidaException("Encomenda invalida, porfavor escolha uma da lista fornecida");
                        }

                        
                        if (ta.getEncomendaQuePrecisaSerEntregue(escolhaEncVoluntario).getEMedica() && !(ta.aceitoTransporteMedicamentos(nomeCod))){
                            view.print("Voce nao pode transportar encomendas medicas!");
                            break;
                        }

                        ta.aceitarEncomenda(nomeCod , escolhaEncVoluntario);
                        ta.addJaFoiBuscar(nomeCod , escolhaEncVoluntario);
                        ta.removeParaLevar(nomeCod ,escolhaEncVoluntario);
                        ta.removeDaFila ( nomeCod , escolhaEncVoluntario);
                    }

                    catch (Exception e){
                        view.printErro(e.getMessage());
                        break;
                    }
                
                    


                    break;
                    
                    
                case 3:
                    //"3 - Transportar encomenda "
                    view.print("Qual a encomenda que pretende Transportar?");
                    try {
                    view.print (ta.getJaFoiBuscar(nomeCod).toString());
                    String escolhaEncParaTransportar = view.scan();

                    if (!ta.getJaFoiBuscar(nomeCod).contains(escolhaEncParaTransportar)){
                        throw new EncomendaInvalidaException("Encomenda invalida, porfavor escolha uma da lista fornecida");
                    }

                    ta.removeJaFoiBuscar(nomeCod , escolhaEncParaTransportar);

                    Encomenda encSendoEntregue = ta.getEncomendaQuePrecisaSerEntregue(escolhaEncParaTransportar);
                    ta.removeEncomendaQuePrecisaSerEntregue(encSendoEntregue);

                    Utilizador utilizadorQuePediuEncomenda = ta.getUtilizadorQuePediuEncomenda (encSendoEntregue);
                    Lojas lojaQuePreparouEncomenda = ta.getLojaQuePreparouEncomenda(encSendoEntregue);


                    double taxaDeEntrega = 0;
                    double kmsPercorridos1 = ta.kmsPercorridos(nomeCod , utilizadorQuePediuEncomenda.getCoordenadas() , lojaQuePreparouEncomenda.getCoordenadas());
                    double tempoDemorado1 = 0;

                    if (lojaQuePreparouEncomenda.getClass().getName().equals("LojaComFila")){
                        LojaComFila lojaComFilaAtual = (LojaComFila) lojaQuePreparouEncomenda;
                        tempoDemorado1 += ta.tempoRecolherEntrega(nomeCod, lojaComFilaAtual , met, estadoCovid);
                        lojaComFilaAtual.addTempoEspera(estadoCovid);
                    }

                    tempoDemorado1 += ta.tempoChegadaLoja(nomeCod , lojaQuePreparouEncomenda,met);
                    tempoDemorado1 += ta.tempoChegadaUtilizador(nomeCod , lojaQuePreparouEncomenda, utilizadorQuePediuEncomenda, met);
                    

                    ta.addToRegistoUser(utilizadorQuePediuEncomenda.getId() , nomeCod , encSendoEntregue.clone(), 0, LocalDate.now(),kmsPercorridos1,taxaDeEntrega, tempoDemorado1);
                    ta.addToRegistoUser(nomeCod ,nomeCod , encSendoEntregue.clone(), 0, LocalDate.now(), kmsPercorridos1,taxaDeEntrega, tempoDemorado1);
                    ta.addToRegistoUser(lojaQuePreparouEncomenda.getId() ,nomeCod , encSendoEntregue.clone(), 0, LocalDate.now(), kmsPercorridos1,taxaDeEntrega,tempoDemorado1);
                                                
                                                
                    view.print("Encomenda Entregue!");

                }catch (EncomendaInvalidaException e2){
                    view.printErro(e2.getMessage());
                }
                    
                    break;


                case 4:
                    //consultas avançadas
                    switch(view.consultarDadosVoluntario())
                    {
                        case 1:
                        //ver perfil
                        Voluntarios v = (Voluntarios) ta.getUser(nomeCod);
                        view.print(v.toString());
                        break;
                        case 2:
                        //2 - Ver as minhas classificacoes
                        view.print(ta.transportadorClassificacoes(nomeCod));
                        break;
                        case 3:
                        //3 - Ver registos de encomendas entregues
                        view.print(ta.registoGeral(nomeCod));
                        break;

                        case 4:
                            //ver registos entre certas datas
                            //ver entregas efetuados por tal voluntario ou tranportadora
                            String entregador = view.perguntaString ("Quem foi o entregador?\n Diga o seu código.\n");
                            LocalDate dI= view.scanDataInicial();
                            LocalDate dF= view.scanDataFinal();
                            view.print(ta.verRegistos(nomeCod,entregador,dI,dF));  //codigo data data
                            break;

                        case 5:
                            criteriosdeComparacao (nomeCod , ta, view );
                            break;
                        
                        case 6:
                            //Voltar Atraz
                            break;

                        default:
                            break;
                    }
                    break;

                case 5:
                    //voltar ao menu inicial
                    sair = true;
                    break;
            }
        }
    }


    /**
     * metodo que permite as Lojas realizarem as operaçoes para si disponiveis
     */
    private static void executaLojas(String nomeCod ,TrazAqui ta,View view) 
    {
        boolean sair = false;
        while (!sair){
            switch(view.menuLoja()){
                                                    
                case 1:
                    //sinalizar encomenda preparada
                    try {
                        view.print("Escolha a encomenda ja preparada (exx)");
                        view.print(ta.LojaEncomendasPreparacao(nomeCod));
                        String encomendaPreparada = view.scan();
                        if (!ta.LojaEncomendasPreparacao(nomeCod).contains(encomendaPreparada)){
                            throw new EncomendaInvalidaException("Encomenda invalida, porfavor escolha uma da lista fornecida");
                        }
                        ta.encomendaPreparada(nomeCod, encomendaPreparada);
                        break;
                    }
                    catch (EncomendaInvalidaException e1){
                        view.printErro(e1.getMessage());
                    }
                    catch (ListaVaziaException e2){
                        view.printErro(e2.getMessage());
                    }


                case 2:
                    //avançadas
                    switch(view.consultarDadosLoja()){
                        case 1:
                            // Ver o meu perfil
                            if (ta.getUser(nomeCod).getClass().getName().equals("LojaComFila")){
                                LojaComFila l = (LojaComFila) ta.getUser(nomeCod);
                                view.print(l.toString());
                            }
                            else{
                                Lojas l = (Lojas) ta.getUser(nomeCod);
                                view.print(l.toString());}

                            break;
                        case 2:
                            //Ver total que a minha loja faturou nesta app
                            view.print("\nO total Faturado nesta app foi:" + ta.lojaTotalFaturado(nomeCod));
                            break;
                            
                        case 3:
                            //ver total pessoas na fila
                            view.print("\nNumero de pessoas na fila:" + ta.lojaSizePreparacao(nomeCod));
                            break;
                        
                        case 4:
                            //ver registos entre certas datas
                            //ver entregas efetuados por tal voluntario ou tranportadora
                            String entregador = view.perguntaString ("Quem foi o entregador?\n Diga o seu código.\n");
                            LocalDate dI= view.scanDataInicial();
                            LocalDate dF= view.scanDataFinal();
                            view.print(ta.verRegistos(nomeCod,entregador,dI,dF));  //codigo data data
                            break;

                        case 5: 
                            //Ver registos de encomendas preparadas
                            view.print("\nRegistos:" + ta.registoGeral(nomeCod));
                            break;
                        case 6:
                            //ver encomendas em preparacao
                            try {
                                view.print("\nEncomendas em preparacao:\n" + ta.lojaEncomendasPreparacao(nomeCod));
                            } catch (Exception e) {
                                view.printErro(e.getMessage());
                            }
                            break;
                        case 7:
                            criteriosdeComparacao (nomeCod , ta, view );
                            break;
                        case 8:
                            //voltar atraz
                            break;
                        default:
                            break;
                    }

                    break;

                case 3:
                    //ir para menu principal
                    sair = true;
                    break;
        
            }
        }
    }


    /**
     * metodo que calcula o Top n por um criterio escolhido
     */
    public static void criteriosdeComparacao (String nomeCod ,TrazAqui ta,View view){

        switch( view.perguntaInt( "Escolha o criterio de ordenacao:\n1 -> Numero de Encomendas\n2 -> Numero de Km efetuados\n")){

            case 1:

                int n = view.perguntaInt("Escolha o numero de elementos a serem considerados no Top\n");
                String top;
                switch (view.perguntaInt("Escolha o que considerar:\n1 -> User , em geral , mais ativo na app\n2 -> Utilizadores (encomendas solicitadas)\n3 -> Voluntarios (encomendas transportadas)\n4 -> Transportadoras (encomendas transportadas)\n5 -> Lojas (encomendas criadas)\n")){
                    
                    case 1:
                        top = ta.verTopN(nomeCod , "User" , n);
                        view.print(top);
                        break;
                    case 2:
                        top = ta.verTopN( nomeCod , "Utilizador" , n);
                        view.print(top);
                        break;
                case 3:
                        top = ta.verTopN( nomeCod , "Voluntarios", n);
                        view.print(top);
                        break;
                case 4:
                        top = ta.verTopN( nomeCod , "Transportadoras" , n);
                        view.print(top);
                        break;
                case 5:
                        top = ta.verTopN( nomeCod , "Lojas" , n);
                        view.print(top);
                        break;
                default:
                        break;
                }
            break;

        case 2:
            
        n = view.perguntaInt("Escolha o numero de elementos a serem considerados no Top\n");
        switch (view.perguntaInt("Escolha os users a considerar:\n1 -> Voluntarios & Transportadoras\n2 -> Voluntarios\n3 -> Transportadoras\n")){

            case 1:
            top = ta.verTopNKm( nomeCod , "Transportador" , n);
            view.print(top);
            break;
        case 2:
            top = ta.verTopNKm(nomeCod , "Voluntarios" , n);
            view.print(top);
            break;
        case 3:
            top = ta.verTopNKm(nomeCod , "Transportadoras" , n);
            view.print(top);
            break;
        default:
            break;
    }
    break;

        default:
            break;               
    }
}

}

