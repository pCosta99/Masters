import java.io.IOException;
import java.util.*;
import java.io.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.io.Serializable;
import java.util.Map;
import java.util.Random;
public class Controlador implements Serializable 
{
    private static TrazAqui trazAqui;
    private static Parse p;
    public  void menu() throws IOException, ClassNotFoundException,MensagemException {
        
        int opcao = -1;
        int segundaopcao = -1;
        int terceiraopcao = -1;
        int quartaopcao = -1;
        int quintaopcao = -1;
        int sextaopcao = -1;
        int setimaopcao = -1;
        int oitavaopcao = -1;

        //MUDAR CAMINHO DO FICHEIRO NAS FUNÇÕES DE LER E GRAVAR FICHEIRO (aqui no controlador no case 4 e case 0)

        trazAqui = new TrazAqui();
        p = new Parse(); 
        try {
            trazAqui = Input.leFicheiro("D://BlueJ//Projetos//Projeto//logs.object");

        } catch (FileNotFoundException e) {
            System.out.println("parse");
            p.parse("D://BlueJ//Projetos//Projeto//logs.csv");  //carregar a cena dos logs
            trazAqui = p.getTrazAqui();
            trazAqui.adicionaFilaEsperaLojas();
            for (Utilizadores u : trazAqui.getUtilizadores().values()){    
                for(Encomenda enc: trazAqui.getEncomendas().values()){
                    if(enc.getDestinatario().equals(u.getCodigo())){     
                        u.addRegisto(enc); 
                        trazAqui.atualizaUtilizador(u);
                    }
                }
            }
        } catch (ClassNotFoundException | IOException e) {
            System.out.println(e.getMessage());
        }
        while (opcao != 0){
            View.menu();
            opcao = Input.lerInt();
            switch (opcao){
                case 1://Registar
                    while(segundaopcao !=0){
                        View.menuRegistos();
                        segundaopcao = Input.lerInt();
                        switch(segundaopcao){
                            case 1://Registar utilizador
                                View.pedirNome();
                                String nome = Input.lerString();
                                View.pedirCodigo();
                                String codigo = Input.lerString();
                                View.pedirLocalizaçaoX();
                                int x1  = Input.lerInt();
                                View.pedirLocalizaçaoY();
                                int y1  = Input.lerInt();
                                GPS loc = new GPS(x1,y1);
                                View.pedirEmail();
                                String email = Input.lerString();
                                View.pedirPass();
                                String pass = Input.lerString();
                                Set<Encomenda> registos =  new HashSet<Encomenda>();
                                Utilizadores u1 = new Utilizadores(nome, codigo, loc, email, pass, registos);
                                try{  
                                    trazAqui.registaUtilizador(u1);    
                                }
                                catch(Exception e){
                                    throw new MensagemException("Utilizador já está registado. \n");
                                }
                                break;
                            
                            case 2: //Registar voluntario
                                View.pedirCodigo();
                                String codigoV = Input.lerString();
                                View.pedirNome();
                                String nomeV = Input.lerString();
                                View.pedirRaio();
                                double raioV = Input.lerDouble();
                                boolean disponivel = true;
                                View.pedirLocalizaçaoX();
                                int x2  = Input.lerInt();
                                View.pedirLocalizaçaoY();
                                int y2  = Input.lerInt();
                                GPS locV = new GPS(x2,y2);
                                View.pedirAptoMed();
                                boolean medicamentos = Input.lerBoolean();
                                List<Integer> avaliacoesV = new ArrayList<Integer>();
                                Set<Encomenda> registosV =  new HashSet<Encomenda>();
                                View.pedirPass();
                                String passV = Input.lerString();
                                Voluntarios v1 = new Voluntarios(codigoV, nomeV, locV, raioV, disponivel, medicamentos, avaliacoesV, registosV, passV);
                                try{    
                                    trazAqui.RegistaVoluntario(v1);
                                }
                                catch(Exception e){
                                    throw new MensagemException("Voluntario já está registado. \n");
                                }
                                break;
                                
                            case 3://Registar Transportadora
                                View.pedirCodigo();
                                String codigoE = Input.lerString();
                                View.pedirNome();
                                String nomeE = Input.lerString();
                                View.pedirLocalizaçaoX();
                                int xE  = Input.lerInt();
                                View.pedirLocalizaçaoY();
                                int yE  = Input.lerInt();
                                GPS locE = new GPS(xE,yE);
                                View.pedirNif();
                                String nif = Input.lerString();
                                View.pedirRaio();
                                double raioE = Input.lerDouble();
                                View.pedirPrecoKm();
                                double preco = Input.lerDouble();
                                double numKms2 = 0;
                                View.pedirAptoMed();
                                boolean medicamentosE = Input.lerBoolean();
                                View.pedirMaxEnc();
                                int maxEnc = Input.lerInt();
                                Map<String,Encomenda> filaEsperaE = new LinkedHashMap<String,Encomenda>();
                                List<Integer> avaliacoesE = new ArrayList<Integer>();
                                Set<Encomenda> registosE =  new HashSet<Encomenda>();
                                View.pedirPass();
                                String passT = Input.lerString();
                                Map<Double,Encomenda> lucro = new HashMap<>();
                                EmpresasTransportadoras e1 = new EmpresasTransportadoras(codigoE, nomeE, locE, nif, raioE, preco,medicamentosE, maxEnc, filaEsperaE, avaliacoesE, registosE,passT,lucro,numKms2);
                                try{    
                                    trazAqui.RegistaTransportadora(e1);
                                }
                                catch(Exception e){
                                    throw new MensagemException("Transportadora já está registada. \n");
                                }
                                break;
                                
                            
                            case 4://Registar Loja
                                View.pedirNome();
                                String nomeL = Input.lerString();
                                View.pedirCodigo();
                                String codigoL = Input.lerString();
                                View.pedirLocalizaçaoX();
                                int xL  = Input.lerInt();
                                View.pedirLocalizaçaoY();
                                int yL  = Input.lerInt();
                                GPS locL = new GPS(xL,yL);
                                Map<String,Encomenda> filaEsperaL = new LinkedHashMap<String,Encomenda>();
                                double tempo = 5;
                                View.pedirPass();
                                String passL = Input.lerString();
                                Lojas l = new Lojas(nomeL,codigoL,locL,filaEsperaL,tempo, passL);
                                try{    
                                    trazAqui.registaLoja(l);
                                }
                                catch(Exception e){
                                    throw new MensagemException("Loja já está registada. \n");
                                }
                                break;
                                
                                
                            case 0:
                                break;
                            
                            default:
                                 View.limpaEcra();
                                 View.opcaoInvalida();
                                 break;
                                
                        }
                    }
                break;    
                case 2://Login
                    while(terceiraopcao!=0){                       
                        View.menuLogin();
                        terceiraopcao = Input.lerInt();
                        switch(terceiraopcao){
                            case 1://Login Utilizador
                                View.pedirEmail();
                                String email = Input.lerString();
                                View.pedirPass();
                                String pass = Input.lerString();
                                if(!trazAqui.existeCredenciaisUtilizador(email,pass)){
                                    View.utilizadorNaoEncontrado();
                                    break;
                                }   
                                else{
                                    Utilizadores u = trazAqui.devolveUtilizadorCredenciais(email,pass); //devolve utilizador registado com o email e passe dados
                                    quartaopcao = -1;
                                    while(quartaopcao !=0){
                                        View.menuLoginUtilizador();
                                        quartaopcao = Input.lerInt();
                                        switch(quartaopcao){
                                            case 1://Solicitar entrega
                                                List<String> novo = trazAqui.utilizadorEncomendasPedidas(u);
                                                if(novo.isEmpty()){
                                                    System.out.println("Não temos registos de qualquer pedido de encomenda feito por si a uma loja ou já aceitou previamente a sua encomenda pelo que não é possível qualquer transporte");
                                                    break;
                                                }
                                                else{
                                                    int conta = 0;
                                                    for(String s : novo){
                                                        if(trazAqui.getAceite().contains(s)){
                                                            conta++;
                                                        }
                                                    }
                                                    if(conta!=novo.size()){
                                                        System.out.println("Estas são as encomendas registadas cujo transporte é possível");
                                                        for (String codEncomenda : novo){
                                                            if(!trazAqui.getAceite().contains(codEncomenda)){
                                                                System.out.println(codEncomenda);
                                                            }
                                                        }

                                                        View.pedirEncomenda();
                                                        String codEnc = Input.lerString();
                                                        if(!trazAqui.existeEncomenda(codEnc)){//vê se a encomenda dada pela utilizador está na app
                                                            View.encomendaNaoEncontrada();
                                                            break;
                                                        }
                                                        else{
                                                            Encomenda enc = trazAqui.getEncomenda(codEnc);//devolve encomenda registada na app com o código igual ao dado pelo utilizador
                                                            Lojas l = trazAqui.getLojaInfo(enc.getVendedor());//devolve loja registada na app com o código igual ao dado pelo utilizador 
                                                            trazAqui.solicitaEncomenda(enc,u,l);//pede encomenda adicionando-a à lista de espera da loja (ver se é preciso mudar u.pedeEncomenda)
                                                            if(l.encomendaPronta(u)){//ve se a encomenda está em primeiro na fila de espera e só nesse caso ela pode ser entregue
                                                                boolean flag = true;
                                                                boolean flag2 = true;
                                                                System.out.println("A sua encomenda está pronta para ser entregue");
                                                                if(!trazAqui.voluntariosDisponiveis().isEmpty()){ //vê se há voluntarios disponiveis  na App (só vai para as transportadoras se não houver voluntários disponiveis)
                                                                    List<Voluntarios> disponiveis = trazAqui.voluntariosDisponiveis();
                                                                    for(Voluntarios v : disponiveis){
                                                                        if(v.recolheEncomenda(enc,l,u)){//vê se o voluntario pode recolher a encomenda
                                                                            v.fazTransporteEncomenda(enc);//voluntário transporta a encomenda, adicionando-a aos registos 
                                                                            trazAqui.atualizaVoluntario(v);//atualiza informação do voluntario na App
                                                                            flag = false;
                                                                            flag2=false;
                                                                            System.out.println("A sua encomenda está a ser levada pelo voluntario " + v.getNome() + " e irá demorar aproximadamente " + v.tempoDeEntrega(enc,l,u) + " minutos");
                                                                            LocalDateTime data = LocalDateTime.now();
                                                                            enc.setData(data);//atualiza data de entrega da encomenda para posterior uso
                                                                            u.addRegisto(enc); //adiciona encomenda aos registos do utilizador
                                                                            trazAqui.atualizaUtilizador(u);//atualiza informação do utilizador na App
                                                                            l.retiraFilaEspera(enc);//tira encomenda da fila de espera da loja visto que já foi entregue
                                                                            trazAqui.atualizaLoja(l);
                                                                            View.pedeClassificaçao();
                                                                            int clas = Input.lerInt();
                                                                            trazAqui.classificarTransporte(v.getCodigo(),clas);
                                                                            EncomendasAceites aceite = new EncomendasAceites(enc.getCodEncomenda());
                                                                            trazAqui.addAceite(aceite);
                                                                            break;
                                                                        }
                                                                    }
                                                                }
                                                                if(flag2){
                                                                    System.out.println("Não há voluntários disponiveis");
                                                                    for(Transporte t : trazAqui.getTransportes().values()){
                                                                        if(t.tipoTransporte()=="EmpresasTransportadoras"){//procura uma transportadora registada na app
                                                                            EmpresasTransportadoras trans = (EmpresasTransportadoras) t;
                                                                            System.out.println("Aceita pagar o valor de " + trans.precoViagem(enc,l,u) + " euros proposto pela transportadora " + trans.getNome() + "? (responda s (sim) ou n (não) porfavor)");
                                                                            String resposta = Input.lerString();
                                                                            if(resposta.equals("s")){ //se utilizador aceitar preço proposto pela transportadora
                                                                                if(trans.recolheEncomenda(enc,l,u)){//se a transportadora aceitar levar a encomenda
                                                                                    trans.fazTransporteEncomenda(enc); //transportadora leva a encomenda, adicionando-a aos registos (não está a meter nos registos as linhas da encomenda)
                                                                                    trans. adicionaLucro(trans.precoViagem(enc,l,u),enc);
                                                                                    Double distancia = trans.getLocalizacao().distancia(trans.getLocalizacao(),l.getLocalizaçao()) + trans.getLocalizacao().distancia(l.getLocalizaçao(),u.getLocalizaçao());
                                                                                    trans. atualizaKms(distancia);
                                                                                    trazAqui.atualizaTransportadora(trans);//atualiza informação sobre a transportadora na App
                                                                                    u.addRegisto(enc);//adiciona encomenda aos registos do utilizador
                                                                                    trazAqui.atualizaUtilizador(u);//atualiza informação do utilizador na App
                                                                                    flag = false;
                                                                                    LocalDateTime data = LocalDateTime.now();
                                                                                    enc.setData(data);//atualiza data de entrega da encomenda para posterior uso
                                                                                    System.out.println("A sua encomenda está a ser levada pela transportadora " + trans.getNome() + " e irá demorar aproximadamente " + trans.tempoDeEntrega(enc,l,u) + " minutos");
                                                                                    l.retiraFilaEspera(enc);//tira encomenda da fila de espera da loja visto que já foi entregue
                                                                                    trazAqui.atualizaLoja(l);
                                                                                    View.pedeClassificaçao();
                                                                                    int clas = Input.lerInt();
                                                                                    trazAqui.classificarTransporte(trans.getCodigo(),clas);
                                                                                    EncomendasAceites aceite = new EncomendasAceites(enc.getCodEncomenda());
                                                                                    trazAqui.addAceite(aceite);
                                                                                    break;
                                                                                }
                                                                                else{
                                                                                    System.out.println("A transportadora " + trans.getNome() + " não consegue realizar a entrega da encomenda");
                                                                                    System.out.println("Vamos verificar se há mais transportadoras disponiveis");
                                                                                }
                                                                            }
                                                                            else{
                                                                                System.out.println("Vamos verificar se há mais transportadoras disponiveis");
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                                if(flag){
                                                                    l.retiraFilaEspera(enc);
                                                                    System.out.println("Infelizmente não há nenhum transporte que corresponda ao seu pedido e assim teremos de cancelar a sua encomenda");
                                                                    System.out.println("Esperamos que consigamos corresponder às suas exigências da próxima vez, obrigado por usar trazAqui!");
                                                                    break;
                                                                }  
                                                                break;
                                                            }
                                                                            
                                                            else{
                                                                System.out.println("A sua encomenda ainda não está pronta para ser entregue, agradecemos que espere alguns minutos");
                                                                break;
                                                                }
                                                        }
                                                    }
                                                    else{
                                                        System.out.println("Todas as suas encomendas registadas estão como aceites pelo que não é possivel qualquer transporte!");
                                                        break;
                                                    }
                                                }

                                            case 2:
                                                View.pedirTempoAntes();
                                                String dataAntes = Input.lerString();
                                                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                                                LocalDateTime dataAntes2 = LocalDateTime.parse(dataAntes, formatter);
                                                View.pedirTempoDepois();
                                                String dataDepois = Input.lerString();
                                                LocalDateTime dataDepois2 = LocalDateTime.parse(dataDepois, formatter);
                                                if(!u.getEncomendasTempo(dataAntes2,dataDepois2).isEmpty()){
                                                    System.out.println("Entre a data " + dataAntes + " e a data " + dataDepois + " recebeu as encomendas: ");
                                                    for(String codEncomenda : u.getEncomendasTempo(dataAntes2,dataDepois2)){
                                                        System.out.println(codEncomenda);
                                                    }
                                                }
                                                else{
                                                    System.out.println("Entre a data " + dataAntes + " e a data " + dataDepois + " não recebeu nenhuma encomenda");
                                                }
                                                break;
                                            case 3:
                                                if(!u.getRegistos().isEmpty()){
                                                    System.out.println("O seu registo de encomendas solicitadas na trazAqui é: ");
                                                    for(Encomenda enco : u.getRegistos()){
                                                        System.out.println(enco.getCodEncomenda());
                                                    }
                                                }
                                                else{
                                                    System.out.println("Ainda não realizou pedidos de encomendas na trazAqui!");
                                                }
                                                break;
                                            case 4:
                                                //gerar encomenda
                                                String codEncomenda = new String();
                                                Random rand = new Random();
                                                int aleatorio = rand.nextInt(9999);
                                                String aleatorio2 = String.valueOf(aleatorio);
                                                if(!trazAqui.getEncomendas().containsKey(aleatorio2)){
                                                    codEncomenda = aleatorio2;
                                                }
                                                else{
                                                    while(trazAqui.getEncomendas().containsKey(aleatorio2)){
                                                        int  succ = Integer.parseInt(aleatorio2);
                                                        aleatorio2 = String.valueOf(succ);
                                                    }
                                                    codEncomenda = aleatorio2;
                                                }
                                                codEncomenda = "e"+codEncomenda;
                                                View.pedirLoja();
                                                String shop = Input.lerString();
                                                //System.out.println("Insira o peso da sua encomenda.");
                                                //Double pes = Input.lerDouble();
                                                LocalDateTime date = LocalDateTime.now();
                                                //gerar linha da encomenda
                                                System.out.println("Insira código do produto da sua encomenda.");
                                                String cod = Input.lerString();
                                                System.out.println("Insira o nome do produto.");
                                                String name = Input.lerString();
                                                System.out.println("Insira a quantidade de produto que vai encomendar.");
                                                Double quant = Input.lerDouble();
                                                System.out.println("Insira o preço do produto na loja.");
                                                Double preco = Input.lerDouble();
                                                int desconto = 0;
                                                int imposto = 0;
                                                
                                                LinhaEncomenda linha = new LinhaEncomenda(cod,name,preco,quant,desconto,imposto);
                                                List<LinhaEncomenda> le = new ArrayList<LinhaEncomenda>();
                                                le.add(linha);
                                                String resposta = new String();
                                                while(resposta!="n"){
                                                    System.out.println("Deseja adicionar produtos à encomenda? (responda sim (s) ou não (n) porfavor)");
                                                    resposta = Input.lerString();
                                                    if(resposta.equals("n")){
                                                        break;
                                                    }
                                                    System.out.println("Insira código do produto da sua encomenda.");
                                                    String cod2 = Input.lerString();
                                                    System.out.println("Insira o nome do produto.");
                                                    String name2 = Input.lerString();
                                                    System.out.println("Insira a quantidade de produto que vai encomendar.");
                                                    Double quant2 = Input.lerDouble();
                                                    System.out.println("Insira o preço do produto na loja");
                                                    Double preco2 = Input.lerDouble();
                                                    int desconto2 = 0;
                                                    int imposto2 = 0;
                                                    LinhaEncomenda linha2 = new LinhaEncomenda(cod2,name2,preco2,quant2,desconto2,imposto2);
                                                    le.add(linha2);
                                                }
                                                Double pes = 0.0;
                                                for(LinhaEncomenda l : le){
                                                    pes+=l.getQE();
                                                }
                                                Encomenda enc = new Encomenda(codEncomenda,u.getCodigo(),shop,pes,date,false,le);
                                                try{    
                                                    trazAqui.registaEncomenda(enc);
                                                }
                                                catch(Exception e){
                                                    throw new MensagemException("Transportadora já está registada. \n");
                                                }
                                                System.out.println("Encomenda número " + enc.getCodEncomenda() + " adicionada com sucesso!");
                                                break;       

                                            case 0:
                                                
                                                break;

                                            default:
                                                View.limpaEcra();
                                                View.opcaoInvalida();
                                                break;
                                                
                                        }
                                    }
                                }
                                
                             break;           
                            case 2://login voluntario
                                View.pedirEmail();
                                String email2 = Input.lerString();
                                View.pedirPass();
                                String pass2 = Input.lerString();
                                if(!trazAqui.existeCredenciaisVoluntario(email2,pass2)){
                                    View.voluntarioNaoEncontrado();
                                    break;
                                }   
                                else{
                                    Voluntarios v = trazAqui.devolveVoluntarioCredenciais(email2,pass2); //devolve voluntario registado com o email e passe dados
                                    quintaopcao = -1;
                                    while(quintaopcao !=0){
                                        View.menuLoginVoluntario();
                                        quintaopcao = Input.lerInt();
                                        switch(quintaopcao){
                                            case 1:
                                                View.pedirTempoAntes();
                                                String dataAntes = Input.lerString();
                                                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                                                LocalDateTime dataAntes2 = LocalDateTime.parse(dataAntes, formatter);
                                                View.pedirTempoDepois();
                                                String dataDepois = Input.lerString();
                                                LocalDateTime dataDepois2 = LocalDateTime.parse(dataDepois, formatter);
                                                if(!v.getEncomendasTempo(dataAntes2,dataDepois2).isEmpty()){
                                                    System.out.println("Entre a data " + dataAntes + " e a data " + dataDepois + " entregou as encomendas: ");
                                                    for(String codEncomenda : v.getEncomendasTempo(dataAntes2,dataDepois2)){
                                                        System.out.println(codEncomenda);
                                                    }
                                                }
                                                else{
                                                    System.out.println("Entre a data " + dataAntes + " e a data " + dataDepois + " não entregou nenhuma encomenda");
                                                }
                                                break;

                                            case 2:
                                                System.out.println("Está diposto a levar uma encomenda? (responda s (sim) ou n (não) porfavor)");
                                                String resposta = Input.lerString();
                                                if(resposta.equals("s")){
                                                    v.setDisponivel(true);
                                                    trazAqui.atualizaVoluntario(v);
                                                    System.out.println("espera");
                                                }
                                                else{
                                                    v.setDisponivel(false);
                                                    trazAqui.atualizaVoluntario(v);
                                                }
                                                break;
                                            case 3:
                                                for (Transporte t : trazAqui.getTransportes().values()){
                                                    if(t.tipoTransporte() == "Voluntarios"){
                                                        Voluntarios vol = (Voluntarios) t;
                                                        if(vol.getCodigo().equals(v.getCodigo())){  
                                                            if(!vol.getRegistos().isEmpty()){
                                                                System.out.println("O seu registo de encomendas transportadas na trazAqui é: ");
                                                                for(Encomenda enco : vol.getRegistos()){
                                                                    System.out.println(enco.getCodEncomenda());
                                                                }
                                                            }
                                                            else{
                                                                System.out.println("Ainda não realizou transporte de encomendas na trazAqui!");
                                                            }
                                                        }
                                                    }
                                                }
                                                break;

                                            case 4:
                                                System.out.println("A sua classificação média é :" + v.classificacao());
                                                break;    

                                            case 0:
                                               
                                                break;

                                            default:
                                                View.limpaEcra();
                                                View.opcaoInvalida();
                                                break;    
                                        }
                                    }
                                }
                             break;                              
                            case 3://login transportadora
                                View.pedirEmail();
                                String email3 = Input.lerString();
                                View.pedirPass();
                                String pass3 = Input.lerString();
                                if(!trazAqui.existeCredenciaisTransportadora(email3,pass3)){
                                    View.TransportadoraNaoEncontrada();
                                    break;
                                }
                                else{
                                    EmpresasTransportadoras trans = trazAqui.devolveTransportadoraCredenciais(email3,pass3); //devolve transportadora registada com o email e passe dados
                                    sextaopcao = -1;
                                    while(sextaopcao !=0){
                                        View.menuLoginEmpresas();
                                        sextaopcao = Input.lerInt();
                                        switch(sextaopcao){
                                            case 1:
                                                View.pedirTempoAntes();
                                                String dataAntes = Input.lerString();
                                                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                                                LocalDateTime dataAntes2 = LocalDateTime.parse(dataAntes, formatter);
                                                View.pedirTempoDepois();
                                                String dataDepois = Input.lerString();
                                                LocalDateTime dataDepois2 = LocalDateTime.parse(dataDepois, formatter);
                                                if(!trans.getEncomendasTempo(dataAntes2,dataDepois2).isEmpty()){
                                                    System.out.println("Entre a data " + dataAntes + " e a data " + dataDepois + " entregou as encomendas: ");
                                                    for(String codEncomenda : trans.getEncomendasTempo(dataAntes2,dataDepois2)){
                                                        System.out.println(codEncomenda);
                                                    }
                                                }
                                                else{
                                                    System.out.println("Entre a data " + dataAntes + " e a data " + dataDepois + " não entregou nenhuma encomenda");
                                                }
                                                break;
                                                
                                            case 2:
                                                for (Transporte t : trazAqui.getTransportes().values()){
                                                    if(t.tipoTransporte() == "EmpresasTransportadoras"){
                                                        EmpresasTransportadoras emp = (EmpresasTransportadoras) t;
                                                        if(emp.getCodigo().equals(trans.getCodigo())){  
                                                            if(!emp.getRegistos().isEmpty()){
                                                                System.out.println("O seu registo de encomendas transportadas na trazAqui é: ");
                                                                for(Encomenda enco : emp.getRegistos()){
                                                                    System.out.println(enco.getCodEncomenda());
                                                                }
                                                            }
                                                            else{
                                                                System.out.println("Ainda não realizou transporte de encomendas na trazAqui!");
                                                            }
                                                        }
                                                    }
                                                }
                                                break;
                                                
                                            case 3:
                                                System.out.println("A sua classificação média é :" + trans.classificacao());
                                                break;
                                                
                                            case 4:
                                                View.pedirTempoAntes();
                                                String dataAntes3 = Input.lerString();
                                                DateTimeFormatter formatter2 = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                                                LocalDateTime dataAntes4 = LocalDateTime.parse(dataAntes3, formatter2);
                                                View.pedirTempoDepois();
                                                String dataDepois3 = Input.lerString();
                                                LocalDateTime dataDepois4 = LocalDateTime.parse(dataDepois3, formatter2);
                                                System.out.println("Entre a data " + dataAntes3 + " e a data " + dataDepois3 + " ganhou " + trans.totalDinheiroTempo(dataAntes4,dataDepois4));
                                                break;    

                                            case 0:
                                                
                                                break;

                                            default:
                                                View.limpaEcra();
                                                View.opcaoInvalida();
                                                break; 
                                        }   

                                    }
                                }   
                            break ;
                            case 4://login lojas
                                View.pedirEmail();
                                String email4 = Input.lerString();
                                View.pedirPass();
                                String pass4 = Input.lerString();
                                if(!trazAqui.existeCredenciaisLoja(email4,pass4)){
                                    View.LojaNaoEncontrada();
                                    break;
                                }
                                else{
                                    Lojas l = trazAqui.devolveLojaCredenciais(email4,pass4); //devolve loja registada com o email e passe dados
                                    setimaopcao = -1;
                                    while(setimaopcao !=0){
                                        View.menuLoginLoja();
                                        setimaopcao = Input.lerInt();
                                        switch(setimaopcao){
                                            case 1:
                                                System.out.println("Atualmente o número de pessoas na sua fila de espera é: " + l.numeroPessoasFila());
                                                break;
                                            case 2:
                                                System.out.println("Insira o código do utilizador");
                                                String codUtilizador = Input.lerString();
                                                if(trazAqui.existeUtilizadorFila(codUtilizador,l.getCodigo())){System.out.println("Neste momento há uma encomenda do utilizador " + codUtilizador + " na sua fila de espera!");}
                                                else{System.out.println("Neste momento não há uma encomenda do utilizador " + codUtilizador + " na sua fila de espera!");}
                                                break;    
                                        }
                                    }
                                }
                                break;
                                        
                                    
                            case 0:
                                
                                break ;
                                        
                            default:
                                View.limpaEcra();
                                View.opcaoInvalida();
                                    break;
                                                                    
                        }
                    }    
                opcao = -1;
                segundaopcao = -1;
                terceiraopcao = -1;
                quartaopcao = -1;
                quintaopcao = -1;
                sextaopcao = -1;
                setimaopcao = -1;
                oitavaopcao = -1;
                break;

                case 3:
                    while(oitavaopcao!=0){  
                        oitavaopcao=-1;                     
                        View.menuBonus();
                        oitavaopcao = Input.lerInt();
                        switch(oitavaopcao){
                            case 1:
                                System.out.println("O top 10 utilizadores que usaram trazAqui até agora é : ");
                                List<Utilizadores> top10 = trazAqui.utilizadoresTop10();
                                for(Utilizadores u : top10){ 
                                    System.out.println(u.getCodigo() + " com um total de " + u.getRegistos().size() + " encomendas recebidas!");
                                }
                                break;
                            case 2:
                                System.out.println("O top 10 transportadoras que usaram trazAqui até agora é : ");
                                List<Transporte> Stop10 = trazAqui.transportadorasTop10();
                                for(Transporte t : Stop10){
                                    EmpresasTransportadoras e = (EmpresasTransportadoras) t; 
                                    System.out.println(e.getCodigo() + " com um total de " + e.getNumKms() + " metros percorridos!");
                                }
                                break;
                            case 0:
                                break ;    
                        }
                    }
                    oitavaopcao = -1;
                    break;
                    
                case 0:
                    try {
                        trazAqui.gravaCSV("D://BlueJ//Projetos//Projeto//logs.csv");
                    } catch (FileNotFoundException e) {
                        System.out.println(e.getMessage());
                    }
                    try {
                        trazAqui.gravaEmFicheiro("D://BlueJ//Projetos//Projeto//logs.object");
                    } catch (FileNotFoundException e) {
                        System.out.println(e.getMessage());
                    } catch (IOException e) {
                        System.out.println(e.getMessage());
                    }
                    View.fecharPrograma();
                    break;
                
                default:
                    View.limpaEcra();
                    View.opcaoInvalida();
                    break;
            }
        }
    }
}