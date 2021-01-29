package Controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import Exceptions.*;
import Model.*;
import View.*;

public class Controller {
    private TrazAqui model;
    private User user;
    private Lojas loja;
    private Voluntarios voluntario;
    private EmpresasTrans empresa;
    private List<String> paraAvaliar; // array que vai ter os codigos dos voluntarios e empresas para fazer o rating
    private List<LinhaEncomenda> paraEncomenda; //array que vai ter as linhas de encomenda para adicionar à encomenda
    private Menu menu;
    

    public Controller(TrazAqui model) {
        this.menu = new Menu();
        this.model = model;
    }

    public void run(){
        String error = "";
        int tentativas = 0;
        this.paraAvaliar = new ArrayList<>();
        this.paraEncomenda = new  ArrayList<>();
        while(this.menu.getRun()) {
            switch (menu.getMenu()){
                case LoginUtilizador:
                    try {
                        NewLogin r = menu.newLogin(error);
                        if (tentativas >= 3){
                        tentativas = 0;
                        menu.showString("Excedeu o numero de tentativas!\nPressione ENTER para tentar novamente.");
                        menu.back();
                        }
                        this.user = model.logIn(r.getUser(), r.getPassword());
                        menu.selectOption(Menu.MenuIndice.MenuCliente);
                        error = "";
                    }
                    catch (UserInexistenteException e){ error = "Username invalido"; tentativas++; }
                    catch (PasswordErradaExecption e){ error = "Password Invalida"; tentativas++; }
                    catch (NaoExisteEmailException e){ tentativas++; error = "Não existe email"; }
                    break; 
                
                case LoginLoja:
                    try {
                        NewLogin l = menu.newLogin(error);
                        if (tentativas >= 3){
                        tentativas = 0;
                        menu.showString("Excedeu o numero de tentativas!\nPressione ENTER para tentar novamente.");
                        menu.back();
                        }
                        this.loja = model.logInL(l.getUser(), l.getPassword());
                        menu.selectOption(Menu.MenuIndice.MenuLoja);
                        error = "";
                    }
                    catch (NaoExisteLojaException e){ error = "Username invalido"; tentativas++; }
                    catch (PasswordErradaExecption e){ error = "Password Invalida"; tentativas++; }
                    break;

                case LoginEmpresa:
                    try {
                        NewLogin e = menu.newLogin(error);
                        if (tentativas >= 3){
                        tentativas = 0;
                        menu.showString("Excedeu o numero de tentativas!\nPressione ENTER para tentar novamente.");
                        menu.back();
                        }
                        this.empresa = model.logInN(e.getUser(), e.getPassword());
                        menu.selectOption(Menu.MenuIndice.MenuEmpresa);
                        error = "";
                    }
                    catch (NaoExisteEmpresaException e){ error = "Empresa invalido"; tentativas++; }
                    catch (PasswordErradaExecption e){ error = "Password Invalida"; tentativas++; }
                    break;
                    
                case LoginVoluntario:
                    try {
                        NewLogin e = menu.newLogin(error);
                        if (tentativas >= 3){
                        tentativas = 0;
                        menu.showString("Excedeu o numero de tentativas!\nPressione ENTER para tentar novamente.");
                        menu.back();
                        }
                        this.voluntario = model.logInV(e.getUser(), e.getPassword());
                        menu.selectOption(Menu.MenuIndice.MenuVoluntario);
                        error = "";
                    }
                    catch (NaoExisteVoluntarioException e){ error = "Voluntário invalido"; tentativas++; }
                    catch (PasswordErradaExecption e){ error = "Password Invalida"; tentativas++; }
                    break;

                case RegistarCliente:
                    try {
                        User utilizador = menu.registarCliente(error);
                        this.model.verificaMail(utilizador.getEmail());
                        this.model.addUserCriado(utilizador);
                        this.menu.showString("Registado com sucesso\nO seu numero de utilizador e: " + utilizador.getCodUtilizador());
                        menu.back();
                        error = "";
                    }
                    catch (EmailExistenteException e) { error = "Email já utilizado"; }
                    catch (EmailInvalidoException e) { error = "Email invalido"; }
                    break;
                
                case RegistarVoluntario:
                    try {
                        Voluntarios v = menu.registarVoluntario(error);
                        this.model.addVoluntarioCriado(v);
                        this.menu.showString("Registado com sucesso\nO seu numero de utilizador e: " + v.getCodVoluntario());
                        menu.back();
                        error = "";
                    }
                    catch (OpcaoInvalidaException e){ error = "Opcao Invalida"; }
                    break;

                case RegistarTransportadora:
                    try {
                        EmpresasTrans emp = menu.registarTransportadora(error);
                        this.model.addEmpresaCriada(emp);
                        this.menu.showString("Registado com sucesso\nO seu numero de utilizador e: " + emp.getCodEmpresa());
                        menu.back();
                        error = "";
                    }
                    catch (OpcaoInvalidaException e){ error = "Opcao Invalida"; }
                    break;

                case RegistarLoja:
                    try {
                        Lojas l = menu.registarLoja(error);
                        this.model.addLojaCriada(l);
                        this.menu.showString("Registado com sucesso\nO seu numero de utilizador e: " + l.getCodLoja()); 
                        menu.back();
                        error = "";
                    }
                    catch (OpcaoInvalidaException e){ error = "Opcao Invalida"; }
                    break;

                    
                case HistoricoEncomendas:
                    try{
                        List<Encomendas> encs = this.model.getListEncUtilizador(this.user);
                        this.menu.showString(encs.toString());
                        menu.back();
                        error = "";
                    }
                    catch(UserInexistenteException e){ error = "O utilizador nao existe"; }
                    break;
                

                case HistoricoEncomendasIntervalo: 
                    try{
                    IntervaloDeTempo it = this.menu.getTimeInterval(error);
                    List<Encomendas> encs = this.model.getListEncUtilizaforDH(this.user, it.getInicio(), it.getFim());
                    this.menu.showString(encs.toString());
                    menu.back();
                    error = ""; 
                    }
                    catch(TempoIntervaloInvalidoException e){error = "Intervalo Inválido"; }
                    catch(UserInexistenteException e){ error = "O utilizador nao existe"; }
                    break;
                        
                case HistoricoEncomendasLoja:
                    try{
                        List<Encomendas> encs = this.model.getListEncLoja(loja);
                        this.menu.showString(encs.toString());
                        menu.back();
                        error = "";
                    }
                    catch(NaoExisteLojaException e){ error = "A loja não existe"; }
                    break;
                
                case HistoricoEncomendasLojaIntervalo:
                    try{
                        IntervaloDeTempo it = this.menu.getTimeInterval(error);
                        List<Encomendas> encs = this.model.getListEncLojaDH(loja, it.getInicio(), it.getFim());
                        this.menu.showString(encs.toString());
                        menu.back();
                        error = ""; 
                    }
                        catch(TempoIntervaloInvalidoException e){error = "Intervalo Inválido"; }
                        catch(NaoExisteLojaException e){ error = "A loja nao existe"; }
                        break;
                    
                case HistoricoEncomendasEmpresa:
                    try{
                        System.out.println("Estou aqui");
                        Set<Encomendas> encos = this.model.getListEncEmpresas(empresa);
                        this.menu.showString(encos.toString());
                        menu.back();
                        error = "";
                    }
                    catch(NaoExisteEmpresaException e){ error = "A Empresa não existe"; }
                    break;
                
                case HistoricoEncomendasEmpresaIntervalo:
                    try{
                        IntervaloDeTempo it = this.menu.getTimeInterval(error);
                        List<Encomendas> encs = this.model.getListEncEmpresasDH(empresa, it.getInicio(), it.getFim());
                        this.menu.showString(encs.toString());
                        menu.back();
                        error = ""; 
                    }
                        catch(TempoIntervaloInvalidoException e){error = "Intervalo Inválido"; }
                        catch(NaoExisteEmpresaException e){ error = "A empresa nao existe"; }
                        break;
                
                case ClassificaoEmpresa:
                    try{
                        double media = this.model.getClassificacaoE(empresa.getCodEmpresa());
                        this.menu.showClassificacao(error,media);
                        menu.back();
                        error="";        
                    }
                    catch(NaoExisteEmpresaException e){error = "A empresa nao existe";}
                    break;

                case MudaEstadoEmpresa:
                    
                    this.model.swapState(empresa);
                    this.menu.showString("O seu estdo foi mudado com sucesso");
                    menu.back();
                    error ="";
                    break;

                case MudaCategoriaEmpresa: 
                    
                    this.model.swapStateMed(empresa);
                    this.menu.showString("A sua categoria foi alterada com sucesso");
                    menu.back();
                    error="";
                    break;

                case HistoricoEncomendasVoluntario:
                    try{
                        
                        List<Encomendas> encs = this.model.getListEncVoluntario(voluntario);
                        this.menu.showString(encs.toString());
                        menu.back();
                        error = "";
                    }
                    catch(NaoExisteVoluntarioException e){ error = "O voluntario não existe"; }
                    break;
                    
                case HistoricoEncomendasVoluntarioIntervalo:
                    try{
                        IntervaloDeTempo it = this.menu.getTimeInterval(error);
                        List<Encomendas> encs = this.model.getListEncVoluntariosDH(voluntario, it.getInicio(), it.getFim());
                        this.menu.showString(encs.toString());
                        menu.back();
                        error = ""; 
                    }
                        catch(TempoIntervaloInvalidoException e){error = "Intervalo Inválido"; }
                        catch(NaoExisteVoluntarioException e){ error = "O voluntario nao existe"; }
                        break;

                case ClassificacaoVoluntario:
                    try{
                        double media = this.model.getClassificacaoV(voluntario.getCodVoluntario());
                        this.menu.showString("A sua classificacao e:" + media);
                        menu.back();
                        error="";        
                    }
                    catch(NaoExisteVoluntarioException e){error = "O voluntario nao existe";}
                    break;
                
                case MudaEstadoVoluntario: //Se esta livre ou nao
                    
                    this.model.swapState(voluntario);
                    this.menu.showString("O seu estado foi mudado com sucesso");
                    menu.back();
                    error ="";
                    break;
                
                case MudaCategoriaVoluntario: 
                    
                    this.model.swapStateMed(voluntario);
                    this.menu.showString("A sua categoria foi alterada com sucesso");
                    menu.back();
                    error="";
                    break;

                case Faturacao:
                    try{
                        IntervaloDeTempo it = this.menu.getTimeInterval(error);
                        double faturacao = this.model.getTotalFaturadoE(empresa, it.getInicio(), it.getFim());
                        String faturacaoS = " O total faturado pela empresa " + empresa.getNome() + " e: " + faturacao;
                        //this.menu.showFaturacao(empresa, faturacao, error);
                        this.menu.showString(faturacaoS);
                        menu.back();
                        error = "";
                    }
                    catch(TempoIntervaloInvalidoException e) {error = "Intervalo Inválido"; }
                    catch(NaoExisteLojaException | UserInexistenteException j) {error = "Impossivel fazer faturcao";}
                    break;
                
                case Best10Utilizadores:
                    try{
                        Set<User> melhores = this.model.getBestClientNrEnc();
                        List<String> res = new ArrayList<>();
                        for(User u : melhores){
                            res.add(u.getCodUtilizador());
                        }
                        System.out.print("\033\143");
                        for(int i=0; i<10 && i<res.size();i++){
                            String id = res.get(i);
                            User client = this.model.getUser(id);
                            System.out.println(id + "->" +client.getEncFeitas().size() + "->" +client.getNome());
                        }
                        this.menu.showString2();
                        menu.back();
                        error = "";
                    }catch(UserInexistenteException u){error = "Nao existe Utilizador";}
                    break;
                
                case Best10Empresas:
                    try{
                        Set<EmpresasTrans> best = this.model.getBestEmpresaKm();
                        List<String> result = new ArrayList<>();
                        for(EmpresasTrans e : best){
                            result.add(e.getCodEmpresa());
                        }
                        System.out.print("\033\143");
                        for(int i=0; i<10 && i<result.size();i++){
                            String id = result.get(i);
                            EmpresasTrans empresa = this.model.getTransportadora(id);
                            System.out.println(id + "->" + empresa.getNrKmsFeitos() + "->" + empresa.getNome());
                        }
                        this.menu.showString2();
                        menu.back();
                        error = "";
                    }catch(NaoExisteEmpresaException u){error = "Nao existe Utilizador";}
                    break;
                
                case AddProduto:
                    try{
                        LinhaEncomenda le = this.menu.registarProduto(error);
                        this.model.adicionaProduto(loja.getCodLoja(), le);
                        this.menu.showString("Produto registado com sucesso");
                        menu.back();
                        error = "";
                    }catch(JaExisteProdutoException e){error = "Ja existe produto com esse codigo";}
                    break;

                case FazerEncomenda:
                    try{
                        List<Lojas> lojas = this.model.getLojas();
                        this.menu.showLojas(lojas,error);
                        String LojaEscolhida =  this.menu.escolheLoja();
                        this.loja = this.model.getLoja(LojaEscolhida);
                        this.menu.selectOption(Menu.MenuIndice.MenuPedido);
                        error = "";
                    }
                    catch(NaoExisteLojaException e){
                        System.out.println(error = "Nao existe loja");
                    }
                    break;

                case Pedido:
                    try{
                        
                        if(this.paraEncomenda.isEmpty()){
                            this.menu.showString("Não escolheu nenhum produto!");
                            this.menu.back();
                        }
                        else{

                            Encomendas e = this.menu.registaEncomenda(error,user.getCodUtilizador(),loja.getCodLoja());
                            for(LinhaEncomenda linn : paraEncomenda){
                                e.adicionaLinha(linn);
                            }
                            Encomendas eRused = this.model.addEncomendaCriada(e);
                            String paraRate = this.model.utFazPedidoE(eRused,user.getCodUtilizador());
                            if( (paraRate!=null) && (paraRate.charAt(0) == 'v' || paraRate.charAt(0) == 'V')){
                                this.menu.showString("A sua encomenda será transportada pelo voluntario:" + paraRate);
                            }
                            if(paraRate!=null) this.paraAvaliar.add(paraRate);
                            this.menu.selectOption(Menu.MenuIndice.MenuCliente);
                            error = "";
                        }

                    }catch(JaExisteEncomendaException e){ error = "Ja existe encomenda";}
                     catch(UserInexistenteException ue) { error = "Não existe utilizador";}
                     catch(UserForaDeAlcanceException uf) { 
                         this.menu.showString("Não existem volunatrios ou empresas no seu alcance!Tente mais tarde");
                         this.menu.selectOption(Menu.MenuIndice.MenuCliente);
                        }
                     catch(NaoExisteLojaException nl) { error = "Não existe loja";}
                     catch(NaoExisteEncomendaException e) { error = "Não existe encomenda";}
                     catch(OpcaoInvalidaException e) { error = "Opcao Invalida";}
                     break;
                
                case EscolhaProduto:
                    try{
                        List<LinhaEncomenda> produtos = this.model.getProdutos(this.loja.getCodLoja());
                        if(produtos.size() == 0){
                            this.menu.showString("Esta loja ainda não tem produtos disponiveis!");
                            this.menu.selectOption(Menu.MenuIndice.MenuCliente);
                        }
                        else{
                            
                            this.menu.showProdutos(produtos,error);
                            String produtoEscolhido =  this.menu.escolheProduto();
                            int quantidade = this.menu.getQuantidade();
                            Map<String,LinhaEncomenda> produtosMap = new HashMap<>(); 
                            for(LinhaEncomenda line : produtos){
                                produtosMap.putIfAbsent(line.getCodProdu(), line); 
                            }
                            LinhaEncomenda paraGuardar = produtosMap.get(produtoEscolhido);
                            if(paraGuardar == null){
                                this.menu.showString("O produto que escolheu nao existe!");
                                menu.back();
                            }
                            else{ 
                                paraGuardar.setQuantidade(quantidade);
                                this.paraEncomenda.add(paraGuardar); 
                                paraGuardar = null;
                                this.menu.back();
                                error = "";
                            }
                        }
                    }
                    catch(NaoExisteLojaException ne){ error = "Nao existe Loja"; }
                    break; 
                
                case Rating:
                    
                        if(this.paraAvaliar.isEmpty()){
                            this.menu.showString("Não tem entidades para Avaliar!");
                            this.menu.back();
                        }
                        else{
                            try{
                            for(String cod: this.paraAvaliar){
                                if (cod.charAt(0) == 'v' || cod.charAt(0) == 'V'){
                                    Voluntarios v = this.model.getVoluntario(cod);
                                    int classificacao = this.menu.fazRate(v.getNome(),error);
                                    v.rate(classificacao); 
                                }
                                else if (cod.charAt(0) == 't' || cod.charAt(0) == 'T'){
                                         EmpresasTrans trans = this.model.getTransportadora(cod);
                                         int classificacao = this.menu.fazRate(trans.getNome(),error);
                                         trans.rate(classificacao); 
                                     }

                                }
                            this.paraAvaliar = new ArrayList<>();
                            this.menu.back();
                            }catch(ClassificacaoInvalidaException ci){ error = "Classificação inválida"; }
                            catch( NaoExisteVoluntarioException| NaoExisteEmpresaException e ){}

                            }

                        break;
                
                default:
                    this.menu.parser();
                    break;
            }
        }
    }
}

