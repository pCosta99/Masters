package Controller;

import Model.Catalogos.IProduto;
import Model.Encomendas.Encomenda;
import Model.Encomendas.Entrega;
import Model.Encomendas.IEncomenda;
import Model.Encomendas.IEntrega;
import Model.ISistema;
import Model.Tipos.*;
import View.IAppView;
import View.INavegador;
import View.Navegador;

import java.util.*;

/**
 * Controller referente apenas à parte exclusiva ao user no projeto
 */
public class UserController implements IUserController {
    private ISistema sistema;
    private IAppView view;
    private INavegador nav;
    private User user;
    private int opcao;
    private IEncomenda encoFinal;


    public UserController() {
        this.nav = new Navegador();
        this.opcao = 0;
        this.encoFinal = new Encomenda();
    }


    public void setSistema(ISistema sistema){
        this.sistema = sistema;
    }
    public void setAppView(IAppView view) {
        this.view = view;
    }
    public void setUser(){
        String id = sistema.getQuem().getPassword().substring(0,3); //ver casos que é só um digito
        this.user = (User) sistema.getUsers().getTipo(id);
    }

    /**
     * switch com case para a opção que o user escolje do menu da view userMode. Faz scan da opção que ele escolhe depois de ver
     * o menu que foi imprimido.
     */
    public int userMode() {
        int res = 0;
        setUser();
        if(sistema.getFilaEncomendas().getEncomendas(user.getId())!=null){
            this.encoFinal = sistema.getFilaEncomendas().getEncomendaRecente(user.getId());
        }
        else{
            view.printMensagem("» Não tem pedidos de encomenda! Ou ainda estão a ser processados!" +
                    "\nPor favor aguarde... Ou faça um novo pedido!\n");
        }
        do {
            Scanner ler = new Scanner(System.in);
            view.userMode();
            opcao = ler.nextInt();
            List<IProduto> prods = new ArrayList<>();
            List<String> quantidades = new ArrayList<>();
            IEncomenda encomenda = new Encomenda();
            switch (opcao) {
                case 1: { //Encomendar algo
                    int num = catalogo(0,prods,quantidades); //se o catálogo por chamado com 0 então pretendemos visualizar os Produtos
                    res = 1;
                    if(num!=-1){
                        view.printMensagem("Enviando pedido à loja...\nPor favor aguarde");
                        view.printMensagem("\nPressione ENTER para aceder ao User Menu\nPressione 0 para voltar ao Login Menu");
                        Scanner scanner = new Scanner(System.in);
                        String aux = scanner.nextLine();
                        if(aux.equals("0")) opcao = 0;
                    }
                    break;
                }
                case 2: { //escolha do transporte
                    if(!sistema.getFilaEncomendas().existsKey(user.getId())){
                        view.printMensagem("A Loja ainda não validou o pedido.\n");
                        break;
                    }
                    HashSet<ITipo> set = new HashSet<>();
                    char op =' ';
                    view.transportes(op,set);
                    while((op!='M') && (op!= 'E')){
                        ler = new Scanner(System.in);
                        op = (ler.nextLine()).charAt(0);
                        encomenda = transporte(op);
                    }
                    if(op=='E'){
                        ler = new Scanner(System.in);
                        String escolha = ler.nextLine();
                        IEntrega entrega = new Entrega();
                        if(escolha.charAt(0)=='v') {
                            ITipo tipo = sistema.getVoluntarios().getTipo(escolha);
                            Voluntario vol = (Voluntario) tipo;
                            while(!vol.getAvailability()){
                                view.printMensagem("Voluntário não disponível. Insira novamente");
                                ler = new Scanner(System.in);
                                escolha = ler.nextLine();
                                tipo = sistema.getVoluntarios().getTipo(escolha);
                                vol = (Voluntario) tipo;
                            }
                            entrega.setTransporte(tipo);
                        }
                        if(escolha.charAt(0)=='t') {
                            ITipo tipo = sistema.getEmpresas().getTipo(escolha);
                            Empresa emp = (Empresa) tipo;
                            while(!emp.getDisponibilidade()){
                                view.printMensagem("Empresa não disponível. Insira novamente");
                                ler = new Scanner(System.in);
                                escolha = ler.nextLine();
                                tipo = sistema.getEmpresas().getTipo(escolha);
                                emp = (Empresa) tipo;
                            }
                            view.printMensagem("Caso esteja a chover ou trânsito irá ser lhe cobrada uma taxa de 1.5 Eur");
                            view.printMensagem("Aceita?\n(Y) Continuar                   (N) Escolher Voluntário");
                            ler = new Scanner(System.in);
                            if(ler.nextLine().equals("N")){
                                view.printMensagem("Escolha de novo o transporte!");
                                opcao = 2;
                                break;
                            }
                            if(encomenda.getCongelados()){
                                view.printMensagem("A sua encomenda contem congelados.Logo o serviço será mais rápido e " +
                                        "irá ser lhe aplicada uma taxa extra da Empresa");
                                view.printMensagem("Aceita?\n(Y) Continuar                   (N) Escolher Voluntário");
                                ler = new Scanner(System.in);
                                if(ler.nextLine().equals("N")){
                                    view.printMensagem("Escolha de novo o transporte!");
                                    opcao =2;
                                    break;
                                }
                            }
                            entrega.setTransporte(tipo);
                        }
                        entrega.setEncomenda(encomenda);
                        sistema.getFilaEncomendas().removeEncomenda(encomenda);
                        sistema.getFilaEntregues().addEncomenda(entrega);
                        view.printMensagem(entrega.toString());
                        view.printMensagem(sistema.toString());
                        view.printMensagem("Transporte escolhido: " + escolha);
                        view.printMensagem("Por favor aguarde o contacto do transporte...");

                        view.printMensagem("\nPressione ENTER para aceder ao User Menu\nPressione 0 para voltar ao Login Menu");
                        Scanner scanner = new Scanner(System.in);
                        String aux = scanner.nextLine();
                        if(aux.equals("0")) opcao = 0;
                    }
                    break;
                }
                case 3: {

                    int classificacao = 0 ;
                    List<IEntrega> lista = this.user.getHistorico();
                    int i=0;
                    if(lista.size()>=1) for(i=0; i< (lista.size()-1); i++);
                    IEntrega e;
                    if(lista.size()!=0){
                        view.printMensagem("Já recebeu a sua encomenda!! ");
                        e = lista.get(i);
                        ITipo transp = e.getTransporte();
                        view.printMensagem("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
                        view.printMensagem("RECIBO DA COMPRA: ");
                        view.printMensagem(e.toString());
                        view.printMensagem("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
                        view.printMensagem("Por favor classifique o transporte "+ transp.getNome() +
                                " da encomenda " + e.getEncomenda().getEncomendaID() + ":\n");
                        view.classificacao();
                        ler = new Scanner(System.in);
                        classificacao = ler.nextInt();
                        if(classificacao==0) break; //sai deste menu
                        if(transp instanceof Voluntario) ((Voluntario) transp).setVolunteer_rating(classificacao);
                        if(transp instanceof Empresa) ((Empresa) transp).setClassificacao(classificacao);
                    }
                    else view.printMensagem("Não há encomendas por classificar!!\n");
                    break;
                }
                case 4: { //Encomendas à espera de transporte
                    if(sistema.getFilaEncomendas().getEncomendas(user.getId())==null) view.printMensagem("Não há encomendas!");
                    else System.out.println(sistema.getFilaEncomendas().getEncomendas(user.getId()).toString());
                    break;
                }
                case 5:{
                    view.printMensagem("");
                    view.lista(user.getHistorico());
                    view.printMensagem("");
                }
                default: break;
            }
        }while (opcao!=0);
        view.printMensagem("Obrigada!Volte Sempre!");
        return res;
    }

    public IEncomenda transporte(char opcao){
        //Se o número de encomendas deste user for igual a 1
        /*Caso o user tenha feito mais do que 1 encomenda , apenas podemos escolher as transportadoras que conseguem
        transportar mais de uma encomenda*/
        IEncomenda res = new Encomenda();
        Set<IEncomenda> encomendasUser = sistema.getFilaEncomendas().getEncomendas(user.getId());
        HashSet<ITipo> set = new HashSet<>();
        if(encomendasUser!=null) {
            for (IEncomenda enco : encomendasUser) {
                res = enco;
                Loja loja = (Loja) sistema.getLojas().getTipo(enco.getLojaID());
                set = sistema.getGestao().verificarTransporte(sistema.getVoluntarios(), sistema.getEmpresas(), enco, loja);
                break;
            }
            view.transportes(opcao, set);
        }
        return res;
    }


    /**
     * função que chama o navegador para printar os produtos e as lojas , caso a opção seja 0 ou 1, respetivamente
     */
    public int catalogo(int opcao, List<IProduto> prods, List<String> quantidades){
        Scanner ler;
        String x = " ";
        int num = 0;
        if(opcao==0)
            nav.divide(sistema.getCatalogoProds(),null,"\nCATALOGO DE PRODUTOS",0);
        if (opcao==1)
            nav.divide(null,sistema.getLojas().getCatalogo(),"\nCATALOGO DE LOJAS", 1);
        nav.menu();
        do {
            ler = new Scanner(System.in);
            x = ler.nextLine();
            switch (x) {
                case "P": {
                    if(opcao==0)
                        nav.proxima(sistema.getCatalogoProds(), null,"\nCATALOGO DE PRODUTOS",0);
                    if(opcao==1)
                        nav.proxima(null, sistema.getLojas().getCatalogo(),"\nCATALOGO DE LOJAS",1);
                    nav.menu();
                    break;
                }
                case "A": {
                    if (opcao==0)
                        nav.anterior(sistema.getCatalogoProds(),null, "\nCATALOGO DE PRODUTOS",0);
                    if(opcao==1)
                        nav.anterior(null,sistema.getLojas().getCatalogo(), "\nCATALOGO DE LOJAS",1);
                    nav.menu();
                    break;
                }
                case "N": {
                    view.printMensagem("Insira o nº da Página:");
                    ler = new Scanner(System.in);
                    num = ler.nextInt();
                    if (opcao==0)
                        nav.escolha(sistema.getCatalogoProds(), null,"\nCATALOGO DE PRODUTOS", 0, num);
                    if(opcao==1)
                        nav.escolha(null, sistema.getLojas().getCatalogo(),"\nCATALOGO DE LOJAS", 1, num);
                    nav.menu();
                    break;
                }
                case "T": {
                    if (opcao==0)
                        nav.total(sistema.getCatalogoProds(),null,0);
                    if (opcao==1)
                        nav.total(null,sistema.getLojas().getCatalogo(),1);
                    nav.menu();
                    break;
                }
                case "E":{
                    if(opcao==0) escolheProdLoja(0,prods,quantidades);
                    if(opcao == 1){
                        String loja = escolheProdLoja(2,prods,quantidades);
                        //Mandar a encomenda para a gestao e depois para a fila de espera
                        this.encoFinal = sistema.getGestao().constroiEncomendaParaLoja(loja,prods,quantidades, user);
                        sistema.getFilaEspera().addEncomenda(encoFinal);
                    }
                    x = "M";
                    break;
                }
                case "M":{
                    return -1;
                }
                default: {
                    if (!(x.equals("M"))) view.printMensagem("Por favor insira uma opção válida!");
                    nav.menu();
                    break;
                }
            }
        }while(!(x.equals("M")));

        return 0;
    }

    /**
     * função auxiliar à inserção da encomenda.
     */
    public String escolheProdLoja(int opcao, List<IProduto> prods, List<String> quantidades) {
        String loja = " ";
        Scanner ler = new Scanner(System.in);
        if(opcao==0){
            view.printMensagem("Insira o código do produto"); //ver se o produto existe
            String prod = ler.nextLine();
            while(!sistema.getCatalogoProds().existsProdStr(prod)){
                view.printMensagem("Produto não existe!! Insira novamente.");
                prod = ler.nextLine();
            }
            IProduto produto = sistema.getCatalogoProds().getProd(prod);
            prods.add(produto);
            view.printMensagem("Cesto de Compras: " + prods.toString());
            view.printMensagem("Qual a quantidade pretendida");
            ler = new Scanner(System.in);
            String quantidade = ler.nextLine();
            quantidades.add(quantidade);
            view.printMensagem("Pretende escolher mais produtos? Selecione 1, caso contrário 0");
            ler = new Scanner(System.in);
            String x = ler.nextLine();
            if (x.equals("0")) {
                escolheProdLoja(1,prods,quantidades);
            }
            if(x.equals("1")){
                catalogo(0,prods,quantidades);
            }

        }

        if (opcao==1) {
            catalogo(1,prods,quantidades);

        }
        if(opcao==2) {
            view.printMensagem("Insira o código da loja");
            ler = new Scanner(System.in);
            loja = ler.nextLine();
            if (sistema.getLojas().existsID(loja)) {
            } else {
                view.printMensagem("Loja inválida, insira novamente");
                catalogo(1, prods,quantidades);
            }
        }
        return loja;
    }
}
