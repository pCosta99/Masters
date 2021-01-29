package Controller;

import Model.Encomendas.IEncomenda;
import Model.ISistema;
import Model.Tipos.ITipo;
import Model.Tipos.Loja;
import View.AppView;
import View.IAppView;

import java.util.*;

/**
 * Controller para a parte referente apenas à loja
 */
public class LojaController implements ILojaController {
    private ISistema sistema; //sistema para dar set na main
    private IAppView view; // view para dar set na main
    private int opcao;//opcao que escolhe

    public LojaController(){
        this.opcao = 0;
        this.view = new AppView();
    }

    public void setSistema(ISistema sistema){ this.sistema = sistema; }

    public void setView(IAppView view){ this.view= view; }

    /**
     * metodo que vai ver qual é opcao que escolhe do menu da view modeLoja. Faz scan da opção que a loja inseriu depois de ver o menu.
     */
    public void lojaMode() {
        view.infos();
        Scanner ler = new Scanner(System.in);
        String tempo = ler.nextLine();
        String id = sistema.getQuem().getPassword().substring(0,3);
        ITipo loja = sistema.getLojas().getTipo(id);
        if(loja instanceof Loja) {
            ((Loja) loja).setTempoPessoa(Double.parseDouble(tempo));
            if(sistema.getFilaEspera().getEncomendas(id)!=null)((Loja) loja).setPessoasFila(sistema.getFilaEspera().getEncomendas(id).size());
            else {
                view.printMensagem("Não há pessoas na Fila de Espera\n");
                ((Loja) loja).setPessoasFila(0);
            }
        }
        do {
            ler = new Scanner(System.in);
            view.modeLoja();
            opcao = ler.nextInt();
            switch (opcao) {
                case 1: { // Encomendas em fila de espera
                    Set<IEncomenda> set = sistema.getFilaEspera().getEncomendas(id);
                    if(set==null) view.printMensagem("Não há encomendas na fila de espera!");
                    else{
                        view.encomendasEspera(set);
                    }
                    break;
                }
                case 2: { //Validar a encomenda
                    view.printMensagem("Validar a Encomenda: ");
                    ler = new Scanner(System.in);
                    String encId = ler.nextLine();
                    while(!sistema.getFilaEspera().containsEncTipo(encId,id)){
                        view.printMensagem("Encomenda inválida. Insira novamente!");
                        ler = new Scanner(System.in);
                        encId = ler.nextLine();
                    }
                    IEncomenda encomenda = sistema.getFilaEspera().getEncomendaTipo(encId,id);
                    view.tipodeEncomenda(3);
                    ler = new Scanner(System.in);
                    String peso = ler.nextLine();
                    encomenda.setPesoTotal(Double.parseDouble(peso));
                    view.tipodeEncomenda(1);
                    ler = new Scanner(System.in);
                    int opcao = ler.nextInt();
                    if(opcao==1) encomenda.setMedicamentos(true);
                    else encomenda.setMedicamentos(false);

                    view.tipodeEncomenda(2);
                    ler = new Scanner(System.in);
                    opcao = ler.nextInt();
                    if(opcao==1) encomenda.setCongelados(true);
                    else encomenda.setCongelados(false);


                    sistema.addAceite(encomenda.getEncomendaID());

                    view.printMensagem("Encomenda Feita!!");
                    view.printMensagem("Esperar pela recolha da encomenda...");
                    view.printMensagem("\nPressione ENTER para aceder ao Loja Menu\nPressione 0 para voltar ao Login Menu");
                    Scanner scanner = new Scanner(System.in);
                    String aux = scanner.nextLine();
                    if(aux.equals("0")) opcao = 0;
                    break;
                }

                case 3: { //Ver o histórico de vendas realizadas
                    if(!sistema.getFilaEncomendas().existsKey(loja.getId())){
                        view.printMensagem("Não tem vendas.");
                        break;
                    }
                    Set<IEncomenda> set = sistema.getFilaEncomendas().getEncomendas(loja.getId());
                    if(set.size()==0){
                        view.printMensagem("Não tem vendas.");
                        break;
                    }
                    else view.vendasLoja(set);
                    break;
                }
                default:
                    break;
            }
        }while (opcao != 0) ;
        view.printMensagem("Obrigada!Volte Sempre!");
    }



}
