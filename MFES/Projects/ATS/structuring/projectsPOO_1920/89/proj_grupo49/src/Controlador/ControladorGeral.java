package Controlador;

import Model.*;
import View.ViewGeral;

import java.io.IOException;
import java.io.Serializable;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class ControladorGeral  implements Serializable {
    private Modelo modelo;

    public ControladorGeral(Modelo m){
        this.modelo  = m;
    }

    public ControladorGeral(){
        Lojas lojas = new Lojas();
        Transportadoras transportadoras = new Transportadoras();
        Voluntarios voluntarios = new Voluntarios();
        Utilizadores utilizadors = new Utilizadores();
        Map<String, Encomenda> encomendas = new TreeMap<>();
        Set<Produto> produtos;
        this.modelo = new Modelo(lojas,transportadoras,voluntarios,utilizadors,encomendas);
    }

    public void setModel(Modelo m){
        this.modelo = m;
    }



    public void run() throws IOException, ClassNotFoundException {
        InterfaceInput i = new Input();
        ViewGeral v = new ViewGeral();


        //System.out.println("Deseja ler de um txt ou de um .dat");
//        modelo.parse("logs2.txt");
//        modelo.gravarObj();
//        this.modelo = Modelo.lerObj();


        try {
            this.modelo = Modelo.lerObj();
        }
        catch (IOException | ClassNotFoundException e ){
            try {
                this.modelo.parse("logs2.txt");
            }catch (IOException es){
                v.readError();
            }
        }

        ControladorLogInTransp t = new ControladorLogInTransp(modelo);
        ControladorLogInVoluntarios vol = new ControladorLogInVoluntarios(modelo);
        ControladorLogInLoja l = new ControladorLogInLoja(modelo);
        ControladorLogInUser u = new ControladorLogInUser(modelo);

        //boolean load = false;
        int op =-1;
        //int op1 =-1;
        v.printHeader();

        while (op!=0){
            v.menuInicial();
            System.out.println("\nEscolha a instrução: ");
            op = i.lerInt();
            switch (op) {
                case 0:
                    v.printExit();
                    break;
                case 1:
                    v.pressioneEnter();
                    i.lerString();
                    v.flush();
                    l.run();
                    break;
                case 2:
                    v.pressioneEnter();
                    i.lerString();
                    v.flush();
                    u.run();
                    break;
                case 3:
                    v.pressioneEnter();
                    i.lerString();
                    v.flush();
                    vol.run();
                    break;
                case 4:
                    v.pressioneEnter();
                    i.lerString();
                    v.flush();
                    t.run();
                    break;
                case 5:
                    v.maisUsados(modelo.maisUsados());
                    v.pressioneEnter();
                    i.lerString();
                    v.flush();
                    break;
                case 6:
                    v.maisUsadosT(modelo.maisUsadosT());
                    v.pressioneEnter();
                    i.lerString();
                    v.flush();
                    break;
                case 7:
                    try {
                        modelo.gravarObj();
                        v.leitura();
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                    }catch (IOException e){
                        v.printErrorMessage(e.getMessage());
                    }
                    break;
                case 8:
                    v.printUsers(modelo.getUtilizadores());
                    break;
                case 9:
                    v.printLojas(modelo.getLojas());
                    break;
                case 10:
                    v.printTransp(modelo.getTransportadoras());
                    break;
                case 11:
                    v.printVols(modelo.getVoluntarios());
                    break;
                default:
                    System.out.println("Opção invalida");
                    break;
            }
        }
    }
}
