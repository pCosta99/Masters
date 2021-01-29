package Projeto.Controllers;

import Projeto.Entidades.Utilizador;
import Projeto.Entidades.Empresa;
import Projeto.Entidades.Loja;
import Projeto.Entidades.Voluntario;
import Projeto.Exceptions.EntidadeNaoExisteException;
import Projeto.Exceptions.IdRepetidoException;
import Projeto.Interfaces.*;
import Projeto.Interfaces.IUtilizador;
import Projeto.Interfaces.IEncomenda;
import Projeto.Model.TrazAqui;
import Projeto.Util.Estado;
import Projeto.Util.GPS;
import Projeto.Views.ViewUtilizador;
import Projeto.Views.ViewEmpresa;
import Projeto.Views.ViewLoja;
import Projeto.Views.ViewVoluntario;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Classe que implementa o Controller.
 * O Controller tem como objetivo interligar as necessidades da View com as informações do model.
 * É chamado pela View, "pede" a informação do model, que a View precisa, e envia essa informação para a View.
 */
public class Controller {
    private TrazAqui model;
    private final boolean gravar;

    /**
     * Contrutor por omissão do Controller.
     * É responsável por "popular" o programa, ou seja, lê um ficheiro de logs
     * ou objeto para criar instâncias para o modelo.
     */
    public Controller(int i, boolean gravar) throws IOException, ClassNotFoundException {
        this.gravar = gravar;
        this.model = new TrazAqui();
        if(i == 1)
            this.model.parse();
        else if (i == 2)
            this.model = this.model.lerObj("Projeto/docs/objeto.txt");
    }

    /**
     * Método que dependendo da String que receber calcula o tamanho da estrutura de dados
     * desse argumento.
     */
    public int quantos(String s) {
        int ret = 0;
        switch (s){
            case "Clientes" : 
                ret = this.model.quantosClientes();
                break;
            case "Empresas" :
                ret = this.model.quantasEmps();
                break;
            case "Lojas" :
                ret = this.model.quantasLojas();
                break;
            case "Voluntarios" :
                ret = this.model.quantosVols();
                break;
            default :
                break;
        }
        return ret;
    }

    /**
     * Método que cria um novo Cliente e adiciona-o na estrutura de dados para clientes.
     * @param id - ID do Cliente.
     * @param nome - Nome do Cliente.
     * @param lat - Latitude do Cliente.
     * @param lon - Longitude do Cliente.
     * @param nif - NIF do Cliente.
     */
    public IUtilizador adicionaCliente(String id, String pw, String nome, float lat, float lon, String nif) throws IOException {
        Collection<IEncomenda> l = new ArrayList<>();
        GPS gps = new GPS(lat, lon);
        IUtilizador c = new Utilizador(id, pw, nome, gps, l, nif);
        try {
            this.model.addCliente(c);
        } catch (IdRepetidoException e) {
            this.adicionaCliente(id + "0", pw, nome, lat, lon, nif);
        }
        if(this.gravar) this.model.gravarObj();
        return c;
    }

    /**
     * Método que cria uma nova Empresa e adiciona-a na estrutura de dados para Empresas.
     * @param id - id da Empresa
     * @param nome - nome da Empresa
     * @param lat - latitude da localização da Empresa
     * @param lon - longitude da localização da Empresa
     * @param raio - raio de ação da Empresa
     * @param medic - se as Empresas podem ou não transformar encomendas medicinais
     * @param cap - capacidade máxima de Encomendas que a Empresa pode transportar
     * @param taxa - taxa que a Empresa cobre pelo transporte de encomenda
     * @param nif - nif da Empresa
     */
    public IEmpresa adicionaEmpresa(String id, String pw, String nome, float lat, float lon, float raio,
                                boolean medic, int cap, float taxa, String nif) throws IOException {
        Collection<IEncomenda> encs = new ArrayList<>();
        Collection<IEncomenda> encsPorEntregar = new ArrayList<>();
        Collection<Float> vls = new ArrayList<>();
        Collection<Integer> cls = new ArrayList<>();
        Estado estado = new Estado("Livre");
        GPS gps = new GPS(lat, lon);
        IEmpresa e = new Empresa(id, pw, nome, gps, encs, vls, raio, medic, cls, estado, cap, taxa, nif, 0, encsPorEntregar);
        try {
            this.model.addEmpresa(e);
        } catch (IdRepetidoException ex) {
            this.adicionaEmpresa(id + "0", pw, nome, lat, lon, raio, medic, cap, taxa, nif);
        }
        if(this.gravar) this.model.gravarObj();
        return e;
    }

    /**
     * Método que cria uma nova Loja, com os dados recebidos da View, e adiciona-a nas estrutura de dados
     * @param id - id da Loja
     * @param nome - nome da Loja
     * @param lat - latitude da localização da Loja
     * @param lon - longitude da localização da Loja
     * @param dadosFila - se a Loja fornece dados sobre a fila ou não
     */
    public ILoja adicionaLoja(String id, String pw, String nome, float lat, float lon, boolean dadosFila) throws IOException {
        GPS gps = new GPS(lat, lon);
        Collection<IProduto> prods = new ArrayList<>();
        Collection<IEncomenda> encs = new ArrayList<>();
        ILoja l = new Loja(id, pw, nome, gps, encs, dadosFila, 0, 0, prods);
        try {
            this.model.addLoja(l);
        } catch (IdRepetidoException ex) {
            this.adicionaLoja(id + "0", pw, nome, lat, lon, dadosFila);
        }
        if (this.gravar) this.model.gravarObj();
        return l;
    }

    /**
     * Método que cria um novo Voluntário, com os dados recebidos da View, e adiciona-o na estrutura de dados.
     * @param id - id do Voluntário
     * @param nome - nome do Voluntário
     * @param lat - latitude da localização do Voluntário
     * @param lon - longitude da localização do Voluntário
     * @param raio - raio de ação do Voluntário
     * @param medic - se o Voluntário pode transportar encomendas medicinais
     * @param cap - capacidade máxima de Encomendas que um Voluntário consegue transportar
     */
    public IVoluntario adicionaVoluntario(String id, String pw, String nome, float lat, float lon, float raio, boolean medic, int cap) throws IOException{
        GPS gps = new GPS(lat, lon);
        Collection<IEncomenda> encs =  new ArrayList<>();
        Collection<IEncomenda> encsPorEntregar =  new ArrayList<>();
        Collection<Float> vels = new ArrayList<>();
        Collection<Integer> cls = new ArrayList<>();
        Estado e = new Estado("Livre");
        IVoluntario v = new Voluntario(id, pw, nome, gps, encs, vels, raio, medic, cls, e, cap, encsPorEntregar);
        try {
            this.model.addVols(v);
        } catch (IdRepetidoException exc) {
            this.adicionaVoluntario(id + "0", pw, nome, lat, lon, raio, medic, cap);
        }
        if(this.gravar) this.model.gravarObj();
        return v;
    }

    /**
     * Método que troca a view para o User.
     * @param menu - View para qual o user vai
     * @param id - id do user
     */
    public void changeMenu(String menu, String id, String pw) throws EntidadeNaoExisteException {
        switch (menu) {
            case "Utilizador":
                if (this.model.existeCliente(id) && this.model.passCertaCliente(id, pw)) {
                    ControllerUtilizador cc = new ControllerUtilizador(this.model, id, this.gravar);
                    ViewUtilizador view = new ViewUtilizador(cc);
                    view.menuUtilizador();
                }
                else throw new EntidadeNaoExisteException("Utilizador inválido ou password inválida!");
                break;

            case "Loja":
                if(this.model.existeLoja(id) && this.model.passCertaLoja(id, pw)) {
                    ControllerLoja cl = new ControllerLoja(this.model, id, this.gravar);
                    ViewLoja vl = new ViewLoja(cl);
                    vl.menuLojas();
                } else throw new EntidadeNaoExisteException("Loja inválida ou password inválida!");
                break;

            case "Empresa":
                if(this.model.existeEmpresa(id) && this.model.passCertaEmpresa(id, pw)) {
                    ControllerEmpresa ce = new ControllerEmpresa(this.model, id, this.gravar);
                    ViewEmpresa ve = new ViewEmpresa(ce);
                    ve.menuEmpresa();
                } else throw new EntidadeNaoExisteException("Empresa inválida ou password inválida!");
                break;

            case "Voluntario":
                if(this.model.existeVoluntario(id) && this.model.passCertaVoluntario(id, pw)) {
                    ControllerVoluntario cv = new ControllerVoluntario(this.model, id, this.gravar);
                    ViewVoluntario vv = new ViewVoluntario(cv);
                    vv.menuVoluntario();
                } else throw new EntidadeNaoExisteException("Voluntário inválida ou password inválida!");
                break;

            default:
                throw new EntidadeNaoExisteException("Menu inválido!");
        }
    }
}