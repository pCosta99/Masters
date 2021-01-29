package Model;

import Model.Catalogos.*;

import Model.Encomendas.IEncomenda;
import Model.Encomendas.IEntrega;
import Model.Logins.ILogin;
import Model.Logins.Login;
import Model.Tipos.*;

import java.io.Serializable;
import java.util.*;

public class Sistema implements ISistema, Serializable {

    private ICatalogoTipo lojas;
    private ICatalogoTipo users;
    private ICatalogoTipo empresas;
    private ICatalogoTipo voluntarios;
    private ICatalogoProds catalogoProds; //Catalogo com todos os produtos que existem na aplicação
    private ICatalogoLogins logins;

    private HashSet<String> aceites; //encomendas já prontas para transportar

    private ILogin quem; //quem é que está com o login aberto

    private IFila filaEspera;
    private IFila filaEncomendas;
    private IFilaEntregues filaEntregues;
    private IGestaoEncomendas gestao;


    public Sistema() {
        this.lojas = new CatalogoTipo();
        this.users = new CatalogoTipo();
        this.empresas = new CatalogoTipo();
        this.voluntarios = new CatalogoTipo();
        this.catalogoProds = new CatalogoProds();

        this.logins = new CatalogoLogins();
        this.aceites = new HashSet<>();

        this.quem = new Login();

        this.filaEspera = new FilaEspera();
        this.filaEncomendas = new FilaEncomendas();
        this.filaEntregues = new FilaEntregues();
        this.gestao = new GestaoEncomendas();
    }


    public Sistema(Sistema sistema) {

        this.lojas = sistema.getLojas();
        this.users = sistema.getUsers();
        this.empresas = sistema.getEmpresas();
        this.voluntarios = sistema.getVoluntarios();
        this.catalogoProds = sistema.getCatalogoProds();

        this.logins = sistema.getLogins();
        this.aceites = sistema.getAceites();

        this.quem = sistema.getQuem();
        this.filaEspera = getFilaEspera();
        this.filaEncomendas = getFilaEncomendas();
        this.filaEntregues = getFilaEntregues();
        this.gestao = getGestao();
    }

    public IGestaoEncomendas getGestao() {
        return gestao;
    }

    public void setGestao(IGestaoEncomendas gestao) {
        this.gestao = gestao;
    }

    public IFila getFilaEspera() {
        return filaEspera;
    }

    public void setFilaEspera(IFila filaEspera) {
        this.filaEspera = filaEspera;
    }

    public IFila getFilaEncomendas() {
        return filaEncomendas;
    }

    public void setFilaEncomendas(IFila filaEncomendas) {
        this.filaEncomendas = filaEncomendas;
    }

    public IFilaEntregues getFilaEntregues() {
        return filaEntregues;
    }

    public void setFilaEntregues(IFilaEntregues filaEntregues) {
        this.filaEntregues = filaEntregues;
    }


    public ILogin getQuem() {
        return quem;
    }

    public void setQuem(ILogin quem) {
        this.quem = quem;
    }

    public HashSet<String> getAceites() {
        return this.aceites;
    }

    public void setAceites(HashSet<String> encs) {
        this.aceites = encs;
    }

    public ICatalogoTipo getLojas() {
        return lojas;
    }

    public void setLojas(ICatalogoTipo lojas) {
        this.lojas = lojas;
    }

    public ICatalogoTipo getUsers() {
        return users;
    }

    public void setUsers(ICatalogoTipo users) {
        this.users = users;
    }

    public ICatalogoTipo getEmpresas() {
        return empresas;
    }

    public void setEmpresas(ICatalogoTipo empresas) {
        this.empresas = empresas;
    }

    public ICatalogoTipo getVoluntarios() {
        return voluntarios;
    }

    public void setVoluntarios(ICatalogoTipo voluntarios) {
        this.voluntarios = voluntarios;
    }


    public ICatalogoLogins getLogins() {
        return this.logins;
    }

    public ICatalogoProds getCatalogoProds() {
        return this.catalogoProds;
    }

    public void setCatalogo(ICatalogoProds cat){
        this.catalogoProds = cat;
    }


    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Sistema sistema = (Sistema) o;
        return this.lojas.equals(sistema.getLojas()) &&
                this.users.equals(sistema.getUsers()) &&
                this.empresas.equals(sistema.getEmpresas()) &&
                this.voluntarios.equals(sistema.getVoluntarios()) &&
                this.logins.equals(sistema.getLogins());
    }

    public String toString() {
        StringBuffer sb = new StringBuffer("Sistema: ");
        //sb.append("\nLista de Lojas: ").append(this.lojas).append(", ");
        //sb.append("\nLista de Clientes: ").append(this.users).append(", ");
        //sb.append("\nLista de Empresas: ").append(this.empresas).append(", ");
        //sb.append("\nLista de Voluntários: ").append(this.voluntarios);
        //sb.append("\nLista de Logins: ").append(this.logins);
        sb.append("\n\nFila de Encomendas à espera de Transporte: ").append(this.filaEncomendas);
        sb.append("\n\nFila de Espera para as Lojas: ").append(this.filaEspera);
        sb.append("\n\nFila de Entregas após escolha transporte: ").append(this.filaEntregues);
        sb.append("\nEncomendas aceites: ").append(this.aceites);
        sb.append("\n\nÚltimo Login: ").append(this.quem);
        return sb.toString();
    }

    public Sistema clone() {
        return new Sistema(this);
    }


    //DEFINIR O STOCK DE CADA LOJA
    public void StockLoja(){
        ICatalogoProds aux = this.catalogoProds;
        for(ITipo loja : this.lojas.getCatalogo()){
            if(loja instanceof Loja) {
                ((Loja) loja).setStock(aux);
            }
        }
    }

    /**
     * Adicionar o ID de uma encomenda à lista de encomendas aceites.
     * Método usado principalmente quando lemos do ficheiro Logs.txt
     */
    public void addAceite(String id){
        this.aceites.add(id);
        IEncomenda encomenda = this.filaEspera.existsEncomenda(id);
        if(encomenda!=null){
            this.filaEncomendas.addEncomenda(encomenda);
            this.filaEspera.removeEncomenda(encomenda);
        }
    }

    /**
     * Só vai até 10 com o array na view
     * @return
     */
    public ITipo[] top10Users(){
       Set<ITipo> set = this.users.getCatalogo();
       ITipo[] array = set.toArray(new ITipo[set.size()]);
       Arrays.sort(array, new Comparator<ITipo>(){
           public int compare(ITipo c1, ITipo c2){
               int res1 = 0; int res2 = 0;
               if(c1 instanceof User){
                   res1 = ((User)c1).getHistorico().size();
               }
               if(c2 instanceof User){
                   res2 = ((User)c2).getHistorico().size();
               }
               if(res1>res2) return -1;
               else if(res1<res2) return 1;
               else return 0;
           }
       });
       return array;
    }

    public Integer[] getNComprasUser(ITipo[] users){
        Integer[] res = new Integer[users.length];
        int n;
        for(int i=0; i< users.length;i++){
            n = ((User)users[i]).getHistorico().size();
            res[i] = n;
        }
        return res;
    }


}
