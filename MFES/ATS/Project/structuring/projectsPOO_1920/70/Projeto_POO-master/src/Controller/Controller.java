package Controller;

import Model.*;
import View.Output;

import java.io.IOException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.*;


public class Controller {
    TrazAqui ta;
    Output output = new Output();
    Parse parser = new Parse();
    String password, newPassword, email;

    public void controller(){
        output.loading();
        Load_Save saveState = new Load_Save();
        try {
            if((ta = (TrazAqui) saveState.load()) == null){
                ta = new TrazAqui();
            }
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
        output.loadingCompleted();

        boolean keep = true;
        while (keep){
            output.startingMenu();
            int option = Input.lerInt();
            switch (option){
                case 1:
                case 2:
                case 3:
                case 4:
                case 5:
                    loginMenu(option);//assim funciona deadlock, deixa estar assim
                    break;
                case 6:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        try {
            saveState.save(ta);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void loginMenu(int menuType){
        output.loginMenu();
        int option = Input.lerInt();
        switch(option){
            case 1:
                /* Métodos para o log in */
                output.email();
                email = Input.lerString();
                output.password();
                password = Input.lerString();
                if(ta.getLoginInfo(menuType).validatePassword(email, password)){
                    email = email.split("@")[0];
                    output.loginSuccessful();
                    switch (menuType){
                        case 1:
                            userMenu();
                            break;
                        case 2:
                            storeMenu();
                            break;
                        case 3:
                            volunteerMenu();
                            break;
                        case 4:
                            transMenu();
                            break;
                        case 5:
                            admimMenu();
                            break;
                        default:
                            throw new IllegalStateException("Unexpected value: " + menuType);
                    }
                }else{
                    output.loginFailed();
                }
                break;
            case 2:
                /* Métodos para o registo */
                output.email();
                email = Input.lerString();
                int tries = 0;
                do {
                    if(tries > 0){
                        output.passwordRepeat();
                    }
                    output.password();
                    password = Input.lerString();
                    output.repeatPassword();
                    newPassword = Input.lerString();
                    tries++;
                }while(!password.equals(newPassword));
                Login log = ta.getLoginInfo(menuType);
                if(log.addRegister(email, password)){
                    switch (menuType){
                        case 1:
                            userRegister();
                            output.registerSuccessful();
                            userMenu();
                            break;
                        case 2:
                            storeRegister();
                            output.registerSuccessful();
                            storeMenu();
                            break;
                        case 3:
                            transRegister();
                            output.registerSuccessful();
                            transMenu();
                            break;
                        case 4:
                            volunteerRegister();
                            output.registerSuccessful();
                            volunteerMenu();
                            break;
                        case 5:
                            output.registerSuccessful();
                            admimMenu();
                            break;
                        default:
                            throw new IllegalStateException("Unexpected value: " + menuType);
                    }
                }else{
                    output.registerFailed();
                }
                break;
            case 3:
                output.exit();
                break;
            default:
                output.invalidCommand();
                break;
        }
    }

    //user
    private void userMenu(){
        User user = ta.getUserCatalog().get(email);
        output.welcomeUser(user.getName());
        boolean keep = true;
        while (keep){
            output.userMenu();
            int option = Input.lerInt();
            switch (option){
                case 1:
                    //nova encomenda
                    Loja loja = displayStores();
                    if(loja != null){
                        //por os produtos de uma loja em pagina e adicionar a encomenda
                        ta.getGestor().addEncomenda(displayProducts(loja.getProductCatalog()));
                    }
                    break;
                case 2:
                    //pedir entrega da encomenda
                    Map<String,Encomenda> encomendas = ta.getGestor().getGestor();
                    Map<Integer,Encomenda> userEnc = new HashMap<>();
                    int i = 1;
                    for(String s: encomendas.keySet()){
                        if(encomendas.get(s).getCodigoUser().equals(email) && encomendas.get(s).isAceitePorLoja() && !(encomendas.get(s).isEntregue())){
                            userEnc.put(i,encomendas.get(s));
                            i++;
                        }
                    }
                    List<Encomenda> newL = new ArrayList<>(i);
                    for(int j = 0; j < i; j++){
                        newL.add(j,userEnc.get(j+1));
                    }
                    Encomenda pedida = displayEncomendas(newL);
                    List<Voluntario> v = ta.voluntariosDisponiveis(pedida);
                    List<EmpresaTransportadora> e = ta.empresasDisponiveis(pedida);
                    output.voluntariosDisponiveis(v.size());
                    output.empresasDisponiveis(e.size());
                    EmpresaTransportadora et = new EmpresaTransportadora();
                    Voluntario vol = new Voluntario();
                    if(v.size() == 0 && e.size() == 0){
                        output.noVolnorEmpFound();
                    } else if(v.size() == 0){
                        et = displayEmpresas(e,pedida);
                        ta.getGestor().getGestor().get(pedida.getCodigo()).setCodigoTrans(et.getCodTrans());
                    } else if(e.size() == 0){
                        vol = displayVoluntarios(v);
                        ta.getGestor().getGestor().get(pedida.getCodigo()).setCodigoTrans(vol.getCodVoluntario());
                    } else {
                        output.perguntaVolOuET();
                        int choice = Input.lerInt();
                        switch(choice){
                            case 1:
                                vol = displayVoluntarios(v);
                                ta.getGestor().getGestor().get(pedida.getCodigo()).setCodigoTrans(vol.getCodVoluntario());
                                break;
                            case 2:
                                et = displayEmpresas(e,pedida);
                                ta.getGestor().getGestor().get(pedida.getCodigo()).setCodigoTrans(et.getCodTrans());
                                break;
                            default:
                                output.invalidCommand();
                                break;
                        }
                    }
                    break;
                case 3:
                    //dar encomenda por terminada
                    List<Encomenda> encomendasDoUser = ta.encByUser(email);
                    Encomenda choice = displayEncomendas(encomendasDoUser);
                    output.perguntaRate();
                    int r = Input.lerInt();
                    if(choice.getCodigoTrans().charAt(0) == 'v'){
                        ta.avaliarVoluntario(r,choice.getCodigoTrans());
                    } else if(choice.getCodigoTrans().charAt(0) == 't'){
                        ta.avaliarTransportadora(r,choice.getCodigoTrans());
                    }
                    ta.getUserCatalog().get(email).setEncomendas(ta.getUserCatalog().get(email).addEnc(choice.getCodigo())); //Adiciona a encomenda terminada ao historico do user
                    break;
                case 4:
                    //historico
                    output.printList(user.getEncomendas());
                    break;
                case 5:
                    //definicoes
                    if(userEdit(user))
                        break;
                case 6:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
    }
    private boolean userEdit(User user){
        Login loginInfo = ta.getLoginInfo(0);
        boolean keep = true;
        while (keep){
            output.userEdit();
            int option = Input.lerInt();
            switch (option){
                case 1:
                    //edit name
                    output.userName();
                    user.setName(Input.lerString());
                    break;
                case 2:
                    //alterar a localização
                    output.registerLocation();
                    output.locationLongitude();
                    double x = Input.lerDouble();
                    output.locationLatitude();
                    double y = Input.lerDouble();
                    Location l = new Location(x,y);
                    user.setLocation(l);
                    break;
                case 3:
                    // alterar password
                    output.oldPassword();
                    password = Input.lerString();
                    output.newPassword();
                    newPassword = Input.lerString();
                    if(loginInfo.changePassword(email, password, newPassword)){
                        output.changePasswordSuccessful();
                    }else {
                        output.changePasswordFailed();
                    }
                    break;
                case 5:
                    //delete account
                    output.confirmarRemocaoConta();
                    if(Input.lerBoolean()){
                        loginInfo.deleteRegister(email);
                    }
                    ta.getUserCatalog().remove(email);
                    return false;
                case 6:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        return true;
    }
    private void userRegister(){
        User user = new User();
        user.setCodUser(email);
        //inserir nome
        output.userName();
        user.setName(Input.lerString());
        //inserir localizaçao
        output.registerLocation();
        Location location = new Location(Input.lerDouble(),Input.lerDouble());
        user.setLocation(location);
        ta.addUser(user);
    }
    private Loja displayStores(){
        for (Map.Entry<String, Loja> entry : ta.getLojas().entrySet()) {
            output.printString(entry.getValue().getName());
        }
        output.receberNomeLoja();
        String escolha = Input.lerString();
        for (Map.Entry<String, Loja> entry : ta.getLojas().entrySet()) {
            if(entry.getValue().getName().equals(escolha)){
                return entry.getValue();
            }
        }
        output.invalidLoja();
        return null;
    }
    private Encomenda displayProducts(List<Produto> productCatalog){
        //Criar uma encomenda
        Encomenda encomenda = new Encomenda();
        int page_size = 5;
        boolean keep = true;
        while(keep){
            int i = 0;
            for(int j = i; j<page_size && j<productCatalog.size(); j++){
                output.printString( j+1 + productCatalog.get(j).toString());
            }
            for (Produto entry : productCatalog) {
                output.printString(entry.toString());
            }
            output.productDisplay();
            int option = Input.lerInt();
            switch (option){
                case 1:
                    //escolher produto
                    output.getProdName();
                    int produto = Input.lerInt();
                    if(produto<0 || produto>productCatalog.size()){
                        output.invalidProduct();
                        break;
                    }
                    output.getQuantity();
                    int quantidade = Input.lerInt();
                    LinhaEncomenda pedido = new LinhaEncomenda(productCatalog.get(produto),quantidade);
                    output.productSuccessful();
                    encomenda.adicionaLinha(pedido.clone());
                case 2:
                    // next page
                    if(i + page_size < productCatalog.size())
                        i += page_size;
                    else
                        output.limiteException();
                    break;
                case 3:
                    // previous page
                    if(i > page_size){
                        i += page_size;
                    }
                    else
                        output.limiteException();
                    break;
                case 4:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        if(encomenda.getLista().size()>0) {
            output.isMedical();
            encomenda.setMedical(Input.lerInt() == 1);
        }
        return encomenda;
    }
    private Encomenda displayEncomendas(List<Encomenda> e){
        int page_size = 5;
        boolean keep = true;
        Encomenda encomenda = new Encomenda();
        while(keep){
            int i = 0;
            for(int j = i; j<page_size && j<e.size(); j++){
                output.printString( j+1 + e.get(j).toString());
            }
            int option = Input.lerInt();
            output.encomendaDisplay();
            switch (option){
                case 1:
                    output.getEncName();
                    int enc = Input.lerInt();
                    if(enc<0 || enc>e.size()){
                        output.invalidEncomenda();
                        break;
                    }
                    encomenda = new Encomenda(e.get(enc));
                    keep = false;
                    break;
                case 2:
                    // next page
                    if(i + page_size < e.size())
                        i += page_size;
                    else
                        output.limiteException();
                    break;
                case 3:
                    // previous page
                    if(i > page_size){
                        i += page_size;
                    }
                    else
                        output.limiteException();
                    break;
                case 4:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        return encomenda;
    }
    private Voluntario displayVoluntarios(List<Voluntario> v){
        int page_size = 5;
        boolean keep = true;
        Voluntario vol = new Voluntario();
        while(keep){
            int i = 0;
            for(int j = i; j<page_size && j<v.size(); j++){
                output.printString( j+1 + v.get(j).toStringChoosable());
            }
            int option = Input.lerInt();
            output.volDisplay();
            switch (option){
                case 1:
                    output.getVolName();
                    int enc = Input.lerInt();
                    if(enc<0 || enc>v.size()){
                        output.invalidVoluntario();
                        break;
                    }
                    vol = new Voluntario(v.get(enc));
                    keep = false;
                    break;
                case 2:
                    // next page
                    if(i + page_size < v.size())
                        i += page_size;
                    else
                        output.limiteException();
                    break;
                case 3:
                    // previous page
                    if(i > page_size){
                        i += page_size;
                    }
                    else
                        output.limiteException();
                    break;
                case 4:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        return vol;
    }
    private EmpresaTransportadora displayEmpresas(List<EmpresaTransportadora> et, Encomenda pedida){
        int page_size = 5;
        boolean keep = true;
        EmpresaTransportadora e = new EmpresaTransportadora();
        while(keep){
            int i = 0;
            for(int j = i; j<page_size && j<et.size(); j++){
                double preco = pedida.calculaValorTotal() + pedida.getPeso()*et.get(j).getTaxapkg() + ta.getLojas().get(pedida.getCodigoLoja()).getLocation().distanceTo(et.get(j).getLocalizacao())*et.get(j).getTaxa();
                output.printString( j+1 + et.get(j).toStringChoosable(preco));
            }
            int option = Input.lerInt();
            output.volDisplay();
            switch (option){
                case 1:
                    output.getVolName();
                    int enc = Input.lerInt();
                    if(enc<0 || enc>et.size()){
                        output.invalidVoluntario();
                        break;
                    }
                    e = new EmpresaTransportadora(et.get(enc));
                    keep = false;
                    break;
                case 2:
                    // next page
                    if(i + page_size < et.size())
                        i += page_size;
                    else
                        output.limiteException();
                    break;
                case 3:
                    // previous page
                    if(i > page_size){
                        i += page_size;
                    }
                    else
                        output.limiteException();
                    break;
                case 4:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        return e;
    }

    //loja
    private void storeMenu(){

        Loja loja = ta.getLojas().get(email);
        if(loja == null){
            System.out.println("Something went wrong");
        }
        boolean keep = true;
        while (keep) {
            output.storeMenu();
            int option = Input.lerInt();
            switch (option) {
                case 1:
                    //catalogo de produtos da loja
                    productStoreMenu(loja.getProductCatalog());
                    break;
                case 2:
                    //confirmar que encomendas estao prontas
                    //dar display das encomendas pedidas e poder selecionar
                    Map<String,Encomenda> newM =ta.getGestor().getGestor();
                    Map<Integer,Encomenda> ret = new HashMap<>();
                    int i = 1;
                    for(String s: newM.keySet()){
                        if(!newM.get(s).isAceitePorLoja()){
                            ret.put(i,newM.get(s));
                            i++;
                        }
                    }
                    output.faltamXEnc(i);
                    List<Encomenda> listEnc = new ArrayList<>(i);
                    for(int j = 0; j < i; j++){
                        listEnc.add(j,ret.get(j+1));
                    }
                    ta.lojaAceitaEncomenda(displayEncomendas(listEnc));
                    break;
                case 3:
                    //historico de vendas
                    output.printHistEnc(ta.getLojas().get(email).getEncomendas().toString());
                    output.getOutKey();
                    String ignored = Input.lerString();
                    break;
                case 4:
                    //definicoes
                    if(storeEdit(loja)) break;
                case 5:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
    }
    private boolean storeEdit(Loja store){
        Login loginInfo = ta.getLoginInfo(1);
        boolean keep = true;
        while (keep){
            output.storeEdit();
            int option = Input.lerInt();
            switch (option){
                case 1:
                    //edit name
                    output.registerStoreName();
                    store.setName(Input.lerString());
                    break;
                case 2:
                    //alterar a localização
                    output.registerStoreLocation();
                    output.locationLongitude();
                    double x = Input.lerDouble();
                    output.locationLatitude();
                    double y = Input.lerDouble();
                    Location l = new Location(x,y);
                    ta.changeStoreLocation(store,l);
                    break;
                case 3:
                    // alterar password
                    output.oldPassword();
                    password = Input.lerString();
                    output.newPassword();
                    newPassword = Input.lerString();
                    if(loginInfo.changePassword(email, password, newPassword)){
                        output.changePasswordSuccessful();
                    }else {
                        output.changePasswordFailed();
                    }
                    break;
                case 4:
                    //delete account
                    output.confirmarRemocaoConta();
                    if(Input.lerBoolean()){
                        loginInfo.deleteRegister(email);
                    }
                    ta.getUserCatalog().remove(email);
                    return false;
                case 5:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        return true;
    }
    private void productStoreMenu(LinkedList<Produto> productCatalog){
        int page_size = 5;
        boolean keep = true;
        while (keep) {
            int i = 0;
            for(int j = i; j<page_size && j<productCatalog.size(); j++){
                output.printString( j+1 + "." + productCatalog.get(j).toString());
            }
            output.productStoreMenu();
            int option = Input.lerInt();
            switch (option) {
                case 1:
                    // add a product
                    String referencia,descricao;
                    double preco;
                    output.addReferencia();
                    referencia = Input.lerString();
                    output.addDescricao();
                    descricao = Input.lerString();
                    output.addPreco();
                    preco = Input.lerDouble();
                    Produto produto = new Produto(referencia,descricao,preco);
                    productCatalog.add(produto);
                    output.productSuccessful();
                    break;
                case 2:
                    // remove a product
                    output.getRemove();
                    int remove = Input.lerInt()-1;
                    if(productCatalog.size() > remove) {
                        productCatalog.remove(remove);
                        output.removeSuccessful();
                    }else{
                        output.invalidProduct();
                    }
                    break;
                case 3:
                    // edit a product
                    output.getEdit();
                    int edit = Input.lerInt()-1;
                    if(productCatalog.size() > edit) {
                        editProduto(productCatalog.get(edit));
                    }else{
                        output.invalidProduct();
                    }
                    break;
                case 4:
                    // next page
                    if(i + page_size < productCatalog.size())
                        i += page_size;
                    else
                        output.limiteException();
                    break;
                case 5:
                    // previous page
                    if(i-page_size > 0){
                        i += page_size;
                    }
                    else
                        output.limiteException();
                    break;
                case 6:
                    //leave
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
    }
    private void editProduto(Produto product){
        boolean keep = true;
        while(keep){
            output.productEdit();
            int option = Input.lerInt();
            switch (option){
                case 1:
                    //editar referencia
                    output.addReferencia();
                    product.setReferencia(Input.lerString());
                    break;
                case 2:
                    //editar descriçao
                    output.addDescricao();
                    product.setDescricao(Input.lerString());
                    break;
                case 3:
                    //editar preco
                    output.addPreco();
                    product.setPreco(Input.lerDouble());
                    break;
                case 4:
                    //leave
                    output.exit();
                    keep = false;
                    break;
        }
        }
    }
    private void storeRegister(){
        Loja loja = new Loja();
        loja.setStoreCode(email);
        output.registerStore();
        //setname
        output.registerStoreName();
        loja.setName(Input.lerString());
        //inserir localizaçao
        output.registerLocation();
        Location location = new Location(Input.lerDouble(),Input.lerDouble());
        loja.setLocation(location);
        ta.addLoja(loja);
    }

    //voluntario
    private void volunteerMenu(){

        Voluntario vol = ta.getVoluntarioCatalog().get(email);
        output.welcomeUser(vol.getName());
        output.volunteerMenu();
        boolean keep = true;
        while (keep) {
            int option = Input.lerInt();
            switch (option) { //Falta adicionar uma opção para o Voluntario escolher qual a encomenda que quer levar
                case 1:
                    //confirmar que pode levar uma encomenda
                    //percorre o gestor e encontra as encomendas que estao designadas a este
                    LinkedList<String> available = new LinkedList<>();
                    GestaoEncomenda encomenda = ta.getGestor();
                    for (Map.Entry<String, Encomenda> entry : encomenda.getGestor().entrySet()) {
                        Encomenda value = entry.getValue();
                        if(value.getCodigoTrans().equals(vol.getCodVoluntario())){
                               available.add(value.getCodigo());
                        }
                    }
                    for(String a : available){
                        output.printString(a);
                    }
                    //receber a escolha e coisar
                    String codigo = Input.lerString();
                    encomenda.getEncomenda(codigo).setEntregue(true);
                    ta.setGestor(encomenda);
                    ta.getVoluntarioCatalog().get(email).setEncomendas(Voluntario.addEnc(ta.getVoluntarioCatalog().get(email).getEncomendas(),codigo));
                    break;
                case 2:
                    //definicoes
                    if(volunteerEdit(vol)) break;
                case 3:
                    //historico
                    output.printHistEnc(ta.getVoluntarioCatalog().get(email).getEncomendas().toString());
                    output.getOutKey();
                    String ignored = Input.lerString();
                    break;
                case 4:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        output.volunteerMessage();
    }
    private void volunteerRegister(){
        Voluntario voluntario = new Voluntario();
        voluntario.setCodVoluntario(email);
        //inserir nome
        output.userName();
        voluntario.setName(Input.lerString());
        //inserir localizaçao
        output.registerLocation();
        Location location = new Location(Input.lerDouble(),Input.lerDouble());
        voluntario.setLocalizacao(location);
        //private double raio_de_acao;
        //private double rate;
        ta.addVoluntario(voluntario);
    }
    private boolean volunteerEdit(Voluntario vol){
        Login loginInfo = ta.getLoginInfo(2);
        boolean keep = true;
        while (keep){
            output.volunteerEdit();
            int option = Input.lerInt();
            switch (option){
                case 1:
                    //edit name
                    output.userName();
                    vol.setName(Input.lerString());
                    break;
                case 2:
                    //alterar a localização
                    output.registerLocation();
                    output.locationLongitude();
                    double x = Input.lerDouble();
                    output.locationLatitude();
                    double y = Input.lerDouble();
                    Location l = new Location(x,y);
                    vol.setLocalizacao(l);
                    break;
                case 3:
                    // alterar password
                    output.oldPassword();
                    password = Input.lerString();
                    output.newPassword();
                    newPassword = Input.lerString();
                    if(loginInfo.changePassword(email, password, newPassword)){
                        output.changePasswordSuccessful();
                    }else {
                        output.changePasswordFailed();
                    }
                    break;
                case 4:
                    //delete account
                    output.confirmarRemocaoConta();
                    if(Input.lerBoolean()){
                        loginInfo.deleteRegister(email);
                    }
                    ta.getUserCatalog().remove(email);
                    return false;
                case 5:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        return true;
    }

    //trasportadora
    private void transMenu() {

        EmpresaTransportadora emp = ta.getEmpresas().get(email);
        output.transMenu();
        boolean keep = true;
        while (keep) {
            int option = Input.lerInt();
            switch (option) {
                case 1:
                    //confirmar que pode levar uma encomenda
                    //percorre o gestor e encontra as encomendas que estao designadas a este
                    LinkedList<String> available = new LinkedList<>();
                    GestaoEncomenda encomenda = ta.getGestor();
                    for (Map.Entry<String, Encomenda> entry : encomenda.getGestor().entrySet()) {
                        Encomenda value = entry.getValue();
                        if(value.getCodigoTrans().equals(emp.getCodTrans())){
                            available.add(value.getCodigo());
                        }
                    }
                    for(String a : available){
                        output.printString(a);
                    }
                    //receber a escolha e coisar
                    String codigo = Input.lerString();
                    if(encomenda.existsEncomenda(codigo)) {
                        Encomenda pack = encomenda.getEncomenda(codigo);
                        pack.setEntregue(true);
                        //soma a distancia dele-loja-user-ele
                        emp.addDistanciaPercorrida(emp.getLocalizacao().distanceTo(ta.getLojas().get(pack.getCodigoLoja()).getLocation()));
                        emp.addDistanciaPercorrida(ta.getUserCatalog().get(pack.getCodigoUser()).getLocation().distanceTo(ta.getLojas().get(pack.getCodigoLoja()).getLocation()));
                        emp.addDistanciaPercorrida(emp.getLocalizacao().distanceTo(ta.getUserCatalog().get(pack.getCodigoUser()).getLocation()));
                    }
                    break;
                case 2:
                    if(transEdit(emp)) break;
                case 3:
                    //total faturado de uma empresa num intervalo
                    List<String> codigosEntregues = emp.getEncomendas();
                    GestaoEncomenda encomesdasEntregues = new GestaoEncomenda();
                    GestaoEncomenda encomendas = ta.getGestor();
                    for (String entry : codigosEntregues) {
                        encomesdasEntregues.addEncomenda(encomendas.getEncomenda(entry));
                    }
                    encomesdasEntregues.dataSort();
                    output.totalFaturado();
                    option = Input.lerInt();
                    double dist = 0;
                    double preco = 0;
                    switch (option) {
                        case 4:
                            output.exit();
                            break;
                        default:
                            output.invalidCommand();
                            break;
                        case 1:
                            //ultima semana
                            encomesdasEntregues.filterData(LocalDate.now().minus(1, ChronoUnit.WEEKS));
                            for (Map.Entry<String, Encomenda> entry : encomesdasEntregues.getGestor().entrySet()) {
                                dist = emp.getLocalizacao().distanceTo(ta.getLojas().get(entry.getValue().getCodigoLoja()).getLocation());
                                dist += ta.getUserCatalog().get(entry.getValue().getCodigoUser()).getLocation().distanceTo(ta.getLojas().get(entry.getValue().getCodigoLoja()).getLocation());
                                dist += emp.getLocalizacao().distanceTo(ta.getUserCatalog().get(entry.getValue().getCodigoUser()).getLocation());
                            }
                            preco = dist*emp.getTaxa();
                            for (Map.Entry<String, Encomenda> entry : encomesdasEntregues.getGestor().entrySet()) {
                                dist = entry.getValue().numeroTotalProdutos() * emp.getTaxapkg();
                            }
                            preco += dist;
                            output.printMoney(preco);
                            break;
                        case 2:
                            //ultimo mes
                            encomesdasEntregues.filterData(LocalDate.now().minus(1, ChronoUnit.MONTHS));
                        case 3:
                            for (Map.Entry<String, Encomenda> entry : encomesdasEntregues.getGestor().entrySet()) {
                                dist = emp.getLocalizacao().distanceTo(ta.getLojas().get(entry.getValue().getCodigoLoja()).getLocation());
                                dist += ta.getUserCatalog().get(entry.getValue().getCodigoUser()).getLocation().distanceTo(ta.getLojas().get(entry.getValue().getCodigoLoja()).getLocation());
                                dist += emp.getLocalizacao().distanceTo(ta.getUserCatalog().get(entry.getValue().getCodigoUser()).getLocation());
                            }
                            preco = dist*emp.getTaxa();
                            for (Map.Entry<String, Encomenda> entry : encomesdasEntregues.getGestor().entrySet()) {
                                dist = entry.getValue().numeroTotalProdutos() * emp.getTaxapkg();
                            }
                            preco += dist;
                            output.printMoney(preco);
                            break;
                    }
                    break;
                case 4:
                    //historico de entregas
                    output.printHistEnc(ta.getEmpresas().get(email).getEncomendas().toString());
                    output.getOutKey();
                    String ignored = Input.lerString();
                    break;
                case 5:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
    }
    private void transRegister(){
        EmpresaTransportadora trans = new EmpresaTransportadora();
        trans.setCodeTrans(email);
        //inserir nome
        output.registerTransName();
        trans.setName(Input.lerString());
        //inserir localizaçao
        output.registerLocation();
        Location location = new Location(Input.lerDouble(),Input.lerDouble());
        trans.setLocalizacao(location);
        //private int NIF;
        output.registerNif();
        trans.setNIF(Input.lerInt());
        //private double taxpkm;
        output.registerTaxaKm();
        trans.setTaxa(Input.lerDouble());
        //private double taxapkg;
        output.registerTaxaPeso();
        trans.setTaxapkg(Input.lerDouble());
        ta.addEmpresaTransportadora(trans);
    }
    private boolean transEdit(EmpresaTransportadora emp){
        Login loginInfo = ta.getLoginInfo(3);
        boolean keep = true;
        while (keep){
            output.transEdit();
            int option = Input.lerInt();
            switch (option){
                case 1:
                    //edit name
                    output.transName();
                    emp.setName(Input.lerString());
                    break;
                case 2:
                    //alterar a localização
                    output.registerLocation();
                    output.locationLongitude();
                    double x = Input.lerDouble();
                    output.locationLatitude();
                    double y = Input.lerDouble();
                    Location l = new Location(x,y);
                    emp.setLocalizacao(l);
                    break;
                case 3:
                    // alterar password
                    output.oldPassword();
                    password = Input.lerString();
                    output.newPassword();
                    newPassword = Input.lerString();
                    if(loginInfo.changePassword(email, password, newPassword)){
                        output.changePasswordSuccessful();
                    }else {
                        output.changePasswordFailed();
                    }
                    break;
                case 4:
                    //delete account
                    output.confirmarRemocaoConta();
                    if(Input.lerBoolean()){
                        loginInfo.deleteRegister(email);
                    }
                    ta.getUserCatalog().remove(email);
                    return false;
                case 5:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
        return true;
    }

    //adm
    private void admimMenu(){

        boolean keep = true;
        while (keep){
            output.admMenu();
            int option = Input.lerInt();
            switch (option){
                case 1:
                    //Adicionar logs (parse)
                    output.perguntaFile();
                    parser.parse(ta,Input.lerString());
                    break;
                case 2:
                    //mostrar registos dos utilizadores todos(emais e passwords) um banhamer era humilde tambem
                    acauntManager();
                    break;
                case 3:
                    //requesitos dos top 10
                    output.toptenMenu();
                    option = Input.lerInt();
                    switch (option){
                        case 1:
                            output.tenUser();
                            output.printMap(ta.topTenUser());
                            break;
                        case 2:
                            output.tenTrans();
                            output.printMapDouble(ta.topTenTrans());
                            break;
                        case 3:
                            output.exit();
                            keep = false;
                            break;
                        default:
                            output.invalidCommand();
                            break;
                    }
                    break;
                case 4:
                    //delete current state
                    output.nuke();
                    switch (Input.lerInt()){
                        case 1:
                            Load_Save state = new Load_Save("saveState.txt");
                            state.resetFileInfo();
                            ta = new TrazAqui();
                            return;
                        case 2:
                            break;
                        default:
                            output.invalidCommand();
                            break;
                    }
                    break;
                case 5:
                    //manage encomendas
                    packageManager();
                    break;
                case 6:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
    }
    private void acauntManager(){
        boolean keep = true;
        while(keep) {
            output.loginType();
            int menu = Input.lerInt();
            switch (menu) {
                case 1:
                case 2:
                case 3:
                case 4:
                case 5:
                    Login login = ta.getLoginInfo(menu);
                    // por aqui opçao de remover um utilizador
                    while (keep){
                        login.getRegisters().forEach((key, value) -> output.printString(key + "-" + value + "|"));
                        output.manageAccounts();
                        int option = Input.lerInt();
                        switch (option){
                            case 1:
                                //remove an acount
                                output.deleteAccount();
                                String conta = Input.lerString();
                                if(login.containsRegister(conta)){
                                    switch (menu){
                                        case 1:
                                            ta.getUserCatalog().remove(conta);
                                            break;
                                        case 2:
                                            ta.getLojas().remove(conta);
                                            break;
                                        case 3:
                                            ta.getEmpresas().remove(conta);
                                            break;
                                        case 4:
                                            ta.getVoluntarioCatalog().remove(conta);
                                            break;
                                        case 5:
                                            break;
                                        default:
                                            output.invalidCommand();
                                            break;

                                    }
                                    login.deleteRegister(conta);
                                }
                                break;
                            case 2:
                                output.exit();
                                keep = false;
                                break;
                            default:
                                output.invalidCommand();
                                break;
                        }
                    }
                    break;
                case 6:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
    }
    private void packageManager(){
        boolean keep = true;
        while(keep) {
            for (Map.Entry<String, Encomenda> entry : ta.getGestor().getGestor().entrySet()) {
                output.printString(entry.getValue().toString());
            }
            output.manageEncomendas();
            int menu = Input.lerInt();
            switch (menu) {
                case 1:
                    //remove a package
                    output.deleteEncomenda();
                    String encomenda = Input.lerString();
                    if (ta.getGestor().existsEncomenda(encomenda)) {
                        //deleting
                        output.removeSuccessful();
                        ta.getGestor().removeEncomenda(encomenda);
                    } else {
                        //not found
                        output.removeFailed();
                    }
                    break;
                case 2:
                    output.exit();
                    keep = false;
                    break;
                default:
                    output.invalidCommand();
                    break;
            }
        }
    }
}

