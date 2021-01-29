package MVC.Controller;

import java.util.ArrayList;
import java.util.List;


import Base.Basic.Coordenadas;
import MVC.Model.Model;
import Users.Loja;

public class ControllerLoja extends Controller {

    private final String[] lojaRegister = {"Loja(Register)"};
    private final String[] lojaLogin = {"Loja(Login)"};
    private final String[] jaExiste = {"Loja já registada"};
    private final String[] naoExiste = {"Loja não existe"};
    private final String[] nomeRegister = {"Nome(Register)"};
    private final String[] password = {"Password"};
    private final String[] passwordRegister = {"Password(Register)"};
    private final String[] passwordRegister1 = {"Password (Confirm Password)"};
    private final String[] lojaQueue = {"Queue"};
    private final String[] gpsRegister = {"GPS(Register)_0.0,0.0"};
    private final String[] quit = {"Quit"};
    private final String[] passwordErrada = {"Password Errada"};
    private final String[] loginSuccess = {"Menu da Loja", "Sinalizar Encomenda Disponivel", "Verificar Queue"};

    
    private ArrayList<Object> cache = new ArrayList<>(5);

    private String loja = new String();

    public ControllerLoja(Model model){
        super(model);
    }

    @Override
    protected void update(){
        String[] campos = this.getOption().split("-");

        switch(campos[0]){
            case "Menu":
                if (campos.length == 1){
                    break;
                }
                switch(campos[1]){
                    case "0":
                        this.setScreen(quit);
                        break;
                    case "1":
                        this.setScreen(lojaLogin);
                        break;
                    case "2":
                        this.setScreen(lojaRegister);
                }
                break;
            //Register Shop
            case "Loja(Register)":
                if (campos.length == 1){
                    break;
                }
                if (this.getModel().containsLoja(campos[1])){
                    this.setScreen(jaExiste);
                }
                else {
                    cache.add(0,campos[1]);
                    this.setScreen(passwordRegister);
                }
                break;

            case "Password(Register)":
                if(campos.length == 1) {
                    break;
                }
                cache.add(campos[1]);
                this.setScreen(passwordRegister1);
                break;

            case "Password (Confirm Password)":
                if(campos.length == 1) {
                    break;
                }
                if(((String) cache.get(1)).equals(campos[1])){
                    this.setScreen(gpsRegister);
                }
                else {
                    cache.clear();
                    this.setScreen(getLogin());
                }
                break;

            case "GPS(Register)":
                if(campos.length == 1) {
                    break;
                }
                String[] coord = campos[1].split(",");
                if(coord.length == 2){
                    Coordenadas coordenadas = new Coordenadas();
                    try {
                        coordenadas.setLatitude(Double.parseDouble(coord[0]));
                    } catch (NumberFormatException e) {
                        this.setScreen(gpsRegister);
                    } catch (NullPointerException e) {
                        this.setScreen(gpsRegister);
                    }
                    try {
                        coordenadas.setLongitude(Double.parseDouble(coord[1]));
                    } catch (NumberFormatException e) {
                        this.setScreen(gpsRegister);
                    } catch (NullPointerException e) {
                        this.setScreen(gpsRegister);
                    }
                    cache.add(coordenadas);
                    this.setScreen(lojaQueue);
                }
                else {
                    setScreen(gpsRegister);
                }
                break;

            case "Queue":
                if(campos.length == 1) {
                    break;
                }
                try {
                    cache.add(Integer.valueOf(campos[1]));
                    this.setScreen(nomeRegister);
                } catch (NumberFormatException e) {
                    this.setScreen(lojaQueue);
                }
                
                
                break;
                
            case "Nome(Register)":
                if(campos.length == 1) {
                    break;
                }
                cache.add(campos[1]);
                this.getModel().addLoja(new Loja((String)cache.get(0), (String)cache.get(4), (Coordenadas)cache.get(2), (Integer)cache.get(3), (String)cache.get(1)));
                cache.clear();
                this.setScreen(super.getLogin());
                break;
            //LOGIN
            case "Loja(Login)":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1] == "0") {
                    setScreen(lojaLogin);
                }
                if (this.getModel().containsLoja(campos[1])) {
                    loja = campos[1];
                    cache.add(campos[1]);
                    this.setScreen(password);
                }
                else {
                    this.setScreen(naoExiste);
                }
                break;
            case "Password":
                if(campos.length == 1) {
                    break;
                }
                if (this.getModel().passwordLoja(((String) cache.get(0)), campos[1])) {
                    cache.clear();
                    setScreen(loginSuccess);
                }
                else {
                    setScreen(passwordErrada);
                }
                break;
            case "Password Errada":
                cache.clear();
                this.setScreen(super.getLogin());
                break;
            case "Loja não Existe":
                cache.clear();
                this.setScreen(super.getLogin());
                break;
            //MENU SHOP
            case "Menu da Loja":
                if(campos.length == 1) {
                    break;
                }
                switch (campos[1]) {
                    case "1":
                        List<String> list = new ArrayList<String>();
                        list.add("Sinalizar Encomenda Disponivel");
                        for (String string : this.getModel().getEncomendasLoja(loja)) {
                            list.add(string);
                        }
                        for (String string : list) {
                            cache.add(string);
                        }
                        setScreen(list);
                        break;
                    case "2":
                        List<String> list2 = new ArrayList<String>();
                        list2.add("Queue:");
                        if (this.getModel().getQueueLoja(loja) == 0){
                            list2.add("0");
                        }
                        else{
                            list2.add(String.valueOf(this.getModel().getQueueLoja(loja)));
                        }
                        setScreen(list2);
                        break;
                    case "0":
                        loja = "";
                        setScreen(this.getLogin());
                        break;
                }
                break;
            case "Sinalizar Encomenda Disponivel":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSuccess);
                    cache.clear();
                }
                else if (campos[1].matches("^[0-9]+$")){
                    int ret = Integer.parseInt(campos[1]);
                    if (ret<=cache.size()){
                        String encomenda = (String)cache.get(ret);
                        this.getModel().addSinalizadas(encomenda);
                        cache.clear();
                        this.setScreen(loginSuccess);
                    }
                }
                break;

        }
    }
}