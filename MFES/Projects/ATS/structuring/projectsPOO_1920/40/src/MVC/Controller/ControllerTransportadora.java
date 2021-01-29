package MVC.Controller;

import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;

import Base.Basic.Coordenadas;
import Base.Basic.Pair;
import Base.Encomenda.Aceite;
import MVC.Controller.Menu.Menu;
import MVC.Model.Model;
import Users.Transportadora;

public class ControllerTransportadora extends Controller{
    
    private final String[] transportadoraRegister = {"Transportadora(Register)"};
    private final String[] transportadoraLogin = {"Transportadora(Login)"};
    private final String[] jaExiste = {"Transportadora já Registada"};
    private final String[] naoExiste = {"Transportadora não Existe"};
    private final String[] password = {"Password"};
    private final String[] passwordRegister = {"Password(Register)"};
    private final String[] passwordRegister1 = {"Password(Register Again)"};
    private final String[] gpsRegister = {"GPS(Register)_0.0,0.0"};
    private final String[] nif = {"NIF"};
    private final String[] raio = {"Raio"};
    private final String[] nomeRegister = {"Nome(Register)"};
    private final String[] passwordErrada = {"Password Errada"};
    private final String[][] loginSucess = {{"Menu Transportadora", "Toogle On", "Encomenda Preço", "Entregar", "Faturado Periodo"},
                                            {"Menu Transportadora", "Toogle Off", "Encomenda Preço", "Entregar", "Faturado Periodo"}};

    private final String[] quit = {"quit"};
    private int on = 0;

    private ArrayList<Object> cache = new ArrayList<>();
    private String transportadora = new String();

    public ControllerTransportadora(Model model){
        super(model);
    }

    @Override
    protected void update() {
        String[] campos = this.getOption().split("-");
        if(campos.length > 2) {
            for (int i = 2; i < campos.length; i++) {
                campos[1] = campos[1] + "-" + campos[i];
            }
        }

        switch (campos[0]) {
            case "Menu":
                if(campos.length == 1) {
                    break;
                }
                switch(campos[1]) {
                    case "0":
                        this.setScreen(quit);
                        break;
                    case "1":
                        this.setScreen(transportadoraLogin);
                        break;
                    case "2":
                        this.setScreen(transportadoraRegister);
                        break;
                }
                break;
            //REGISTER
            case "Transportadora(Register)":
                if(campos.length == 1) {
                    break;
                }
                if (this.getModel().containsTransportadora(campos[1])) {
                    this.setScreen(jaExiste);
                }
                else {
                    cache.add(campos[1]);//0
                    this.setScreen(passwordRegister);
                }
                break;
            case "Password(Register)":
                if(campos.length == 1) {
                    break;
                }
                cache.add(campos[1]);//1
                this.setScreen(passwordRegister1);
                break;
            case "Password(Register Again)":
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
                    try {
                        cache.add(Double.parseDouble(coord[0]));//2
                    } catch (NumberFormatException e) {
                        this.setScreen(gpsRegister);
                    } catch (NullPointerException e) {
                        this.setScreen(gpsRegister);
                    }
                    try {
                        cache.add(Double.parseDouble(coord[1]));//3
                    } catch (NumberFormatException e) {
                        cache.remove(cache.size()-1);
                        this.setScreen(gpsRegister);
                    } catch (NullPointerException e) {
                        cache.remove(cache.size()-1);
                        this.setScreen(gpsRegister);
                    }
                    this.setScreen(nif);
                }
                else {
                    setScreen(gpsRegister);
                }
                break;
            case "NIF":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(super.getLogin());
                    cache.clear();
                }
                else if (campos[1].matches("^[0-9]+$")) {
                    try {
                        cache.add(Integer.parseInt(campos[1]));//4
                        this.setScreen(raio);
                    } catch (NumberFormatException e) {
                        this.setScreen(nif);
                    }
                }
                break;
            case "Raio":
                if(campos.length == 1) {
                    break;
                }
                try {
                    cache.add(Double.parseDouble(campos[1]));//5
                    this.setScreen(new Menu("Price Per Km"));
                } catch (NumberFormatException e) {
                    this.setScreen(raio);
                } catch (NullPointerException e) {
                    this.setScreen(raio);
                }
                break;
            case "Price Per Km":
                if(campos.length == 1) {
                    break;
                }
                try {
                    cache.add(Double.parseDouble(campos[1]));//6
                    this.setScreen(new Menu("Price Per Kg"));
                } catch (NumberFormatException e) {
                    this.setScreen(new Menu("Price Per Km"));
                } catch (NullPointerException e) {
                    this.setScreen(new Menu("Price Per Km"));
                }
                break;
            case "Price Per Kg":
                if(campos.length == 1) {
                    break;
                }
                try {
                    cache.add(Double.parseDouble(campos[1]));//7
                    this.setScreen(nomeRegister);
                } catch (NumberFormatException e) {
                    this.setScreen(new Menu("Price Per Kg"));
                } catch (NullPointerException e) {
                    this.setScreen(new Menu("Price Per Kg"));
                }
                break;
            case "Nome(Register)":
                if(campos.length == 1) {
                    break;
                }
                this.getModel().addTransportadora(new Transportadora((String) cache.get(0), (String) cache.get(1) ,campos[1], new Coordenadas((Double) cache.get(2), (Double) cache.get(3)),
                                                                        (Integer) cache.get(4), (Double) cache.get(5), (Double) cache.get(6), false, (Double) cache.get(7), null, 0));
                cache.clear();
                this.setScreen(getLogin());
                break;
            case "Transportadora já Registada":
                cache.clear();
                this.setScreen(super.getLogin());
                break;
            //LOGIN
            case "Transportadora(Login)":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1] == "0") {
                    setScreen(transportadoraLogin);
                }
                if (this.getModel().containsTransportadora(campos[1])) {
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
                if (this.getModel().passwordTransportadora(((String) cache.get(0)), campos[1])) {
                    transportadora = (String) cache.get(0);
                    this.on = this.getModel().transportadoraOn(transportadora);
                    cache.clear();
                    setScreen(loginSucess[this.on]);
                }
                else {
                    setScreen(passwordErrada);
                }
                break;
            case "Password Errada":
                cache.clear();
                this.setScreen(super.getLogin());
                break;
            case "Transportadora não Existe":
                cache.clear();
                this.setScreen(super.getLogin());
                break;
            case "Menu Transportadora":
                if(campos.length == 1) {
                    break;
                }
                switch (campos[1]) {
                    case "1":
                        this.on = this.getModel().toogleTransportadora(transportadora);
                        setScreen(loginSucess[this.on]);
                        break;
                    case "2":
                        Pair<List<String>,List<String>> lists = this.getModel().getEncomendasTransportadora(transportadora);
                        List<String> list = lists.getSecond();
                        list.add(0,"Escolher Encomenda(Preço)");
                        for (String string : lists.getFirst()) {
                            cache.add(string);
                        }
                        setScreen(list);
                        break;
                    case "3":
                        Pair<List<String>,List<String>> listss = this.getModel().getEncomendasTransportadora(transportadora);
                        List<String> list1 = listss.getSecond();
                        list1.add(0,"Escolher Encomenda(Entregar)");
                        for (String string : listss.getFirst()) {
                            cache.add(string);
                        }
                        setScreen(list1);
                        break;
                    case "4":
                        setScreen(new Menu("Periodo_(2007-12-03T10:15:30/2007-12-03T10:16:30)"));
                        break;
                    case "0":
                        transportadora = "";
                        setScreen(this.getLogin());
                        break;
                }
                break;
            case "Escolher Encomenda(Preço)":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess[this.on]);
                    cache.clear();
                }
                else if (campos[1].matches("^[0-9]+$")) {
                    int ret = Integer.parseInt(campos[1]);
                    if(ret <= cache.size()) {
                        String encomenda = (String) cache.get(ret-1);
                        cache.clear();
                        Double preco = this.getModel().preco(encomenda);
                        setScreen(new Menu("Preço_" + preco.toString() + "€"));
                    }
                }
                break;
            case "Preço":
                cache.clear();
                setScreen(loginSucess[this.on]);
                break;
            case "Escolher Encomenda(Entregar)":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess[this.on]);
                    cache.clear();
                }
                else if (campos[1].matches("^[0-9]+$")) {
                    int ret = Integer.parseInt(campos[1]);
                    if(ret <= cache.size()) {
                        String encomenda = (String) cache.get(ret-1);
                        cache.clear();
                        cache.add(encomenda);
                        Double preco = this.getModel().preco(encomenda);

                        setScreen(new Menu("Adicionar Tempo em Minutos_Preço:" + preco.toString() + "€"));
                    }
                }
                break;
            case "Adicionar Tempo em Minutos":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess[this.on]);
                    cache.clear();
                }
                else if (campos[1].matches("^[0-9]+$")) {
                    int min = Integer.parseInt(campos[1]);
                    this.getModel().setEncomenda((String) cache.get(0), min);
                    this.getModel().addEntregues(new Aceite((String) cache.get(0)));
                    cache.clear();
                    this.setScreen(loginSucess[this.on]);
                }
                break;
            case "Periodo":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess[this.on]);
                    cache.clear();
                }
                else {
                    try {
                        String[] periodos = campos[1].split("/");
                        LocalDateTime antes = LocalDateTime.parse(periodos[0]);
                        LocalDateTime depois = LocalDateTime.parse(periodos[1]);
                        this.setScreen(new Menu("Preço _ " + this.getModel().totalFaturado(transportadora, antes, depois)));
                    } catch (DateTimeParseException e) {
                    }
                }
                break;
            case "Preço ":
                cache.clear();
                setScreen(loginSucess[this.on]);
                break;
        }
        return;
    }

}