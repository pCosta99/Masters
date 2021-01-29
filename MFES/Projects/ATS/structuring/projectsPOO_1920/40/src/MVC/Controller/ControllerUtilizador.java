package MVC.Controller;

import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;

import Base.Basic.Coordenadas;
import Base.Basic.Pair;
import Base.Encomenda.Aceite;
import Base.Encomenda.Encomenda;
import Base.Encomenda.LinhaEncomenda;
import MVC.Controller.Menu.Menu;
import MVC.Model.Model;
import Users.Utilizador;
import java.util.Random;

public class ControllerUtilizador extends Controller{

    private final String[] utilizadorRegister = {"Utilizador(Register)"};
    private final String[] utilizadorLogin = {"Utilizador(Login)"};
    private final String[] jaExiste = {"Utilizador já Registado"};
    private final String[] naoExiste = {"Utilizador não Existe"};
    private final String[] password = {"Password"};
    private final String[] passwordRegister = {"Password(Register)"};
    private final String[] passwordRegister1 = {"Password(Register Again)"};
    private final String[] gpsRegister = {"GPS(Register)_0.0,0.0"};
    private final String[] nomeRegister = {"Nome(Register)"};
    private final String[] passwordErrada = {"Password Errada"};
    private final String[] loginSucess = {"Menu Utilizador", "Solicitar Entrega de Encomenda ~ Voluntário", "Solicitar Entrega de Encomenda ~ Transportadora", "Entregas Efetuadas", "Classificar Ultima Entrega", "Adicionar Encomenda"};
    private final String[] chosePeriodoType = {"Periodo~Escolha", "Todos", "Voluntários", "Transportadoras"};

    private final String[] quit = {"quit"};

    private ArrayList<Object> cache = new ArrayList<>();
    private String utilizador = new String();

    public ControllerUtilizador(Model model){
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
                        this.setScreen(utilizadorLogin);
                        break;
                    case "2":
                        this.setScreen(utilizadorRegister);
                        break;
                }
                break;
            //REGISTER
            case "Utilizador(Register)":
                if(campos.length == 1) {
                    break;
                }
                if (this.getModel().contains(campos[1])) {
                    this.setScreen(jaExiste);
                }
                else {
                    cache.add(campos[1]);
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
                        cache.add(Double.parseDouble(coord[0]));
                    } catch (NumberFormatException e) {
                        this.setScreen(gpsRegister);
                    } catch (NullPointerException e) {
                        this.setScreen(gpsRegister);
                    }
                    try {
                        cache.add(Double.parseDouble(coord[1]));
                    } catch (NumberFormatException e) {
                        cache.remove(cache.size()-1);
                        this.setScreen(gpsRegister);
                    } catch (NullPointerException e) {
                        cache.remove(cache.size()-1);
                        this.setScreen(gpsRegister);
                    }
                    this.setScreen(nomeRegister);
                }
                else {
                    setScreen(gpsRegister);
                }
                break;
            case "Nome(Register)":
                if(campos.length == 1) {
                    break;
                }
                this.getModel().addUtilizador(new Utilizador(((String) cache.get(0)), campos[1], ((String) cache.get(0)), ((String) cache.get(1)), new Coordenadas(((Double) cache.get(2)), ((Double) cache.get(3)))));
                cache.clear();
                this.setScreen(getLogin());
                break;
            case "Utilizador já Registado":
                cache.clear();
                this.setScreen(super.getLogin());
                break;
            //LOGIN
            case "Utilizador(Login)":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1] == "0") {
                    setScreen(utilizadorLogin);
                }
                if (this.getModel().contains(campos[1])) {
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
                if (this.getModel().password(((String) cache.get(0)), campos[1])) {
                    utilizador = (String) cache.get(0);
                    cache.clear();
                    setScreen(loginSucess);
                }
                else {
                    setScreen(passwordErrada);
                }
                break;
            case "Password Errada":
                cache.clear();
                this.setScreen(super.getLogin());
                break;
            case "Utilizador não Existe":
                cache.clear();
                this.setScreen(super.getLogin());
                break;
            case "Menu Utilizador":
                if(campos.length == 1) {
                    break;
                }
                switch (campos[1]) {
                    case "1":
                        Pair<List<String>,List<String>> lists = this.getModel().getEncomendas(utilizador);
                        List<String> list = lists.getSecond();
                        list.add(0,"Solicitar Entrega de Encomenda ~ Voluntário");
                        for (String string : lists.getFirst()) {
                            cache.add(string);
                        }
                        setScreen(list);
                        break;
                    case "2":
                        Pair<List<String>,List<String>> listss = this.getModel().getEncomendas(utilizador);
                        List<String> list2 = listss.getSecond();
                        list2.add(0,"Solicitar Entrega de Encomenda ~ Transportadora");
                        for (String string : listss.getFirst()) {
                            cache.add(string);
                        }
                        setScreen(list2);
                        break;
                    case "3":
                        setScreen(new Menu("Periodo_(2007-12-03T10:15:30/2007-12-03T10:16:30)"));
                        break;
                    case "4":
                        Pair<List<String>,List<String>> list10 = this.getModel().getRatings(utilizador);
                        cache.add(list10.getFirst());
                        List<String> list11 = list10.getSecond();
                        list11.add(0,"Ratings");
                        setScreen(list11);
                        break;
                    case "5":
                        Pair<List<String>,List<String>> list12 = this.getModel().getLojas();
                        cache.add(list12.getFirst());
                        List<String> list13 = list12.getSecond();
                        list13.add(0,"Lojas");
                        setScreen(list13);
                        break;
                    case "0":
                        utilizador = "";
                        setScreen(this.getLogin());
                        break;
                }
                break;
            case "Solicitar Entrega de Encomenda ~ Voluntário":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    cache.clear();
                }
                else if (campos[1].matches("^[0-9]+$")) {
                    int ret = Integer.parseInt(campos[1]);
                    if(ret <= cache.size()) {
                        String encomenda = (String) cache.get(ret-1);
                        cache.clear();
                        cache.add(encomenda);
                        Pair<List<String>,List<String>> list2 = this.getModel().getVoluntarios(encomenda);
                        if(list2.getFirst().size() == 0) {
                            cache.clear();
                            this.setScreen(loginSucess);
                        }
                        else {
                            cache.add(list2.getFirst());
                            List<String> list3 = list2.getSecond();
                            list3.add(0,"Escolhas Voluntario");
                            setScreen(list3);
                        }
                    }
                }
                break;
            case "Solicitar Entrega de Encomenda ~ Transportadora":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    cache.clear();
                }
                else if (campos[1].matches("^[0-9]+$")) {
                    int ret = Integer.parseInt(campos[1]);
                    if(ret <= cache.size()) {
                        String encomenda = (String) cache.get(ret-1);
                        cache.clear();
                        cache.add(encomenda);
                        Pair<List<String>,List<String>> list2 = this.getModel().getTransportadoras(encomenda);
                        if(list2.getFirst().size() == 0) {
                            cache.clear();
                            this.setScreen(loginSucess);
                            break;
                        }
                        else {
                            cache.add(list2.getFirst());
                            List<String> list3 = list2.getSecond();
                            list3.add(0,"Escolhas Transportadoras");
                            setScreen(list3);
                        }
                    }
                }
                break;
            case "Escolhas Transportadoras":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    cache.clear();
                }
                else if(campos[1].matches("^[0-9]+$")) {
                    int ret = Integer.parseInt(campos[1]);
                    ArrayList<String> list4 = (ArrayList<String>) cache.get(1);
                    if(ret <= list4.size()) {
                        this.getModel().addAceite(list4.get(ret-1), new Aceite(((String) cache.get(0))));
                        this.setScreen(loginSucess);
                        cache.clear();
                    }
                }
                break;
            case "Escolhas Voluntario":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    cache.clear();
                }
                else if(campos[1].matches("^[0-9]+$")) {
                    int ret = Integer.parseInt(campos[1]);
                    ArrayList<String> list5 = (ArrayList<String>) cache.get(1);
                    if(ret <= list5.size()) {
                        this.getModel().addAceite(list5.get(ret-1), new Aceite(((String) cache.get(0))));
                        this.setScreen(loginSucess);
                        cache.clear();
                    }
                }
                break;
            case "Periodo":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    cache.clear();
                }
                else {
                    try {
                        String[] periodos = campos[1].split("/");
                        cache.add(LocalDateTime.parse(periodos[0]));
                        cache.add(LocalDateTime.parse(periodos[1]));
                        setScreen(chosePeriodoType);
                    } catch (DateTimeParseException e) {
                    }
                }
                break;
            case "Periodo~Escolha":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    cache.clear();
                }
                switch (campos[1]) {
                    case "1":
                        List<String> list6 = this.getModel().getPeriodo(utilizador, (LocalDateTime) cache.get(0), (LocalDateTime) cache.get(1));
                        list6.add(0,"Periodo~Resultados");
                        setScreen(list6);
                        break;
                    case "2":
                        List<String> list7 = this.getModel().getPeriodo(utilizador, (LocalDateTime) cache.get(0), (LocalDateTime) cache.get(1));
                        list7.add(0,"Periodo~Resultados");
                        setScreen(list7);
                        break;
                    case "3":
                        List<String> list8 = this.getModel().getPeriodo(utilizador, (LocalDateTime) cache.get(0), (LocalDateTime) cache.get(1));
                        list8.add(0,"Periodo~Resultados");
                        setScreen(list8);
                        break;
                }
                break;
            case "Periodo~Resultados":
                cache.clear();
                setScreen(loginSucess);
                break;
            case "Ratings":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    cache.clear();
                }
                else if (campos[1].matches("^[0-9]+$")) {
                    int ret = Integer.parseInt(campos[1]);
                    ArrayList<String> list12 = (ArrayList<String>) cache.get(0);
                    if(ret <= list12.size()) {
                        this.setScreen(new Menu("Rating encomenda_" + list12.get(ret-1)));
                        cache.clear();
                        cache.add(list12.get(ret-1));
                    }
                }
                break;
            case "Rating encomenda":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    cache.clear();
                }
                if(campos[1].matches("^[1-5]$")) {
                    this.getModel().SetRating((String) cache.get(0), Double.parseDouble(campos[1]));
                    cache.clear();
                    this.setScreen(loginSucess);
                }
                break;
            case "Lojas":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    cache.clear();
                }
                else if (campos[1].matches("^[0-9]+$")) {
                    int ret = Integer.parseInt(campos[1]);
                    ArrayList<String> list12 = (ArrayList<String>) cache.get(0);
                    if(ret <= list12.size()) {
                        this.setScreen(new Menu("Encomenda_" + "(CodigoProduto,descricao,quantidade,valor unitario, peso)"));
                        cache.clear();
                        
                        cache.add(new Encomenda("", utilizador, list12.get(ret-1), 0, null));
                    }
                }
                break;
            case "Encomenda":
                if(campos.length == 1) {
                    break;
                }
                if(campos[1].equals("0")) {
                    this.setScreen(loginSucess);
                    String random = ((Integer) (new Random()).nextInt(10000)).toString();
                    String paddedNumberAsString = "0000".substring(random.length()) + random;
                    while(this.getModel().containsEncomenda(paddedNumberAsString)) {
                        random = ((Integer) (new Random()).nextInt(10000)).toString();
                        paddedNumberAsString = "0000".substring(random.length()) + random;
                    }
                    Encomenda x = (Encomenda) cache.get(0);
                    x.setCodEncomenda("e" + paddedNumberAsString);
                    if(!x.listaEmpty()) {
                        this.getModel().addEncomendas(x);
                    }
                    cache.clear();
                }
                else {
                    String linhas[] = campos[1].split(",");
                    int quantidade;
                    double valorU;
                    double peso;
                    if(linhas.length == 5) {
                        try {
                            if(linhas[0].isEmpty()) break;
                            if(linhas[1].isEmpty()) break;
                            if((quantidade = Integer.parseInt(linhas[2])) <= 0) break;
                            if((valorU = Double.parseDouble(linhas[3])) <= 0.0) break;
                            if((peso = Double.parseDouble(linhas[4])) <= 0.0) break;
                            Encomenda x = (Encomenda) cache.get(0);
                            x.addPeso(peso);
                            x.addLinhaEncomenda(new LinhaEncomenda(linhas[0], linhas[1], quantidade, valorU));
                            this.setScreen(new Menu("Encomenda_" + "(CodigoProduto,descricao,quantidade,valor unitario, peso)"));
                        } catch (NumberFormatException e) {
                        } catch (NullPointerException e) {
                        }
                    }
                }
                break;
        }
        return;
    }

    

}