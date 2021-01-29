
package MVC.Controller;

import java.util.ArrayList;
import java.util.List;

import Base.Basic.Coordenadas;
import Base.Basic.Pair;
import Base.Encomenda.Aceite;
import MVC.Controller.Menu.Menu;
import MVC.Model.Model;
import Users.Voluntario;

public class ControllerVoluntario extends Controller{
    private Menu menu[];
    private final String[] voluntarioRegister = {"Voluntário(Register)"};
    private final String[] voluntarioLogin = {"Voluntário(Login)"};
    private final String[] jaExiste = {"Voluntário já Registado"};
    private final String[] naoExiste = {"Voluntário não Existe"};
    private final String[] password = {"Password"};
    private final String[] passwordRegister = {"Password(Register)"};
    private final String[] passwordRegister1 = {"Password(Register Again)"};
    private final String[] gpsRegister = {"GPS(Register)_0.0,0.0"};
    private final String[] raioRegister = {"Raio(Register)"};
    private final String[] nomeRegister = {"Nome(Register)"};
    private final String[] passwordErrada = {"Password Errada"};
    private final String[][] loginSucess = {{"Menu Voluntário", "Toggle On", "Decidir Entrega de Encomenda ~ Loja", "Tempo de transporte"},
                                        {"Menu Voluntário", "Toggle Off", "Decidir Entrega de Encomenda ~ Loja", "Tempo de transporte"}};
    private final String[] quit = {"quit"};
    int on = 0;
    String voluntario =  new String();
    int option = 0;

    private ArrayList<Object> cache = new ArrayList<>();

    public ControllerVoluntario(Model model){
        super(model);
    }

    @Override
    protected void update() {
        String[] campos = this.getOption().split("-");
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
                        this.setScreen(voluntarioLogin);
                        break;
                    case "2":
                        this.setScreen(voluntarioRegister);
                        break;
                }
                break;
        //REGISTER
        case "Voluntário(Register)":
            if(campos.length == 1) {
                break;
            }
            if (this.getModel().containsVoluntario(campos[1])) {
                this.setScreen(jaExiste);
            }
            else {
                cache.add(campos[1]); //0
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
                this.setScreen(raioRegister);
            }
            else {
                cache.clear();
                this.setScreen(getLogin());
            }
            break;

        case "Raio(Register)":
            if(campos.length == 1) {
                break;
            }
            try {
                cache.add(Double.parseDouble(campos[1]));
                this.setScreen(gpsRegister);
            } catch (NumberFormatException e) {
                this.setScreen(raioRegister);
            } catch (NullPointerException e) {
                this.setScreen(raioRegister);
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
            cache.add(campos[1]);
            this.getModel().addVoluntario(new Voluntario((String)cache.get(4), (String)cache.get(1),(String)cache.get(0), (Coordenadas)cache.get(3), (Double) cache.get(2), false));
            cache.clear();
            this.setScreen(getLogin());
            break;

        case "Voluntário já Registado":
            cache.clear();
            this.setScreen(super.getLogin());
            break;
        //LOGIN
        case "Voluntário(Login)":
            if(campos.length == 1) {
                break;
            }
            if (this.getModel().containsVoluntario(campos[1])) {
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
            if (this.getModel().passwordVoluntario(((String) cache.get(0)), campos[1])) {
                voluntario = (String) cache.get(0);
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
        
        case "Voluntário não Existe":
            cache.clear();
            this.setScreen(super.getLogin());
            break;

        case "Menu Voluntário":
            if(campos.length == 1) {
                break;
            }
        
            switch (campos[1]) {
                case "1":
                    this.on = this.getModel().toggleVoluntario(voluntario);
                    setScreen(loginSucess[this.on]);
                break;
                case "2":
                    option = 0;
                    Pair<List<String>,List<String>> lists = this.getModel().getEncomendasVoluntario(voluntario);
                    List<String> list = lists.getSecond();
                    list.add(0,"Escolher Encomenda");
                    for (String string : lists.getFirst()) {
                        cache.add(string);
                    }
                    setScreen(list);
                    break;
                case "3":
                    option = 1;
                    Pair<List<String>,List<String>> listss = this.getModel().getEncomendasVoluntario(voluntario);
                    List<String> list1 = listss.getSecond();
                    list1.add(0,"Escolher Encomenda");
                    for (String string : listss.getFirst()) {
                        cache.add(string);
                    }
                    setScreen(list1);
                    break;
                case "0":
                    voluntario = "";
                    setScreen(this.getLogin());
                    break;
            }
            break;
        case "Escolher Encomenda":
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
                    if(option == 1) {
                        setScreen(new Menu("Adicionar Tempo em Minutos_" + encomenda));
                    } else {
                        setScreen(new Menu("Vais Buscar_(Y/N)"));
                    }
                    
                }
            }
            break;
        case "Vais Buscar":
            if(campos.length == 1) {
                break;
            }
            if(campos[1].equals("0")) {
                this.setScreen(loginSucess[this.on]);
                cache.clear();
            }
            else if(campos[1].toLowerCase().equals("y")) {
                cache.clear();
                this.setScreen(loginSucess[this.on]);
            } else if (campos[1].toLowerCase().equals("n")) {
                this.getModel().removeAceite(new Aceite((String)cache.get(0)));
                this.setScreen(loginSucess[this.on]);
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
    }
}
}