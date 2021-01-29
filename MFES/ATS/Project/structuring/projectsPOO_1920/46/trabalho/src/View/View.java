package View;

import Model.*;
import Controler.*;
import javafx.application.Platform;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.List;

public class View implements IView{

    private IControler c;
    private TextField usertxt, txt, txt2, txt3, txt4;
    private PasswordField passwordtxt;
    private ListView<String> listView;
    private int rating = 10;

    public View(IControler c){
        this.c = c;
    }

    public View(){
        c = new Controler();
    }

    @Override
    public void alert(String titulo, String mensagem){
        Stage w = new Stage();
        w.initModality(Modality.APPLICATION_MODAL);
        w.setTitle(titulo);
        w.setMinWidth(300);

        Label label = new Label();
        label.setText(mensagem);
        Button closeButton = new Button("Fechar.");
        closeButton.setOnAction(e -> w.close());

        VBox layout = new VBox(15);
        layout.getChildren().addAll(label, closeButton);
        layout.setAlignment(Pos.CENTER);

        Scene scene = new Scene(layout);
        w.setScene(scene);
        w.showAndWait();
    }

    public int rating(String titulo, String mensagem){
        Stage w = new Stage();
        w.initModality(Modality.APPLICATION_MODAL);
        w.setTitle(titulo);
        w.setMinWidth(300);

        listView= new ListView<>();
        listView.getItems().addAll(
                "1", "2", "3",
                "4", "5", "6", "7",
                "8", "9", "10"
        );
        listView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        Button b1 = new Button("Escolher.");
        b1.setOnAction(e ->{
                rating = pick_rating();
                c.end_scene(e);
        });

        VBox layout = new VBox(5);
        layout.getChildren().addAll(listView,b1);
        layout.setAlignment(Pos.CENTER);

        Scene scene = new Scene(layout);
        w.setScene(scene);
        w.show();
        return rating;
    }

    private int pick_rating(){
        String s = String.valueOf(listView.getSelectionModel().getSelectedItems());
        if(s.equals("[1]"))
            return 1;
        if(s.equals("[2]"))
            return 2;
        if(s.equals("[3]"))
            return 3;
        if(s.equals("[4]"))
            return 4;
        if(s.equals("[5]"))
            return 5;
        if(s.equals("[6]"))
            return 6;
        if(s.equals("[7]"))
            return 7;
        if(s.equals("[8]"))
            return 8;
        if(s.equals("[9]"))
            return 9;
        if(s.equals("[10]"))
            return 10;
        alert("Erro", "É necessário dar um rating.");
        System.out.println("didnt rate");
        return rating("Introduza o rating", "Por favor, avalie a sua satisfação perante a encomenda realizada. [0 muito mau] [10 muito bom]");
    }

    @Override
    public Scene menu() {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        listView= new ListView<>();
        listView.getItems().addAll(
                "Registar Utilizador", "Registar Transportadora", "Registar Voluntário",
                "Registar Loja", "Login Utilizador", "Login Transportadora", "Login Voluntário",
                "Login Loja", "Encomendas Ativas", "TOP Compradores", "TOP Transportadoras"
        );
        listView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);

        Button b1 = new Button("Escolher.");
        b1.setOnAction(e -> escolher_menu());

        Button b2 = new Button("Sair.");
        b2.setOnAction(e -> {
            c.save();
            Platform.exit();
        });

        Button b3 = new Button("Guardar.");
        b3.setOnAction(e -> {
            c.save();
            c.end_scene(e);
            make_window("Menu Principal", menu());
        });
        layout.getChildren().addAll(listView,b1,b2,b3);
        return new Scene(layout, 400, 400);
    }

    @Override
    public Scene registar_user() {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        usertxt = new TextField();
        Label lblUser = new Label("Email de Utilizador");

        passwordtxt = new PasswordField();
        Label lblPassword = new Label("Password");

        txt = new TextField();
        Label lblNome = new Label("Nome");

        Button b = new Button("Registar.");
        b.setOnAction(e -> {
            String user = usertxt.getText();
            String pwd = passwordtxt.getText();
            String nome = txt.getText();

            if(user.equals("")) alert("Email NULL", "Precisa de inserir um email para se registar.");
            if(pwd.equals("")) alert("Password NULL", "Precisa de inserir uma palavra-passe para se registar.");
            if(nome.equals("")) alert("Nome NULL", "Precisa de inserir um nome para se registar.");
            else {
                c.validaRegUser(user, pwd, nome);
                c.end_scene(e);
            }
        });

        layout.getChildren().addAll(lblUser, usertxt, lblPassword, passwordtxt, lblNome, txt, b);
        return new Scene(layout, 500, 400);
    }

    @Override
    // not done
    public Scene registar_transportadora() {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        usertxt = new TextField();
        Label lblUser = new Label("Email de Transportadora");

        passwordtxt = new PasswordField();
        Label lblPassword = new Label("Password");

        txt = new TextField();
        Label lblNome = new Label("Nome");

        txt2 = new TextField();
        Label lblNif = new Label("Nif");

        txt3 = new TextField();
        Label lblRange = new Label("Range");

        txt4 = new TextField();
        Label lblPreco = new Label("Preço/km");

        Button b = new Button("Registar.");
        b.setOnAction(e -> {
            String user = usertxt.getText();
            String pwd = passwordtxt.getText();
            String nome = txt.getText();
            String nif = txt2.getText();
            String range = txt3.getText();
            String precokm = txt4.getText();

            if(user.equals("")) alert("Email NULL", "Precisa de inserir um email para se registar.");
            if(pwd.equals("")) alert("Password NULL", "Precisa de inserir uma palavra-passe para se registar.");
            if(nome.equals("")) alert("Nome NULL", "Precisa de inserir um nome para se registar.");
            if(nif.equals("")) alert("Nif NULL", "Precisa de inserir um nif para se registar.");
            if(range.equals("")) alert("Range NULL", "Precisa de inserir um range para se registar.");
            if(precokm.equals("")) alert("Preço NULL", "Precisa de inserir um preço/km para se registar.");
            else {
                c.validaRegTrans(user, pwd, nome, nif, range, precokm);
                c.end_scene(e);
            }
        });

        layout.getChildren().addAll(lblUser, usertxt, lblPassword, passwordtxt, lblNome, txt, lblNif, txt2, lblRange, txt3, lblPreco, txt4, b);
        return new Scene(layout, 500, 450);
    }

    @Override
    // not done
    public Scene registar_voluntario() {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        usertxt = new TextField();
        Label lblUser = new Label("Email de Voluntário");

        passwordtxt = new PasswordField();
        Label lblPassword = new Label("Password");

        txt = new TextField();
        Label lblNome = new Label("Nome");

        txt3 = new TextField();
        Label lblRange = new Label("Range");

        Button b = new Button("Registar.");
        b.setOnAction(e -> {
            String user = usertxt.getText();
            String pwd = passwordtxt.getText();
            String nome = txt.getText();
            String range = txt3.getText();

            if(user.equals("")) alert("Email NULL", "Precisa de inserir um email para se registar.");
            if(pwd.equals("")) alert("Password NULL", "Precisa de inserir uma palavra-passe para se registar.");
            if(nome.equals("")) alert("Nome NULL", "Precisa de inserir um nome para se registar.");
            if(range.equals("")) alert("Range NULL", "Precisa de inserir um range para se registar.");
            else {
                c.validaRegVol(user, pwd, nome, range);
                c.end_scene(e);
            }
        });

        layout.getChildren().addAll(lblUser, usertxt, lblPassword, passwordtxt, lblNome, txt, lblRange, txt3, b);
        return new Scene(layout, 500, 400);
    }

    @Override
    //not done
    public Scene registar_loja() {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        usertxt = new TextField();
        Label lblUser = new Label("Email de Loja");

        passwordtxt = new PasswordField();
        Label lblPassword = new Label("Password");

        txt = new TextField();
        Label lblNome = new Label("Nome");

        Button b = new Button("Registar.");
        b.setOnAction(e -> {
            String user = usertxt.getText();
            String pwd = passwordtxt.getText();
            String nome = txt.getText();

            if(user.equals("")) alert("Email NULL", "Precisa de inserir um email para se registar.");
            if(pwd.equals("")) alert("Password NULL", "Precisa de inserir uma palavra-passe para se registar.");
            if(nome.equals("")) alert("Nome NULL", "Precisa de inserir um nome para se registar.");
            else {
                c.validaRegLoja(user, pwd, nome);
                c.end_scene(e);
            }
        });

        layout.getChildren().addAll(lblUser, usertxt, lblPassword, passwordtxt, lblNome, txt, b);
        return new Scene(layout, 500, 400);
    }

    @Override
    public Scene login_user() {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        usertxt = new TextField();

        Label lblUser = new Label("Email de Utilizador");

        passwordtxt = new PasswordField();
        Label lblPassword = new Label("Password");

        Button b = new Button("Login.");
        b.setOnAction(e -> {
            String user = usertxt.getText();
            String pwd = passwordtxt.getText();
            c.validaLogInUser(user, pwd);
            c.end_scene(e);
        });

        layout.getChildren().addAll(lblUser, usertxt, lblPassword, passwordtxt, b);
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene login_voluntario() {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        usertxt = new TextField();

        Label lblUser = new Label("Email de Voluntário");

        passwordtxt = new PasswordField();
        Label lblPassword = new Label("Password");

        Button b = new Button("Login.");
        b.setOnAction(e -> {
            String user = usertxt.getText();
            String pwd = passwordtxt.getText();
            c.validaLogInVol(user, pwd);
            c.end_scene(e);
        });

        layout.getChildren().addAll(lblUser, usertxt, lblPassword, passwordtxt, b);
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene login_transportadora() {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        usertxt = new TextField();

        Label lblUser = new Label("Email de Transportadora");

        passwordtxt = new PasswordField();
        Label lblPassword = new Label("Password");

        Button b = new Button("Login.");
        b.setOnAction(e -> {
            String user = usertxt.getText();
            String pwd = passwordtxt.getText();
            c.validaLogInTrans(user, pwd);
            c.end_scene(e);
        });

        layout.getChildren().addAll(lblUser, usertxt, lblPassword, passwordtxt, b);
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene login_loja() {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        usertxt = new TextField();

        Label lblUser = new Label("Email de Loja");

        passwordtxt = new PasswordField();
        Label lblPassword = new Label("Password");

        Button b = new Button("Login.");
        b.setOnAction(e -> {
            String user = usertxt.getText();
            String pwd = passwordtxt.getText();
            c.validaLogInLoja(user, pwd);
            c.end_scene(e);
        });

        layout.getChildren().addAll(lblUser, usertxt, lblPassword, passwordtxt, b);
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene menu_user(IUtilizador u, List<String> lojas, List<String> historico, IEncomenda encomenda) {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        ComboBox<String> cb = new ComboBox<>();
        cb.getItems().addAll(lojas);
        cb.setPromptText("Selecione a Loja da qual pertende encomendar:");
        cb.setOnAction(e -> {
            c.loja_selecionada(u, cb.getValue());
            c.end_scene(e);
        });

        ComboBox<String> cb2 = new ComboBox<>();
        cb2.getItems().addAll(historico);
        cb2.setPromptText("Ver histórico de encomendas:");
        cb2.setOnAction(e -> {
            c.update_user(u);
            c.end_scene(e);
        });

        Button update = new Button("Update");
        update.setOnAction(e -> {
            c.update_user(u);
            c.end_scene(e);
        });

        if(encomenda!=null){
            ComboBox<String> cb3 = new ComboBox<>();
            cb3.getItems().addAll(encomenda.getEstafeta());
            cb3.setPromptText("Lista de Encomendas a precisarem de ser processadas:");
            cb3.setOnAction(e -> {
                String[] split = cb3.getValue().split(" ", 4);
                c.finalizar_encomenda(u, cb3.getValue(), split[1]);
                c.rating(u, split[1], split[1].charAt(0));
                c.update_user(u);
                c.end_scene(e);
            });
            layout.getChildren().addAll(cb, cb2, cb3, update);
        } else layout.getChildren().addAll(cb, cb2, update);
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene menu_transportadora(ITransportadora t, List<String> lojas, List<String> faturacao , Double fat) {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        ComboBox<String> cb = new ComboBox<>();
        cb.getItems().addAll(lojas);
        cb.setPromptText("Selecione a Loja da qual pertende entregar:");
        cb.setOnAction(e -> {
            c.loja_selecionada(t, cb.getValue());
        });

        ComboBox<String> cb2 = new ComboBox<>();
        cb2.getItems().addAll(faturacao);
        cb2.setPromptText("Ver histórico de faturacao:");

        DecimalFormat df = new DecimalFormat("####0.00");

        Label lbl = new Label("Faturação total: " + df.format(fat) + " €");

        Button update = new Button("Update");
        update.setOnAction(e -> {
            c.update_transportadora(t);
            c.end_scene(e);
        });

        layout.getChildren().addAll(cb, cb2, lbl, update);
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene menu_voluntario(IVoluntario v, List<String> lojas, List<String> historico) {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        ComboBox<String> cb = new ComboBox<>();
        cb.getItems().addAll(lojas);
        cb.setPromptText("Selecione a Loja da qual pertende entregar:");
        cb.setOnAction(e -> {
            c.loja_selecionada(v, cb.getValue());
        });

        ComboBox<String> cb2 = new ComboBox<>();
        cb2.getItems().addAll(historico);
        cb2.setPromptText("Ver histórico de Voluntário:");

        Button update = new Button("Update");
        update.setOnAction(e -> {
            c.update_voluntario(v);
            c.end_scene(e);
        });

        layout.getChildren().addAll(cb, cb2, update);
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene menu_loja(ILoja l, List<String> encomendas) {
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        Label lblnumero = new Label("Número de pessoas em fila:");
        Label lbl = new Label(l.fila());

        ComboBox<String> cb = new ComboBox<>();
        cb.getItems().addAll(encomendas);
        cb.setPromptText("Encomendas em Loja:");

        Button update = new Button("Update");
        update.setOnAction(e -> {
            c.update_loja(l);
            c.end_scene(e);
        });

        layout.getChildren().addAll(lblnumero, lbl, cb, update);
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene select_produtos(IUtilizador u, ILoja l, List<String> produtos) {
        VBox layout = new VBox(10);

        Button comprar = new Button("Finalizar encomenda");
        comprar.setOnAction(e -> {
            for (LinhaEncomenda ec : l.getInventario()) {
                if(listView.getSelectionModel().getSelectedItems().contains(ec.getDescricao())){
                    try {
                        c.pedidoUser(ec, l.getId(), u.getId());
                    } catch (IOException ioException) {
                        alert("Erro", "Falhou a realizar uma encomenda");
                    }
                }
            }
            c.update_user(u);
            c.end_scene(e);
        });

        listView = new ListView<>();
        listView.getItems().addAll(produtos);
        listView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);

        layout.getChildren().addAll(listView,comprar);
        layout.setPadding(new Insets(20, 20, 20, 20));
        return new Scene(layout, 400, 300);
    }

    @Override
    public void make_window(String title, Scene s){
        Stage w = new Stage();
        w.setTitle(title);
        w.setScene(s);
        w.show();
    }

    @Override
    public Scene encomendas_ativas(ITransportadora t, List<String> recolhas){
        VBox layout = new VBox(10);

        ComboBox<String> cb = new ComboBox<>();
        cb.getItems().addAll(recolhas);
        cb.setPromptText("Selecione a Loja da qual pertende ir realizar uma recolha:");
        cb.setOnAction(e -> {
            c.pedir_recolha(t, cb.getValue());
            c.end_scene(e);
        });

        layout.getChildren().addAll(cb);
        layout.setPadding(new Insets(20, 20, 20, 20));
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene encomendas_ativas(IVoluntario v, List<String> recolhas){
        VBox layout = new VBox(10);
        ComboBox<String> cb = new ComboBox<>();
        cb.getItems().addAll(recolhas);
        cb.setPromptText("Selecione a Loja da qual pertende ir realizar uma recolha:");
        cb.setOnAction(e -> {
            c.pedir_recolha(v, cb.getValue());
            c.end_scene(e);
        });

        layout.getChildren().addAll(cb);
        layout.setPadding(new Insets(20, 20, 20, 20));
        return new Scene(layout, 400, 300);
    }

    @Override
    public Scene print_list(List<String> encomendas){
        VBox layout = new VBox(10);
        layout.setPadding(new Insets(20, 20, 20, 20));

        listView= new ListView<>();
        listView.getItems().addAll(encomendas);

        layout.getChildren().addAll(listView);
        return new Scene(layout, 600, 500);
    }

    /* Verifica a escolhe feita no menu */
    private void escolher_menu(){
        String s = String.valueOf(listView.getSelectionModel().getSelectedItems());
        if(s.equals("[Registar Utilizador]")){
            Stage w = new Stage();
            w.setTitle("Registo de Utilizador");
            w.setScene(registar_user());
            w.show();
        }

        if(s.equals("[Registar Transportadora]")){
            Stage w = new Stage();
            w.setTitle("Registo de Transportadora");
            w.setScene(registar_transportadora());
            w.show();
        }

        if(s.equals("[Registar Voluntário]")){
            Stage w = new Stage();
            w.setTitle("Registo de Voluntário");
            w.setScene(registar_voluntario());
            w.show();
        }

        if(s.equals("[Registar Loja]")){
            Stage w = new Stage();
            w.setTitle("Registo de Loja");
            w.setScene(registar_loja());
            w.show();
        }

        if(s.equals("[Login Utilizador]")){
            Stage w = new Stage();
            w.setTitle("Login de Utilizador");
            w.setScene(login_user());
            w.show();
        }

        if(s.equals("[Login Transportadora]")){
            Stage w = new Stage();
            w.setTitle("Login de Transportadora");
            w.setScene(login_transportadora());
            w.show();
        }

        if(s.equals("[Login Voluntário]")){
            Stage w = new Stage();
            w.setTitle("Login de Voluntário");
            w.setScene(login_voluntario());
            w.show();
        }

        if(s.equals("[Login Loja]")){
            Stage w = new Stage();
            w.setTitle("Login de Loja");
            w.setScene(login_loja());
            w.show();
        }

        if(s.equals("[Encomendas Ativas]")){
            c.listar_on_going();
        }

        if(s.equals("[TOP Compradores]")){
            c.listar_top_users();
        }

        if(s.equals("[TOP Transportadoras]")){
            c.listar_top_transportadoras();
        }
    }
}
