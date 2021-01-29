package view;

import static java.lang.System.out;

import java.io.Serializable;
import java.util.InputMismatchException;
import java.util.Scanner;

public class MenuLogin extends Menu implements Serializable {
    private String email;
    private String password;


    public MenuLogin(String[] opcoes) {
        super(opcoes);
        this.email = "";
        this.password = "";
    }

    public void executaParametros() {
        System.out.println("Pressione 0 para sair");
        System.out.print("E-mail: ");
        this.email = leString();
        System.out.print("Password: ");
        this.password = leString();
        if(this.email.equals("0") || this.password.equals("0") ) this.setOp(0);
    }

    public void executaReader(){
        showMenu();
        int aux;
        do{
            aux=lerOpcao();
        } while(aux<0 || aux>5);
        this.setOp(aux);
    }

    public String leString(){
        String op = null;
        Scanner sc = new Scanner(System.in);
        try {
            op = sc.nextLine();
        }
        catch (InputMismatchException e){
            out.println("Não leu string");
        }
        return op;
    }

    public String getEmail(){
        return this.email;
    }

    public String getPassword(){
        return this.password;
    }
}

