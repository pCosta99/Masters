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

    public MenuLogin() {
        super();
        this.email = "";
        this.password = "";
    }

    public void executaParametros() {
        System.out.print("E-mail: ");
        this.email = leString();
        System.out.print("Password: ");
        this.password = leString();
    }

    public void executaReader(){
        showMenu();
        int aux;
        do{
            aux=lerOpcao();
        } while(aux<0 || aux>2);
        this.setOpcao(aux);
    }

    public String leString(){
        String op = null;
        Scanner sc = new Scanner(System.in);
        try {
            op = sc.nextLine();
        }
        catch (InputMismatchException e){
            System.out.println("Não leu string");
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