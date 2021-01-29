import java.util.InputMismatchException;
import java.util.stream.Collectors;
import java.util.Scanner;
import java.util.List;
import java.util.Arrays;

public class Menu
{
    private List<String> utilizadorChoices;
    private List<String> lojaChoices;
    private List<String> volChoices;
    private List<String> servicoChoices;
    private List<String> menuChoices;
    private int opt;
    
    public Menu(String[] uc,String[] lc,String[] vc,String[] sc,String[] mc){
        this.opt = -1;
        this.menuChoices = Arrays.asList(mc);
        this.volChoices = Arrays.asList(vc);
        this.servicoChoices = Arrays.asList(sc);
        this.lojaChoices = Arrays.asList(lc);
        this.utilizadorChoices = Arrays.asList(uc);
    }
    
    public int getOpt(){
        return this.opt;
    }
    
    private void setOpt(int x){
       this.opt = x; 
    }
    
    public void mMenu(){
        do{
            System.out.println("*** Menu ***");
            showMOptions();
            readOption(this.menuChoices.size());
        }while(this.opt == -1);
    }
    
    public void utiMenu(){
        do{
            System.out.println("*** Menu de Utilizador ***");
            showUtioptions();
            readOption(this.utilizadorChoices.size());
        }while(this.opt == -1);
    }
    
    public void lojMenu(){
        do{
            System.out.println("*** Menu de Loja ***");
            showLojoptions();
            readOption(this.lojaChoices.size());
        }while(this.opt == -1);
    }

    public void volMenu(){
        do{
            System.out.println("*** Menu de Voluntário ***");
            showVoloptions();
            readOption(this.volChoices.size());
        }while(this.opt == -1);
    }

    public void showVoloptions(){
        for(int i = 0; i < this.volChoices.size(); i ++){
            System.out.printf("%d-", i+1);
            System.out.println(this.volChoices.get(i));
        }
        System.out.println("0-Exit");
    }

    //Não faz sentifo chamar Menu serviço mas sim Menu Transportadora
    public void servMenu(){
        do{
            System.out.println("*** Menu de Transportadora ***");
            showServoptions();
            readOption(this.servicoChoices.size());
        }while(this.opt == -1);
    }
    
    public void showServoptions(){
        for(int i = 0; i < this.servicoChoices.size(); i ++){
            System.out.printf("%d-", i+1);
            System.out.println(this.servicoChoices.get(i));
        }
        System.out.println("0-Exit");
    }
    
    public void showLojoptions(){
        for(int i = 0; i < this.lojaChoices.size(); i ++){
            System.out.printf("%d-", i+1);
            System.out.println(this.lojaChoices.get(i));
        }
        System.out.println("0-Exit");
    }
    
    public void showUtioptions(){
        for(int i = 0; i < this.utilizadorChoices.size(); i ++){
            System.out.printf("%d-", i+1);
            System.out.println(this.utilizadorChoices.get(i));
        }
        System.out.println("0-Exit");
    }
    
    public void showMOptions(){

        for(int i = 0; i < this.menuChoices.size(); i ++){
            System.out.printf("%d-", i+1);
            System.out.println(this.menuChoices.get(i));
        }
        System.out.println("0-Exit");        
    }
    
    public void readOption(int size){

        try{
            Scanner in = new Scanner(System.in);
            System.out.print("Escolha: ");
            this.opt = in.nextInt(); 
        }catch(InputMismatchException e){
            System.out.println(e.getMessage());
        }
        if(this.opt < 0 || this.opt > size) this.opt = -1;
    }
}