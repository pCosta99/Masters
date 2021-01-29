package src.view;

import src.controller.LoginController;
import src.exceptions.LoginErrorException;
import src.exceptions.ValorInvalidoException;
import src.model.Ponto;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class LoginView {
    private LoginController controller;
    private Scanner sc;

    public LoginView(LoginController c, Scanner sc){
        this.controller = c;
        this.sc = sc;
    }

    public Map<String,Boolean> run(){
        int x = -1;

        while( x != 0){
            System.out.println("Escolha uma opção:");
            System.out.println("1 - Registar Utilizador");
            System.out.println("2 - Registar Loja");
            System.out.println("3 - Registar Transportadora");
            System.out.println("4 - Registar Transportadora Médica");
            System.out.println("5 - Registar Voluntário");
            System.out.println("6 - Registar Voluntário Médico");
            System.out.println("7 - Login Utilizador");
            System.out.println("8 - Login Loja");
            System.out.println("9 - Login Transportadora");
            System.out.println("10 - Login Voluntário");
            System.out.println("11 - Login Transportadora Médica");
            System.out.println("12 - Login Voluntário Médico");
            System.out.println("0 - Sair");
            
            try{
                x = this.sc.nextInt();
                sc.nextLine();

                if(x > 12) System.out.println("Escolha uma opção válida!");

                else if (x > 0 && x < 7) return this.registar(x);
                else if (x >= 7) return this.login(x);
            }

            catch(LoginErrorException e){
                System.out.println("Não foi possível Realizar o login");
                System.out.println(e.getMessage());
            }

            catch(RuntimeException e){
                System.out.println("Não foi possível Realizar o login");
                System.out.println("Input inválido!");
                sc.nextLine();
            }

            catch (ValorInvalidoException v){
                System.out.println("Valor inválido!");
            }

        }
        return new HashMap<>();
    }

    public Map<String,Boolean> registar(int i) throws LoginErrorException, RuntimeException, ValorInvalidoException {
        String email;
        Map<String,Boolean> res = new HashMap<>();
        String password;
        String nome;
        double x,y,precoPorKm,precoPorMin,raio;

            System.out.println("Insira um email:");
            email = sc.nextLine();
            System.out.println("Insira a sua password:");
            password = sc.nextLine();
            System.out.println("Insira o seu nome:");
            nome = sc.nextLine();
            System.out.println("Insira o valor da longitude da sua localização:");
            x = sc.nextDouble();
            System.out.println("Insira o valor da latitude da sua localização:");
            y = sc.nextDouble();
            switch (i){

                case(1):
                    res.put(this.controller.registaUtilizador(email, password, nome, new Ponto(x, y)), false);
                return res;

                case(2):
                    res.put(this.controller.registaLoja(email,password,nome, new Ponto(x,y)),false);
                return res;

                case(3):
                System.out.println("Insira o preço por Km da sua Transportadora:");
                precoPorKm = sc.nextDouble();
                System.out.println("Insira o preço por Min da sua Transportadora:");
                precoPorMin = sc.nextDouble();
                System.out.println("Insira o raio de ação da sua Transportadora:");
                raio = sc.nextDouble();
                res.put(this.controller.registaTransportadora(email,nome,password,new Ponto(x,y),precoPorKm,precoPorMin,raio),false);

                return res;

                case(4):
                System.out.println("Insira o preço por Km da sua Transportadora:");
                precoPorKm = sc.nextDouble();
                System.out.println("Insira o preço por Min da sua Transportadora:");
                precoPorMin = sc.nextDouble();
                System.out.println("Insira o raio de ação da sua Transportadora:");
                raio = sc.nextDouble();
                res.put(this.controller.registaTransportadoraMedica(email,nome,password,new Ponto(x,y),precoPorKm,precoPorMin,raio),true);

                return res;

                case(5):
                System.out.println("Insira o seu raio de ação:");
                raio = sc.nextDouble();
                res.put(this.controller.registaVoluntario(email,password,nome,new Ponto(x,y),raio),false);
                return res;

                case(6):
                System.out.println("Insira o seu raio de ação:");
                raio = sc.nextDouble();
                res.put(this.controller.registaVoluntarioMedico(email,nome,password,new Ponto(x,y),raio),true);
                return res;
            }   
        return res;
    }




    public Map<String,Boolean> login(int i) throws LoginErrorException{
        String email;
        String password;
        Map<String,Boolean> res = new HashMap<>();

        System.out.println("Insira um email:");
        email = sc.nextLine();
        System.out.println("Insira a sua password:");
        password = sc.nextLine();

        String x = this.controller.login(email,password,i);

        res.put(x,this.controller.isMedico(x));

        return res;
    }

}
