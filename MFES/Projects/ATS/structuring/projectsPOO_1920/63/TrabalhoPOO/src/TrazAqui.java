import java.util.Scanner;

public class TrazAqui {
    public static void main(String[] args) {
        //inicializa o model
        Model m = new Model();
        Scanner input = new Scanner(System.in);
        String buffer;
        char option;

        System.out.println("Deseja carregar algum ficheiro de logs (txt) (s/n)?");
        System.out.println("Note que se nao carregar, irao ser carregados os dados da ultima utilizacao");

        do {
            buffer = input.nextLine();
            if (buffer.length() > 0) {
                option = buffer.charAt(0);
            } else {
                option = ' ';
            }
            if(option!='n' && option!='s'){
                System.out.println("Valor inválido");
            }
        }while(option!='n' && option!='s');


        if(option == 's') {
            // carrega um ficheiro de logs (estado inicial por omissao)

            System.out.print("Indique o ficheiro de logs a abrir [enter para abrir default]: ");
            String path = input.nextLine();

            Parse p = new Parse();
            if (!(path.length() > 0)) {
                path = "./input/EstadoInicial.txt";
                p.parse(m, path, 0);
            } else {
                p.parse(m, path, 1);
            }
        }else{
            try{
                m = Model.lerLog();
            }catch(Exception e){
                System.out.println(e.getMessage());
            }
        }
        //inicializa a view
        View v = new View();

        // inicializa o control
        Control c = new Control(v, m);
        c.inicio();
        c.gravarLog();
        input.close();
    }
}
