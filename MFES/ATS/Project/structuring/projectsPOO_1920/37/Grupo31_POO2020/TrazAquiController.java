
// classe controladora com a estrutura de dados
import java.io.*;

public class TrazAquiController implements Serializable {
    // estrutura de dados
    GestaoTotal gt = new GestaoTotal();

    // construtor vazio
    public TrazAquiController() {
    }

    // construtor
    public TrazAquiController(Object o) {
        System.out.println("\f\n\n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                        T R A Z                                                                       \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                          A Q U I                                                     \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "                                                                                                      \n"
                + "  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   \n"
                + "                                                                                                      \n"
                + "       .~.:.~.:.~.:.~.:.~.:.~.:.~.:.~.:.~.:.~.:.<>.:.~.:.~.:.~.:.~.:.~.:.~.:.~.:.~.:.~.:.~.:.~        \n"
                + "                                                                                                      \n");
        try {
            Thread.sleep(1000);
        } catch (Exception e) {
            System.out.println("Erro.");
        }

        System.out.println(
                "\n\n" + "***********************************************************************************\n"
                        + "                                                                                   \n"
                        + "          Bem-Vindo...                                                             \n"
                        + "              TRAZ   AQUI                                                          \n"
                        + "                                                                                   \n"
                        + "                                                                                   \n"
                        + "                                                            o                      \n"
                        + "      .----------------------------------......._____       |                      \n"
                        + "      |______________________________________________`_,    |                      \n"
                        + "      |  .------------------------------.  |.----------,    |                      \n"
                        + "      |  |    Joana Sousa,A83614        |  ||           \\  |                      \n"
                        + "      |  |    João Coutinho,A83545      |  ||  Grupo     \\ |                      \n"
                        + "      |  |    Tiago Gomes,A78141        |  ||        31   \\|_                     \n"
                        + "      |  `------------------------------'  ||              \\/ .---------.         \n"
                        + "      |____________________________________||_______________\\_(_________)_        \n"
                        + "      | .---.                        .---. |                `%,------------~-.     \n"
                        + "      | |(O)|                        |(O)| |  __             |               |     \n"
                        + "     (| `---'                        `---' | (-              |               |)    \n"
                        + "     (|                                    |  ~~             |               |     \n"
                        + "      |                                    |                 |               |     \n"
                        + "      |       __,---,__                    |                `%,  __,---,__   |_    \n"
                        + "     =|______//       \\___________________|_________________|__//       \\__|_]   \n"
                        + "              |   .-.   |                                        |   .-.   |       \n"
                        + "              |   `-'   |                                        |   `-'   |       \n"
                        + "              \\      //                                         \\       //       \n"
                        + "                '---'                                               '---'          \n"
                        + "* ------------------------------------------------------------------------------   \n"
                        + "*****************************************************************************\n");
        // menu
        new Menu(1);
    }

    // apontador da gestão total
    public GestaoTotal getGT(){
        return this.gt;
        
    }

    // set gt
    public void setGT(GestaoEmpresas gemp, GestaoEncomendas ge, GestaoLojas gl, GestaoUtilizadores gu, GestaoVoluntarios gv) {
        this.gt.setGEMP(gemp);
        this.gt.setGT(ge);
        this.gt.setGL(gl);
        this.gt.setGU(gu);
        this.gt.setGV(gv);
    }


    public void carregaEstado() throws FileNotFoundException, IOException, ClassNotFoundException {
        FileInputStream fis = new FileInputStream("TrazAqui.dat");
        ObjectInputStream ois = new ObjectInputStream(fis);

        Menu novo = (Menu) ois.readObject();
        // cria um menu novo e é preciso alterar a gestão total para dar load aos dados
        this.setGT(novo.getGT().getGEMP(), novo.getGT().getGE(), novo.getGT().getGL(), novo.getGT().getGU(),novo.getGT().getGV());
        ois.close();
    }

    public void guardaEstado() throws FileNotFoundException, IOException{
        FileOutputStream fos = new FileOutputStream("TrazAqui.dat");
        
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        
        oos.writeObject(this); 
        oos.flush();
        oos.close();
    }
}